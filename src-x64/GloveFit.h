// Copyright (C) 2015 - 2016  Dmitriy Selivanov
// This file is part of text2vec
//
// text2vec is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// text2vec is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with text2vec.  If not, see <http://www.gnu.org/licenses/>.

#include "text2vec.h"

// see https://github.com/maciejkula/glove-python/pull/9#issuecomment-68058795
// clips the cost for numerical stability
#define CLIP_GRADIENT 100
using namespace RcppParallel;
using namespace Rcpp;
using namespace std;

inline int sgn(float val) {
  return (0.0 < val) - (val < 0.0);
}

class GloveFit {
  public:
  GloveFit(size_t vocab_size,
           size_t word_vec_size,
           float learning_rate,
           uint32_t x_max,
           float alpha,
           float lambda,
           NumericMatrix w_i_init,
           NumericMatrix w_j_init,
           NumericVector b_i_init,
           NumericVector b_j_init):
    vocab_size(vocab_size), word_vec_size(word_vec_size),
    x_max(x_max), learning_rate(learning_rate),
    alpha(alpha), lambda(lambda) {

    b_i.resize(vocab_size);
    b_j.resize(vocab_size);
    for(size_t i = 0; i < vocab_size; i++) {
      b_i[i] = b_i_init[i];
      b_j[i] = b_j_init[i];
    }
    grad_sq_b_i.resize(vocab_size);
    grad_sq_b_j.resize(vocab_size);

    w_i.resize(vocab_size);
    w_j.resize(vocab_size);

    grad_sq_w_i.resize(vocab_size);
    grad_sq_w_j.resize(vocab_size);

    for(size_t i = 0; i < vocab_size; i++) {
      w_i[i].resize(word_vec_size);
      w_j[i].resize(word_vec_size);
      for(size_t j = 0; j < word_vec_size; j++) {
        w_i[i][j] = w_i_init(i, j);
        w_j[i][j] = w_j_init(i, j);
      }
      grad_sq_w_i[i].resize(word_vec_size);
      grad_sq_w_j[i].resize(word_vec_size);
    }

    fill_mat_val(grad_sq_w_i, word_vec_size, 1.0);
    fill_mat_val(grad_sq_w_j, word_vec_size, 1.0);

    fill_vec_val(grad_sq_b_i, 1.0);
    fill_vec_val(grad_sq_b_j, 1.0);

    // fit with L1 regilarization
    // so need init additional variables
    if(lambda > 0.0) {
      FLAG_DO_L1_REGURARIZATION = 1;
      time.resize(vocab_size);

      u_w_i.resize(vocab_size);
      for(size_t i = 0; i < vocab_size; i++) {
        u_w_i[i].resize(word_vec_size);
      }
      fill_mat_val(u_w_i, word_vec_size, 0.0);
      fill_vec_val(time, 0.0);
      //u_b_i.resize(vocab_size);
      //fill_vec_val(u_b_i, 0.0);
    } else
    {
      FLAG_DO_L1_REGURARIZATION = 0;
    }
  };

  static inline int is_odd(size_t ind) { return ind & 1; }

  inline float weighting_fun(float x, float x_max, float alpha) {
    if(x < x_max)
      return pow(x / x_max, alpha);
    else
      return 1.0;
  }

  // used only with L1 gegularization, so only check w_i
  float get_word_vectors_sparsity_ratio() {
    size_t zeros_count = 0;

    for(size_t i = 0; i < vocab_size; i++)
      for(size_t j = 0; j < word_vec_size; j++)
        if(w_i[i][j] == 0) zeros_count++;

    return (double)zeros_count / vocab_size / word_vec_size;
  }

  float partial_fit( size_t begin,
                       size_t end,
                       const RVector<int> &x_irow,
                       const RVector<int> &x_icol,
                       const RVector<double> &x_val,
                       const RVector<int> &iter_order) {

    float global_cost = 0.0, weight, cost_inner, cost;
    float grad_b_i, grad_b_j, grad_w_i, grad_w_j, abs_u_w_i_k_t;
    size_t x_irow_i, x_icol_i, i_iter_order;

    int flag_do_shuffle = 0;

    if ( iter_order.length() == x_irow.length() )
      flag_do_shuffle = 1;

    for (size_t i = begin; i < end; i++) {

      if ( flag_do_shuffle )
        // subtract 1 here, because we expecte 1-based indices from R
        // sample.int() returns 1-based shuffling indices
        i_iter_order = iter_order [ i ] - 1;
      else
        i_iter_order = i;

      x_irow_i = x_irow[ i_iter_order ];
      x_icol_i = x_icol[ i_iter_order ];

      weight = weighting_fun(x_val[ i_iter_order ], x_max, this->alpha);
      // when we fit with L1 regularization, we simulteniously grow
      // main and context vectors in w_i matrix.
      if(FLAG_DO_L1_REGURARIZATION) {
        cost_inner = inner_product(w_i[ x_irow_i ].begin(), w_i[ x_irow_i ].end() ,
                                   w_i[ x_icol_i].begin(),
                                   // init with (b_i + b_j - log(x_ij))
                                   b_i[ x_irow_i ] + b_i[ x_icol_i ] - log( x_val[ i_iter_order ] ) );
        //clip cost for numerical stability
        if (cost_inner > CLIP_GRADIENT)
          cost_inner = CLIP_GRADIENT;
        else if (cost_inner < -CLIP_GRADIENT)
          cost_inner = -CLIP_GRADIENT;

        cost = weight * cost_inner;

        // add cost^2
        global_cost += cost * cost_inner;

        //Compute gradients for bias terms
        grad_b_i = cost;
        grad_b_j = cost;

        time[x_icol_i] += 1;
        time[x_irow_i] += 1;
        for (uint32_t k = 0; k < word_vec_size; k++) {

          grad_w_i = cost * w_i[ x_icol_i ][ k ];
          grad_w_j = cost * w_i[ x_irow_i ][ k ];

          // main word
          abs_u_w_i_k_t = abs(u_w_i[x_irow_i][k]) / time[x_irow_i];
          if( abs_u_w_i_k_t  <= lambda) {
            w_i[ x_irow_i ][ k ] = 0;
          } else {
            w_i[ x_irow_i ][ k ] = -sgn(u_w_i[x_irow_i][k]) *
              ((learning_rate * time[x_irow_i]) / sqrt( grad_sq_w_i[ x_irow_i ][ k ] ) ) *
              (abs_u_w_i_k_t - lambda);
          }
          // context word
          abs_u_w_i_k_t = abs(u_w_i[x_icol_i][k]) / time[x_icol_i];
          if( abs_u_w_i_k_t  <= lambda) {
            w_i[ x_icol_i ][ k ] = 0;
          } else {
            w_i[ x_icol_i ][ k ] = -sgn(u_w_i[x_icol_i][k]) *
              ((learning_rate * time[x_icol_i]) / sqrt( grad_sq_w_i[ x_icol_i ][ k ] ) ) *
              (abs_u_w_i_k_t - lambda);
          }
          // Update unnormalized gradient sums for word vectors
          u_w_i[ x_irow_i ][ k ] += grad_w_i;
          u_w_i[ x_icol_i ][ k ] += grad_w_j;
          // Update squared gradient sums for word vectors
          grad_sq_w_i[ x_irow_i ][ k ] += grad_w_i * grad_w_i;
          grad_sq_w_i[ x_icol_i ][ k ] += grad_w_j * grad_w_j;
        }
        // Perform adaptive updates for bias terms
        b_i[ x_irow_i ] -= (learning_rate * grad_b_i / sqrt( grad_sq_b_i[ x_irow_i ] ));
        b_i[ x_icol_i ] -= (learning_rate * grad_b_j / sqrt( grad_sq_b_i[ x_icol_i ] ));

        // Update squared gradient sums for biases
        grad_sq_b_i[ x_irow_i ] += grad_b_i * grad_b_i;
        grad_sq_b_i[ x_icol_i ] += grad_b_j * grad_b_j;
      } else
        // vanilla GloVe
      {
        cost_inner = inner_product(w_i[ x_irow_i ].begin(), w_i[ x_irow_i ].end() ,
                                   w_j[ x_icol_i].begin(),
                                   // init with (b_i + b_j - log(x_ij))
                                   b_i[ x_irow_i ] + b_j[ x_icol_i ] - log( x_val[ i_iter_order ] ) );
        //clip cost for numerical stability
        if (cost_inner > CLIP_GRADIENT)
          cost_inner = CLIP_GRADIENT;
        else if (cost_inner < -CLIP_GRADIENT)
          cost_inner = -CLIP_GRADIENT;

        cost = weight * cost_inner;

        // add cost^2
        global_cost += cost * cost_inner;

        //Compute gradients for bias terms
        grad_b_i = cost;
        grad_b_j = cost;
        for (uint32_t k = 0; k < word_vec_size; k++) {

          grad_w_i = cost * w_j[ x_icol_i ][ k ];
          grad_w_j = cost * w_i[ x_irow_i ][ k ];

          // Perform adaptive updates for word vectors
          // main
          w_i[ x_irow_i ][ k ] -= (learning_rate * grad_w_i / sqrt( grad_sq_w_i[ x_irow_i ][ k ] ) );
          // context
          w_j[ x_icol_i ][ k ] -= (learning_rate * grad_w_j / sqrt( grad_sq_w_j[ x_icol_i ][ k ] ) );

          // Update squared gradient sums for word vectors
          // main
          grad_sq_w_i[ x_irow_i ][ k ] += grad_w_i * grad_w_i;
          // context
          grad_sq_w_j[ x_icol_i ][ k ] += grad_w_j * grad_w_j;
        }
        // Perform adaptive updates for bias terms
        b_i[ x_irow_i ] -= (learning_rate * grad_b_i / sqrt( grad_sq_b_i[ x_irow_i ] ));
        b_j[ x_icol_i ] -= (learning_rate * grad_b_j / sqrt( grad_sq_b_j[ x_icol_i ] ));

        // Update squared gradient sums for biases
        grad_sq_b_i[ x_irow_i ] += grad_b_i * grad_b_i;
        grad_sq_b_j[ x_icol_i ] += grad_b_j * grad_b_j;
      }
    }
    return 0.5 * global_cost;
  }

  NumericMatrix get_word_vectors() {
    NumericMatrix wv(this->w_i.size(), this->word_vec_size);
    for (size_t i = 0; i < this->w_i.size(); i++)
      for (size_t j = 0; j < this->word_vec_size; j++)
        wv(i, j) = this->w_i[i][j];
        // if(this->FLAG_DO_L1_REGURARIZATION)
        //   wv(i, j) = this->w_i[i][j];
        // else
        //   // sum of context and main word vectors
        //   wv(i, j) = this->w_i[i][j] + this->w_j[i][j];
    return wv;
  }

  List dump_model() {
    return List::create(_["w_i"] = convert2Rmat(this->w_i, this->word_vec_size),
                        _["w_j"] = convert2Rmat(this->w_j, this->word_vec_size),
                        _["b_i"] = this->b_i,
                        _["b_j"] = this->b_j);
  }
  private:
    size_t vocab_size, word_vec_size;
    uint32_t x_max;
    float learning_rate;
    // initial learning rate
    float alpha;
    // word vecrtors
    vector<vector<float> > w_i, w_j;
    // word biases
    vector<float> b_i, b_j;
    // word vectors square gradients
    vector<vector<float> > grad_sq_w_i, grad_sq_w_j;
    // word biases square gradients
    vector<float> grad_sq_b_i, grad_sq_b_j;

    // variables used if we will repform fit with L1 regularization
    int FLAG_DO_L1_REGURARIZATION;
    float lambda;
    vector<float> time;
    vector<vector<float>> u_w_i;
    //vector<float> u_b_i;
};
