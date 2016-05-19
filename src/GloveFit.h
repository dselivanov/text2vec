#include "text2vec.h"
using namespace RcppParallel;
using namespace Rcpp;
using namespace std;

class GloveFit {
  public:
  GloveFit(size_t vocab_size,
           size_t word_vec_size,
           float learning_rate,
           int x_max,
           float max_cost,
           float alpha):
  vocab_size(vocab_size), word_vec_size(word_vec_size),
  x_max(x_max), learning_rate(learning_rate), max_cost(max_cost), alpha(alpha) {

    w_i.resize(vocab_size);
    grad_sq_w_i.resize(vocab_size);

    for(size_t i = 0; i < vocab_size; i++) {
      w_i[i].resize(word_vec_size);
      grad_sq_w_i[i].resize(word_vec_size);
    }
    fill_mat_rand(w_i, word_vec_size, -0.5, 0.5);
    fill_mat_val(grad_sq_w_i, word_vec_size, 1.0);

    b_i.resize(vocab_size);
    grad_sq_b_i.resize(vocab_size);
    fill_vec_rand(b_i, -0.5, 0.5);
    fill_vec_val(grad_sq_b_i, 1.0);
  };

  static inline int is_odd(size_t ind) { return ind & 1; }

  inline float weighting_fun(float x, float x_max, float alpha) {
    if(x < x_max)
      return pow(x / x_max, alpha);
    else
      return 1.0;
  }

  inline float partial_fit( size_t begin,
                                 size_t end,
                                 const RVector<int> &x_irow,
                                 const RVector<int> &x_icol,
                                 const RVector<double> &x_val,
                                 const RVector<int> &iter_order) {

    float global_cost = 0.0, weight, cost_inner, cost;
    float grad_b_i, grad_b_j, grad_k_i, grad_k_j;
    size_t x_irow_i, x_icol_i, i_iter_order;

    int flag_do_shuffle = 0;

    if ( iter_order.length() == x_irow.length() )
      flag_do_shuffle = 1;

    for (size_t i = begin; i < end; i++) {

      if ( flag_do_shuffle )
        // IMPORTANT NOTE
        // subtract 1 here, because we expecte 1-based indices from R
        // sample.int() returns 1-based shuffling indices
        i_iter_order = iter_order [ i ] - 1;
      else
        i_iter_order = i;
      // we assume input matrix initially is **symmetrical and upper-triangular**
      // partial_fit will be called 2 times - on this upper-triangular matrix and on transposed one.
      // So if we want to iterate with random order we will swap indices to
      // emulate upper-diagonal and lower-diagonal elements
      if ( is_odd( i ) ) {
        x_irow_i = x_irow[ i_iter_order ];
        x_icol_i = x_icol[ i_iter_order ];
      } else {
        x_irow_i = x_icol[ i_iter_order ];
        x_icol_i = x_irow[ i_iter_order ];
      }

      weight = weighting_fun(x_val[ i_iter_order ], x_max, this->alpha);

      cost_inner = inner_product(w_i[ x_irow_i ].begin(), w_i[ x_irow_i ].end() ,
                                        w_i[ x_icol_i].begin(),
                                        // init with (b_i + b_j - log(x_ij))
                                        b_i[ x_irow_i ] + b_i[ x_icol_i ] - log( x_val[ i_iter_order ] ) );
      //clip cost for numerical stability
      if (cost_inner > this->max_cost)
        cost_inner = max_cost;
      else if (cost_inner < -(this->max_cost))
        cost_inner = -max_cost;

      cost = weight * cost_inner;

      // add cost^2
      global_cost += cost * cost_inner;

      //Compute gradients for bias terms
      // main
      grad_b_i = cost;
      // context
      grad_b_j = cost;

      // Compute gradients for word vector terms.
      for (uint32_t k = 0; k < word_vec_size; k++) {

        grad_k_i = cost * w_i[ x_icol_i ][ k ];
        grad_k_j = cost * w_i[ x_irow_i ][ k ];

        // Perform adaptive updates for word vectors
        // main
        w_i[ x_irow_i ][ k ] -= (learning_rate * grad_k_i / sqrt( grad_sq_w_i[ x_irow_i ][ k ] ) );
        // context
        w_i[ x_icol_i ][ k ] -= (learning_rate * grad_k_j / sqrt( grad_sq_w_i[ x_icol_i ][ k ] ) );

        // Update squared gradient sums for word vectors
        // main
        grad_sq_w_i[ x_irow_i ][ k ] += grad_k_i * grad_k_i;
        // context
        grad_sq_w_i[ x_icol_i ][ k ] += grad_k_j * grad_k_j;
      }

      // Perform adaptive updates for bias terms
      // main
      b_i[ x_irow_i ] -= (learning_rate * grad_b_i / sqrt( grad_sq_b_i[ x_irow_i ] ));
      // context
      b_i[ x_icol_i ] -= (learning_rate * grad_b_j / sqrt( grad_sq_b_i[ x_icol_i ] ));

      // Update squared gradient sums for biases
      // main
      grad_sq_b_i[ x_irow_i ] += grad_b_i * grad_b_i;
      // context
      grad_sq_b_i[ x_icol_i ] += grad_b_j * grad_b_j;
    }
    return 0.5 * global_cost;
  }

  NumericMatrix get_word_vectors() {
    return convert2Rmat(this->w_i, this->word_vec_size);
  }

  List get_fit() {
    return List::create(_["w_i"] = convert2Rmat(this->w_i, this->word_vec_size),
                        _["grad_sq_w_i"] = convert2Rmat(this->grad_sq_w_i, this->word_vec_size),
                        _["b_i"] = this->b_i,
                        _["grad_sq_b_i"] = this->grad_sq_b_i);
  }
  private:
    size_t vocab_size, word_vec_size;
    float x_max, learning_rate;
    // see https://github.com/maciejkula/glove-python/pull/9#issuecomment-68058795
    // clips the cost for numerical stability
    float max_cost;
    // initial learning rate
    float alpha;
    // word vecrtors
    //vector<vector<float> > w_i, w_j;
    vector<vector<float> > w_i;
    // word biases
    //vector<float> b_i, b_j;
    vector<float> b_i;
    // word vectors square gradients
    //vector<vector<float> > grad_sq_w_i, grad_sq_w_j;
    vector<vector<float> > grad_sq_w_i;
    // word biases square gradients
    //vector<float> grad_sq_b_i, grad_sq_b_j;
    vector<float> grad_sq_b_i;
};
