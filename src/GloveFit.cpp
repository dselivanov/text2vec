#include "GloveFit.h"

struct AdaGradIter : public Worker {
  RVector<int> x_irow;
  RVector<int> x_icol;
  RVector<double> x_val;

  GloveFit &fit;

  int vocab_size;
  int word_vec_size;
  int num_iters;
  int x_max;
  double learning_rate;

  // accumulated value
  double global_cost;

  // function to set global_cost = 0 between iterations
  void set_cost_zero() { global_cost = 0; };

  //init function to use with rcpp-modules
  void init(const IntegerVector &x_irowR,
            const IntegerVector &x_icolR,
            const NumericVector &x_valR,
            GloveFit &fit) {
    x_irow = RVector<int>(x_irowR);
    x_icol = RVector<int>(x_icolR);
    x_val  = RVector<double> (x_valR);
    fit = fit;
    global_cost = 0;
  }


  // dummy constructor
  AdaGradIter(GloveFit &fit):
    x_irow(IntegerVector(0)),
    x_icol(IntegerVector(0)),
    x_val(NumericVector(0)),
    fit(fit) {};

  // constructors
  AdaGradIter(const IntegerVector &x_irowR,
              const IntegerVector &x_icolR,
              const NumericVector &x_valR,
              GloveFit &fit):
    x_irow(x_irowR),
    x_icol(x_icolR),
    x_val(x_valR),
    fit(fit),
    global_cost(0) {}

  AdaGradIter(const AdaGradIter& AdaGradIter, Split):
    x_irow(AdaGradIter.x_irow),
    x_icol(AdaGradIter.x_icol),
    x_val(AdaGradIter.x_val),
    fit(AdaGradIter.fit),
    global_cost(0) {}

  // process just the elements of the range
  void operator()(size_t begin, size_t end) {
    global_cost += this->fit.adagrad_iterate(begin, end, x_irow, x_icol, x_val);
  }
  // join my value with that of another global_cost
  void join(const AdaGradIter& rhs) {
    global_cost += rhs.global_cost;
  }
};

class GloveFitter {
public:
  GloveFitter(size_t vocab_size,
            size_t word_vec_size,
            uint32_t x_max,
            double learning_rate,
            uint32_t grain_size):
  GRAIN_SIZE(grain_size),
  gloveFit(vocab_size,  word_vec_size, learning_rate, x_max),
  adaGradIter(gloveFit)
  {}

  double fit_chunk(const IntegerVector x_irow,
                   const IntegerVector  x_icol,
                   const NumericVector x_val) {
    this->adaGradIter.init(x_irow, x_icol, x_val, gloveFit);
    parallelReduce(0, x_irow.size(), adaGradIter, GRAIN_SIZE);
    double res = this->adaGradIter.global_cost;
    return (res);
    // return(0);
  }

  List get_word_vectors() {
    return List::create(_["word_vectors"] = adaGradIter.fit.get_word_vectors());
  }

  void set_cost_zero() {adaGradIter.set_cost_zero();};

private:
  uint32_t GRAIN_SIZE;
  GloveFit gloveFit;
  AdaGradIter adaGradIter;
};

RCPP_MODULE(GloveFitter) {
  class_< GloveFitter >( "GloveFitter" )
  //<vocab_size, word_vec_size, x_max, learning_rate, grain_size>
  .constructor<size_t, size_t, uint32_t, double, uint32_t>()
  .method( "get_word_vectors", &GloveFitter::get_word_vectors, "returns word vectors")
  .method( "set_cost_zero", &GloveFitter::set_cost_zero, "sets cost to zero")
  .method( "fit_chunk", &GloveFitter::fit_chunk, "process TCM data chunk")
  ;
}

// List fit_glove(const IntegerVector x_irow,
//                const IntegerVector x_icol,
//                const NumericVector x_val,
//                size_t vocab_size,
//                size_t word_vec_size,
//                uint32_t x_max,
//                uint32_t num_iters,
//                double learning_rate = 0.05,
//                int verbose = 1,
//                double convergence_threshold = 0.001,
//                uint32_t GRAIN_SIZE = 1e5) {
//
//   double cost_imrovement = convergence_threshold;
//
//   size_t job_size = x_irow.size(), i = 0;
//
//   NumericVector cost_history(num_iters);
//
//   GloveFit gloveFit(vocab_size,  word_vec_size, learning_rate, x_max);
//
//   AdaGradIter adaGradIter(x_irow, x_icol, x_val, gloveFit);
//
//   // main fitting loop
//   while (i < num_iters && cost_imrovement >= convergence_threshold) {
//
//     checkUserInterrupt();
//
//     adaGradIter.set_cost_zero();
//
//     parallelReduce(0, job_size, adaGradIter, GRAIN_SIZE);
//     cost_history[i] = adaGradIter.global_cost / job_size;
//     if(i > 0)
//       cost_imrovement = (cost_history[i - 1] /cost_history[i] ) - 1;
//
//     if(verbose)
//       Rprintf("iter = %d, expected global_cost = %f\n", i + 1, cost_history[i]);
//     i++;
//   }
//   return List::create(_["word_vectors"] = adaGradIter.fit.get_word_vectors(),
//                       _["cost_history"] = cost_history);
// }
