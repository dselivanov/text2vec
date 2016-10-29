#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP create_xptr_unordered_set(CharacterVector x) {
  std::unordered_set<std::string>* res = new std::unordered_set<std::string>;
  for (auto it:x) {
    res->insert(as<std::string>(it.get()));
  }
  XPtr<std::unordered_set<std::string> > ptr(res, true);
  return ptr;
}

// [[Rcpp::export]]
ListOf<CharacterVector> collapse_collocations_cpp(const ListOf<const CharacterVector> &docs,
                                                  SEXP xptr_unordered_set,
                                                  const String r_sep) {
  const std::string sep = r_sep;
  size_t input_size = docs.size();
  List res(input_size);
  // std::unordered_set<std::string> collocations;
  Rcpp::XPtr<std::unordered_set<std::string>> collocations(xptr_unordered_set);
  std::unordered_set<std::string>::iterator it_collocations;
  // create map for lookup phrases
  // for (auto it:phrases) {
  //   collocations.insert(as<std::string>(it.get()));
  // }
  // loop through documents
  for (uint32_t i = 0; i < input_size; i++) {
    checkUserInterrupt();

    CharacterVector terms = docs[i];
    size_t tsize = terms.size();

    std::vector<std::string> new_terms(tsize);

    size_t k = 0;
    std::string collocation_candidate;
    std::string collocation_candidate_2;
    std::string w0 = "";
    std::string w2;
    // loop through terms
    if(tsize > 0) {
      uint32_t j = 1;
      std::string w1 = as<std::string>(terms[j - 1]);
      new_terms[k] = w1;
      while(j <= tsize) {
        if(j < tsize) {
          w2 = as<std::string>(terms[j]);
          collocation_candidate = w1 + sep + w2;
          it_collocations = collocations->find(collocation_candidate);
        }
        if(it_collocations != collocations->end() && j < tsize) {
          // found collactaion in our collocations set
          w1 = collocation_candidate;
        } else {
          collocation_candidate_2 = w0 + sep + w1;
          auto it2 = collocations->find(collocation_candidate_2);
          if(it2 != collocations->end()) {
            new_terms[k - 1] = collocation_candidate_2;
          }
          else {
            new_terms[k] = w1;
            w0 = w1;
            k++;
          }
          w1 = w2;
        }
        j++;
      }
    }
    // if we are at last word
    //-----------------------------------
    // collocation_candidate_2 = prev_out_word + sep + w1;
    // auto it2 = collocations.find(collocation_candidate_2);
    // if(it2 != collocations.end()) {
    //   new_terms[k - 1] = collocation_candidate_2;
    // }
    // else {
    //   new_terms[k] = w1;
    //   prev_out_word = w1;
    //   k++;
    // }
    //-----------------------------------
    // create result for current document
    CharacterVector r_new_terms(k);
    for(size_t j = 0; j < k; j++ )
      r_new_terms[j] = new_terms[j];
    //add to list
    res[i] = r_new_terms;
  }
  return(res);
}
