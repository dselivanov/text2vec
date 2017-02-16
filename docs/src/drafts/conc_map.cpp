// #include<Rcpp.h>
// #include<RcppParallel.h>
// #include <stdio.h>
// #include <stdlib.h>
// using namespace std;
// using namespace Rcpp;
// using namespace tbb;
// using namespace RcppParallel;
// vector< vector<const char *>> extract_char_pointers( const ListOf<const CharacterVector> &R_docs) {
//   size_t N = R_docs.size();
//   vector< vector<const char *> > docs(N);
//   for (size_t i = 0; i < N; i++) {
//     CharacterVector v = R_docs[i];
//     size_t K = v.size();
//     vector<const char *> strs(K);
//     for(size_t j = 0; j < K; j++) {
//       // extract pointer to R's string pool
//       strs[j] = v[j].begin();
//     }
//     docs[i] = strs;
//   }
//   return docs;
// }
//
// class hash_func {
// public:
//   // struct hash<std::pair<uint32_t, uint32_t>>
//   // inline uint64_t operator()(const std::pair<uint32_t, uint32_t>& k) const
//   // inline hash(const std::pair<uint32_t, uint32_t>& k)
//   inline uint64_t operator()(const std::pair<const uint32_t, const uint32_t>& k) const
//   {
//     return (uint64_t) k.first << 32 | k.second;
//   }
//   static size_t hash(const std::pair<const uint32_t, const uint32_t>& k)
//   {
//     return (uint64_t) k.first << 32 | k.second;
//   }
//
//   static bool equal( const std::pair<uint32_t, uint32_t>& k1,
//                      const std::pair<uint32_t, uint32_t>& k2)
//   {
//     return k1.first == k2.first && k1.second == k2.second;
//   }
// };
// typedef concurrent_hash_map<const pair<const uint32_t, const uint32_t>, uint32_t, hash_func> StringTable;
// // typedef concurrent_unordered_map<const pair<const uint32_t, const uint32_t>, uint32_t, hash_func> StringTable;
// // typedef concurrent_hash_map<string, uint32_t> StringTable;
// namespace std {
// template <>
// struct hash<std::pair<uint32_t, uint32_t>>
// {
//   inline uint64_t operator()(const std::pair<uint32_t, uint32_t>& k) const
//   {
//     return (uint64_t) k.first << 32 | k.second;
//   }
// };
// }
// // typedef unordered_map<const pair<const uint32_t, const uint32_t>, uint32_t, hash_func> StringTable;
//
// struct ConcMatrix : public Worker
// {
//   // source matrix
//   vector< vector<const char*>> &input;
//   StringTable &output;
//   unordered_map<string, uint32_t> &vocab;
//   typename unordered_map < string, uint32_t > :: const_iterator term_iterator;
//   // unordered_map< pair<uint32_t, uint32_t>, uint32_t >  output;
//   // typename unordered_map < pair<uint32_t, uint32_t>, uint32_t > :: const_iterator term_iterator2;
//   size_t size() {
//     return this->output.size();
//   }
//   // ConcMatrix( vector< vector<const char *>> &input2,
//   //            // StringTable &output2,
//   //            unordered_map<string, uint32_t> &vocab2) :
//   //            input(input2), vocab(vocab2){}
//   // ConcMatrix( ConcMatrix& wc, Split) : input(wc.input), vocab(wc.vocab) {}
//   ConcMatrix( vector< vector<const char *>> &input2,
//               StringTable &output2,
//               unordered_map<string, uint32_t> &vocab2) :
//     input(input2), output(output2), vocab(vocab2){}
//   void operator()(std::size_t begin, std::size_t end)  {
//     vector<const char *> *temp;
//     for (std::size_t i = begin; i < end; i++) {
//       temp = &input[i];
//       for(size_t j = 0; j < (*temp).size(); j++) {
//         StringTable::accessor a;
//         term_iterator = vocab.find((*temp)[j]);
//         if(term_iterator != vocab.end()) {
//           // output[make_pair(i, term_iterator->second)] += 1;
//           // output.insert( make_pair(i, term_iterator->second) );
//           output.insert( a, make_pair(i, term_iterator->second) );
//           a->second += 1;
//           a.release();
//         }
//       }
//     }
//   }
//   // void join(const ConcMatrix& rhs) {
//   //   printf("join\n");
//   //   if(rhs.output.size() > output.size()) {
//   //     for (auto it: output) {
//   //       term_iterator2 = rhs.output.find(it.first);
//   //       if(term_iterator2 != rhs.output.end())
//   //         output[it.first] += term_iterator2->second;
//   //     }
//   //   } else {
//   //     printf("else\n");
//   //     for (auto it: rhs.output)
//   //       output[it.first] += it.second;
//   //   }
//   // }
//
// };
//
// /// ' @export
// /// [[Rcpp::export]]
// int CountOccurrences(const ListOf<const CharacterVector> &R_docs,
//                      const CharacterVector &vocab_R, int grain_size) {
//   unordered_map<string, uint32_t> vocab;
//   uint32_t i = 0;
//   for (auto val:vocab_R) {
//     vocab.insert(make_pair(as< string >(val), i));
//     i++;
//   }
//   // Rprintf("vocab done\n");
//   vector< vector<const char *>> ptr = extract_char_pointers(R_docs);
//   Rprintf("ptr done\n");
//   // return ptr.size();
//   StringTable table;
//   ConcMatrix cm(ptr, table, vocab);
//   parallelFor(0, ptr.size(), cm, grain_size);
//   // ConcMatrix cm(ptr, vocab);
//   // parallelFor(0, ptr.size(), cm, grain_size);
//   // parallelReduce(0, ptr.size(), cm, grain_size);
//   return cm.output.size();
// }
// // StringTable table;
//
// // data("movie_review")
// //   tokens = movie_review$review %>% tolower %>% word_tokenizer
// //   it = itoken(tokens)
// //   v = create_vocabulary(it)
// //
// //   system.time(dtm <- create_dtm(it, vocab_vectorizer(v)))
// //
// // # tokens2 = rep(tokens, 20)
// //   RcppParallel::setThreadOptions(numThreads = 4)
// //     system.time(res <- CountOccurrences(tokens, v$vocab$terms, 1))
// //     res
// //
// //     system.time(dtm <- create_dtm(itoken(tokens2), vocab_vectorizer(v)))
// //
// //     for (i in 1:30) CountOccurrences(tokens, v$vocab$terms, 100)
// //
