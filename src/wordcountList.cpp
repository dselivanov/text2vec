#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]
std::map<std::string, int> WordCount2(CharacterVector stringVec) {
  std::map<std::string, int> wordCount;
  for (auto it : stringVec) {
    ++wordCount[as<std::string>(it)];
  }
  return(wordCount);
}
//' @export
// [[Rcpp::export]]
List ListWordCount2(List stringList) {
  std::list< std::map<std::string, int> > resInternal;
  std::map<std::string, int> wordCountDocument;
  std::list<int> docIndex;
  std::vector<int> doc;
  int memCnt=0;
  int i = 0;
  for (auto it : stringList) {
    wordCountDocument = WordCount2(as<CharacterVector>(it));
    resInternal.push_back(wordCountDocument);
    memCnt += wordCountDocument.size();
    for (int j = 0; j < wordCountDocument.size(); j++)
      docIndex.push_back(i + 1);
    i++;
  }
  // construct result
  NumericVector word_cnt(memCnt);
  std::vector<std::string> names(memCnt);
  i = 0;
  for (auto itOuter : resInternal) {
    for (auto itInner : itOuter) {
      word_cnt[i] = itInner.second;
      names[i] = itInner.first;
      i++;
    }
  }
  word_cnt.names() = names;
  return List::create(Named("wordCounts") = word_cnt, Named("documentIndex") = docIndex);
}
