#include "text2vec.h"
// header from digest package
#include <pmurhashAPI.h>

// https://github.com/wush978/FeatureHashing/issues/96

uint32_t murmurhash3_hash (const string &str) {
  return PMurHash32(MURMURHASH3_HASH_SEED, str.c_str(), str.size());
}

int murmurhash3_sign (const string &str) {
  return (int)PMurHash32(MURMURHASH3_SIGN_SEED, str.c_str(), str.size());
}

uint32_t fast_int_hash(uint32_t a) {
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a);
  return a;
}
