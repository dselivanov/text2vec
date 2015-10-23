// header from digest package
#include <pmurhashAPI.h>

using namespace std;

// borrowed from FeatureHashing package
// https://github.com/wush978/FeatureHashing/blob/ea7cc034b0dbdfa6fc742fcebaa7dc4025f5364e/src/digest.c
// https://github.com/wush978/FeatureHashing/issues/96

// seeds for hashing trick
const uint32_t MURMURHASH3_HASH_SEED = 3120602769LL;
const uint32_t MURMURHASH3_SIGN_SEED = 79193439LL;

// feature hash
uint32_t murmurhash3_hash (const string &str) {
  return PMurHash32(MURMURHASH3_HASH_SEED, str.c_str(), str.size());
}

// feature sign hash
int murmurhash3_sign (const string &str) {
  return (int)PMurHash32(MURMURHASH3_SIGN_SEED, str.c_str(), str.size());
}
