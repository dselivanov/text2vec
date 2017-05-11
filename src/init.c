#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP text2vec_collapse_collocations_cpp(SEXP, SEXP, SEXP);
extern SEXP text2vec_colMaxs(SEXP);
extern SEXP text2vec_colMins(SEXP);
extern SEXP text2vec_cpp_get_document_count(SEXP);
extern SEXP text2vec_cpp_get_vocab_statistics(SEXP);
extern SEXP text2vec_cpp_glove_create(SEXP);
extern SEXP text2vec_cpp_glove_dump_model(SEXP);
extern SEXP text2vec_cpp_glove_get_sparsity_level(SEXP);
extern SEXP text2vec_cpp_glove_get_word_vectors(SEXP);
extern SEXP text2vec_cpp_glove_partial_fit(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP text2vec_cpp_glove_set_cost_zero(SEXP);
extern SEXP text2vec_cpp_hash_corpus_create(SEXP, SEXP, SEXP, SEXP);
extern SEXP text2vec_cpp_hash_corpus_get_dtm(SEXP);
extern SEXP text2vec_cpp_hash_corpus_get_tcm(SEXP);
extern SEXP text2vec_cpp_hash_corpus_insert_document_batch(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP text2vec_cpp_vocab_create(SEXP, SEXP, SEXP, SEXP);
extern SEXP text2vec_cpp_vocabulary_corpus_create(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP text2vec_cpp_vocabulary_corpus_get_dtm(SEXP);
extern SEXP text2vec_cpp_vocabulary_corpus_get_tcm(SEXP);
extern SEXP text2vec_cpp_vocabulary_corpus_insert_document_batch(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP text2vec_cpp_vocabulary_insert_document_batch(SEXP, SEXP);
extern SEXP text2vec_create_xptr_unordered_set(SEXP);
extern SEXP text2vec_euclidean_dist(SEXP, SEXP);
extern SEXP text2vec_hasher(SEXP, SEXP);
extern SEXP text2vec_is_invalid_ptr(SEXP);
extern SEXP text2vec_rowMaxs(SEXP);
extern SEXP text2vec_rowMins(SEXP);
extern SEXP text2vec_run_one_iter_doc(SEXP, SEXP);
extern SEXP text2vec_run_one_iter_word(SEXP, SEXP);
extern SEXP text2vec_warplda_create(SEXP, SEXP, SEXP);
extern SEXP text2vec_warplda_get_c_global(SEXP);
extern SEXP text2vec_warplda_get_doc_topic_count(SEXP);
extern SEXP text2vec_warplda_get_local_diff(SEXP);
extern SEXP text2vec_warplda_get_topic_word_count(SEXP);
extern SEXP text2vec_warplda_init_dtm(SEXP, SEXP, SEXP, SEXP);
extern SEXP text2vec_warplda_pseudo_loglikelihood(SEXP);
extern SEXP text2vec_warplda_reset_local_diff(SEXP);
extern SEXP text2vec_warplda_set_c_global(SEXP, SEXP);
extern SEXP text2vec_warplda_set_topic_word_count(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"text2vec_collapse_collocations_cpp",                   (DL_FUNC) &text2vec_collapse_collocations_cpp,                   3},
    {"text2vec_colMaxs",                                     (DL_FUNC) &text2vec_colMaxs,                                     1},
    {"text2vec_colMins",                                     (DL_FUNC) &text2vec_colMins,                                     1},
    {"text2vec_cpp_get_document_count",                      (DL_FUNC) &text2vec_cpp_get_document_count,                      1},
    {"text2vec_cpp_get_vocab_statistics",                    (DL_FUNC) &text2vec_cpp_get_vocab_statistics,                    1},
    {"text2vec_cpp_glove_create",                            (DL_FUNC) &text2vec_cpp_glove_create,                            1},
    {"text2vec_cpp_glove_dump_model",                        (DL_FUNC) &text2vec_cpp_glove_dump_model,                        1},
    {"text2vec_cpp_glove_get_sparsity_level",                (DL_FUNC) &text2vec_cpp_glove_get_sparsity_level,                1},
    {"text2vec_cpp_glove_get_word_vectors",                  (DL_FUNC) &text2vec_cpp_glove_get_word_vectors,                  1},
    {"text2vec_cpp_glove_partial_fit",                       (DL_FUNC) &text2vec_cpp_glove_partial_fit,                       5},
    {"text2vec_cpp_glove_set_cost_zero",                     (DL_FUNC) &text2vec_cpp_glove_set_cost_zero,                     1},
    {"text2vec_cpp_hash_corpus_create",                      (DL_FUNC) &text2vec_cpp_hash_corpus_create,                      4},
    {"text2vec_cpp_hash_corpus_get_dtm",                     (DL_FUNC) &text2vec_cpp_hash_corpus_get_dtm,                     1},
    {"text2vec_cpp_hash_corpus_get_tcm",                     (DL_FUNC) &text2vec_cpp_hash_corpus_get_tcm,                     1},
    {"text2vec_cpp_hash_corpus_insert_document_batch",       (DL_FUNC) &text2vec_cpp_hash_corpus_insert_document_batch,       6},
    {"text2vec_cpp_vocab_create",                            (DL_FUNC) &text2vec_cpp_vocab_create,                            4},
    {"text2vec_cpp_vocabulary_corpus_create",                (DL_FUNC) &text2vec_cpp_vocabulary_corpus_create,                5},
    {"text2vec_cpp_vocabulary_corpus_get_dtm",               (DL_FUNC) &text2vec_cpp_vocabulary_corpus_get_dtm,               1},
    {"text2vec_cpp_vocabulary_corpus_get_tcm",               (DL_FUNC) &text2vec_cpp_vocabulary_corpus_get_tcm,               1},
    {"text2vec_cpp_vocabulary_corpus_insert_document_batch", (DL_FUNC) &text2vec_cpp_vocabulary_corpus_insert_document_batch, 6},
    {"text2vec_cpp_vocabulary_insert_document_batch",        (DL_FUNC) &text2vec_cpp_vocabulary_insert_document_batch,        2},
    {"text2vec_create_xptr_unordered_set",                   (DL_FUNC) &text2vec_create_xptr_unordered_set,                   1},
    {"text2vec_euclidean_dist",                              (DL_FUNC) &text2vec_euclidean_dist,                              2},
    {"text2vec_hasher",                                      (DL_FUNC) &text2vec_hasher,                                      2},
    {"text2vec_is_invalid_ptr",                              (DL_FUNC) &text2vec_is_invalid_ptr,                              1},
    {"text2vec_rowMaxs",                                     (DL_FUNC) &text2vec_rowMaxs,                                     1},
    {"text2vec_rowMins",                                     (DL_FUNC) &text2vec_rowMins,                                     1},
    {"text2vec_run_one_iter_doc",                            (DL_FUNC) &text2vec_run_one_iter_doc,                            2},
    {"text2vec_run_one_iter_word",                           (DL_FUNC) &text2vec_run_one_iter_word,                           2},
    {"text2vec_warplda_create",                              (DL_FUNC) &text2vec_warplda_create,                              3},
    {"text2vec_warplda_get_c_global",                        (DL_FUNC) &text2vec_warplda_get_c_global,                        1},
    {"text2vec_warplda_get_doc_topic_count",                 (DL_FUNC) &text2vec_warplda_get_doc_topic_count,                 1},
    {"text2vec_warplda_get_local_diff",                      (DL_FUNC) &text2vec_warplda_get_local_diff,                      1},
    {"text2vec_warplda_get_topic_word_count",                (DL_FUNC) &text2vec_warplda_get_topic_word_count,                1},
    {"text2vec_warplda_init_dtm",                            (DL_FUNC) &text2vec_warplda_init_dtm,                            4},
    {"text2vec_warplda_pseudo_loglikelihood",                (DL_FUNC) &text2vec_warplda_pseudo_loglikelihood,                1},
    {"text2vec_warplda_reset_local_diff",                    (DL_FUNC) &text2vec_warplda_reset_local_diff,                    1},
    {"text2vec_warplda_set_c_global",                        (DL_FUNC) &text2vec_warplda_set_c_global,                        2},
    {"text2vec_warplda_set_topic_word_count",                (DL_FUNC) &text2vec_warplda_set_topic_word_count,                2},
    {NULL, NULL, 0}
};

void R_init_text2vec(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
