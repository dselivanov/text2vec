# calculates cosine distance between m_query and m_source matrices
# also user should provide m_source_norm - norm for matrix
cosine <- function(m_query, m_source, m_source_norm) {
  m_query_norm = sqrt(rowSums(m_query ^ 2))
  tcrossprod(m_query, m_source) / outer(m_query_norm, m_source_norm)
}
