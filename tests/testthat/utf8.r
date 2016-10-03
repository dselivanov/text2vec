context("test UTF-8")

test_that("Vocabulary with UTF-8 strings", {
  all_text = list(enc2utf8(c("标准间", "太差", "房间", "还", "不如", "星", "的",
                             "而且", "设施", "非常", "陈旧", "建议", "酒店", "把", "老", "的",
                             "标准间", "从", "新", "改善")))
  it = itoken(all_text,ids = 1, progressbar = FALSE)
  vocab = create_vocabulary(it)
  expect_true(all(Encoding(vocab$vocab$terms) == 'UTF-8'))
  expect_equal(setdiff(unique(all_text[[1]]), vocab$vocab$terms), character(0))
  expect_equal(setdiff(vocab$vocab$terms, unique(all_text[[1]])), character(0))
})
