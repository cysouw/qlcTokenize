context("Rules (old version")

# help functions
outstrings <- function(output) {
  as.character(output$strings$tokenized)
}

out1 <- tokenize_old(c("ane","ant"), orthography.profile = "n-test")
out2 <- tokenize_old("abcd", orthography.profile = "abcd-test")

# testing rules
test_that("Application of rules", {
  expect_equal(outstrings(out1), c("a n e", "an t"))
  expect_equal(outstrings(out2), c("ab cd"))
  expect_warning(tokenize("bbcd", orthography.profile = "abcd-test"))
})
