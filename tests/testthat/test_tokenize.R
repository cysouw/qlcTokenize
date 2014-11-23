context("Tokenization")

# help functions
outstrings <- function(output) {
  as.character(output$strings$tokenized)
}
outgraphemes <- function(output) {
  as.character(output$orthography.profile$graphemes)
}

# basic structure of tokenize function
example <- c("this is", "a test")
out <- tokenize(example)

test_that("output structure", {
  expect_equal(length(out), 3)
  expect_equal(names(out)[1], "strings")
  expect_equal(names(out)[2], "orthography.profile")
  expect_equal(names(out)[3], "warnings")
})

test_that("output", {
  expect_equal(outstrings(out), c("t h i s # i s", "a # t e s t"))
  expect_equal(outgraphemes(out), c("a", "e", "h", "i", "s", "t"))
  expect_equal(out$warnings, NULL)
})

# changing profile as R object and using such a prfile
profile <- as.data.frame(rbind(as.matrix(out$orthography.profile),c("th","","","","")))
out2 <- tokenize(example, orthography.profile = profile)

test_that("using profile", {
  expect_equal(outstrings(out2), c("th i s # i s", "a # t e s t"))
  expect_equal(outgraphemes(out2), c("a", "e", "i", "s", "t", "th"))
  expect_equal(out2$warnings, NULL)
})

test_that("getting errors on missing characters", {
  expect_warning(tokenize("thing", orthography.profile = profile))
})
