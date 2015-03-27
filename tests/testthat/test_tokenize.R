context("Tokenization")

# help functions
outstrings <- function(output) {
  as.character(output$strings$tokenized)
}
outgraphemes <- function(output) {
  as.character(output$profile$graphemes)
}

# basic structure of tokenize function
example <- c("this is", "a test")
out <- tokenize(example)

test_that("output structure", {
  expect_equal(length(out), 3)
  expect_equal(names(out)[1], "strings")
  expect_equal(names(out)[2], "profile")
  expect_equal(names(out)[3], "missing")
})

test_that("output", {
  expect_equal(outstrings(out), c("t h i s # i s", "a # t e s t"))
  expect_equal(outgraphemes(out), c(" ","a", "e", "h", "i", "s", "t"))
  expect_equal(out$missing, NULL)
})

# changing profile as R object and using such a profile
profile <- as.data.frame(rbind(
  as.matrix(out$profile)
  , c("th","","","","")))

out2 <- tokenize(example, orthography.profile = profile)

test_that("using profile", {
  expect_equal(outstrings(out2), c("th i s # i s", "a # t e s t"))
  expect_equal(outgraphemes(out2), c(" ","a", "e", "i", "s", "t", "th"))
  expect_equal(out2$missing, NULL)
})

test_that("getting errors on missing characters", {
  expect_warning(tokenize("thing", orthography.profile = profile))
})

# check different methods of tokenization
profile <- as.data.frame(rbind(
  as.matrix(out2$profile)
  , c("his","","","","")))

out_global <- tokenize(example, orthography.profile = profile, global.match = TRUE)
out_linear <- tokenize(example, orthography.profile = profile, global.match = FALSE)

test_that("different methods of tokenization", {
  expect_equal(outstrings(out_global), c("t his # i s", "a # t e s t"))
  expect_equal(outstrings(out_linear), c("th i s # i s", "a # t e s t"))
})


