context("Profiles (old version)")

# help functions
outstrings <- function(output) {
  as.character(output$strings$tokenized)
}

# testing writing and reading profile files
write.orthography.profile_old("thing", file = "thing.prf")
cat("th\nng\n", file = "thing.prf", append = TRUE)
profile <- read.orthography.profile_old("thing")
out <- tokenize_old("thing",orthography.profile = "thing.prf")

test_that("structure of reading profile from disk", {
  expect_equal(length(profile), 2)
  expect_equal(names(profile)[1], "graphs")
  expect_equal(names(profile)[2], "rules")
})

test_that("using manually changed profile from disk", {
  expect_equal(outstrings(out), "th i ng")
})

file.remove("thing.prf")

test_that("reading profiles", {
  # prf-file with multiple columns
  expect_named(read.orthography.profile_old("n-test.prf"))
  # prf-file with only graphemes
  expect_named(read.orthography.profile_old("abcd_test.prf"))
})