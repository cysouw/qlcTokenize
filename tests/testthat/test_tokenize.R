context("Tokenization")

example1 <- c("this is", "a test")
out <- tokenize(example1)

test_that("output structure", {
  expect_equal(length(out), 3)
  expect_equal(names(out)[1], "strings")
  expect_equal(names(out)[2], "orthography.profile")
  expect_equal(names(out)[3], "warnings")
})

test_that("output", {
  expect_equal(as.character(out$strings$tokenized), c("t h i s # i s", "a # t e s t"))
  expect_equal(as.character(out$orthography.profile$graphemes), c("a", "e", "h", "i", "s", "t"))
  expect_equal(out$warnings, NULL)
})

profile <- as.data.frame(rbind(as.matrix(out$orthography.profile),c("th","","","","")))
out2 <- tokenize(example1, orthography.profile = profile)

test_that("using profile", {
  expect_equal(as.character(out2$strings$tokenized), 
               c("th i s # i s", "a # t e s t"))
  expect_equal(as.character(out2$orthography.profile$graphemes), 
               c("a", "e", "i", "s", "t", "th"))
  expect_equal(out2$warnings, NULL)
})

test_that("getting errors on missing characters", {
  expect_warning(tokenize("thing", orthography.profile = profile), 
                 "There are characters in the data that are not in the orthography profile. Check warnings for a table with all problematic strings")
  
})
