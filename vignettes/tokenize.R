## ----setup, include = FALSE----------------------------------------------
library(qlcTokenize)

## ----, eval = FALSE------------------------------------------------------
#  # install devtools from CRAN
#  install.packages("devtools")
#  # install qlcTokenize from github using devtools
#  devtools::install_github("cysouw/qlcTokenize")
#  # load qlcTokenize package
#  library(qlcTokenize)
#  # access help files of the package
#  help(qlcTokenize)

## ------------------------------------------------------------------------
test <- "hállo hállо"

## ----, eval = FALSE------------------------------------------------------
#  write.orthography.profile(test)

## ----echo=FALSE, results='asis'------------------------------------------
# some example string
knitr::kable(write.orthography.profile(test))

## ------------------------------------------------------------------------
# the differenec between various "o" characters is mostly invisible on screen
"o" == "o"  # these are the same "o" characters, so this statement in true
"o" == "о"  # this is one latin and and cyrillic "o" character, so this statement is false

## ------------------------------------------------------------------------
test <- c("this thing", "is", "a", "vector", "with", "many", "strings")

## ----, eval = FALSE------------------------------------------------------
#  write.orthography.profile(test)

## ----echo=FALSE, results='asis'------------------------------------------
# some example string
knitr::kable(write.orthography.profile(test))

## ------------------------------------------------------------------------
tokenize(test)

## ----, eval = FALSE------------------------------------------------------
#  dir.create("~/Desktop/tokenize")
#  setwd("~/Desktop/tokenize")
#  tokenize(test, file="test")

## ----, echo = FALSE, results='asis'--------------------------------------
tmp <- as.data.frame(rbind(as.matrix(tokenize(test)$o),c("th"," "," "," "," "),c("ng"," "," "," "," ")))
knitr::kable(tmp)

## ----, eval = FALSE------------------------------------------------------
#  tokenize(test, orthography.profile = "test")
#  
#  # with overwriting of the existing profile:
#  # tokenize(test, orthography.profile = "test", file = "test")
#  
#  # note that you can abbreviate this in R:
#  # tokenize(test, o = "test", f = "test")

## ----, echo = FALSE------------------------------------------------------
tokenize(test, orthography.profile = tmp)

