\name{write.profile}
\alias{write.profile}
\title{
Writing of a orthography profile skeleton
}
\description{
To process strings, it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile. 

Provided here is a function to prepare a skeleton for an orthography profile. This function takes some strings and lists detailed information on the Unicode characters in the strings.
}
\usage{
write.profile(strings, 
    normalize = NULL, info = TRUE, editing = FALSE, sep = NULL, 
    file.out = NULL, collation.locale = NULL)
}
\arguments{
  \item{strings}{
  A vector of strings on which to base the orthography profile
}
  \item{normalize}{
  Should any unicode normalization be applied before making a profile? By default, no normalization is applied, giving direct feedback on the actual encoding as observed in the strings. Other options are \code{NFC} and \code{NFD}. In combination with \code{sep} these options can lead to different insights into the structure of your strings (see examples below).
}
  \item{info}{
  Add columns with Unicode information on the graphemes: Unicode code points, Unicode names, and frequency of occurrence in the input strings.
}
  \item{editing}{
  Add empty columns for further editing of the orthography profile: left context, right context, class, and translitation. See \code{\link{tokenize}} for detailed information on their usage.
}
  \item{sep}{
  separator to separate the strings. When NULL (by default), then unicode character definitions are used to split (as provided by UCI, ported to R by \code{stringi::stri_split_boundaries}. When \code{sep} is specified, strings are split by this separator. Often useful is \code{sep = ""} to split by unicode codepoints (see examples below).
}
  \item{file.out}{
  Filename for writing the profile to disk. When \code{NULL} the profile is returned as an R dataframe consisting of strings. When \code{file.out} is specified (as a path to a file), then the profile is written to disk and the R dataframe is returned invisibly.
}
  \item{collation.locale}{
  Specify to ordering to be used in writing the profile. By default it uses the ordering as specified in the current locale (check \code{Sys.getlocale("LC_COLLATE")}).
  }
}
\details{
String are devided into default grapheme clusters as defined by the Unicode specification. Underlying code is due to the UCI as ported to R in the \code{stringi} package.
}
\value{
A dataframe with strings representing a skeleton of an orthography profile.
}
\references{
Moran & Cysouw (forthcoming)
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
\code{\link{tokenize}}
}
\examples{
# produce statistics, showing two different kinds of "A"s in Unicode.
# look at the output of "example" in the console to get the point!
(example <- "\u0041\u0391\u0410")
write.profile(example)

# note the differences. Again, look at the example in the console!
(example <- "ÙÚÛ\u0055\u0300\u0055\u0301\u0055\u0302")
# default settings
write.profile(example)
# split according to unicode codepoints
write.profile(example, sep = "")
# after NFC normalization unicode codepoints have changed
write.profile(example, normalize = "NFC", sep = "")
# NFD normalization gives yet another structure of the codepoints
write.profile(example, normalize = "NFD", sep = "")
# note that NFC and NFD normalization are identical under unicode character definitions!
write.profile(example, normalize = "NFD")
write.profile(example, normalize = "NFC")
}
