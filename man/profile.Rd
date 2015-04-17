\name{write.profile}
\alias{write.profile}
\title{
Writing of a orthography profile skeleton
}
\description{
To process strings, it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile. 

A function to establish a skeleton for an orthography profile is provided here. This function takes some strings and lists detailed information on the Unicode characters in the strings.
}
\usage{
write.profile(strings, info = TRUE, editing = FALSE, sep = NULL, file.out = NULL)
}
\arguments{
  \item{strings}{
  A vector of strings on which to base the orthography profile
}
  \item{info}{
  Add columns with Unicode information on the graphemes: Unicode code points, Unicode names, and frequency of occurrence in the input strings.
}
  \item{editing}{
  Add empty columns for further editing of the orthography profile: left context, right context, class, and translitation. See \code{\link{tokenize}} for detailed information on their usage.
}
  \item{sep}{
  separator to separate the strings. When NULL (by default), then unicode character definitions are used to split (as provided by UCI, ported to R by \code{stringi::stri_split_boundaries}. When \code{sep} is specified, strings are split by this separator.
}
  \item{file}{
  Filename for writing the profile to disk. When \code{NULL} the profile is returned as an R dataframe consisting of strings.
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
# unfortunately, this is not possible in Rd-documents, so I had to remove the interesting bits.
example <- "AAA"
write.profile(example)
}
