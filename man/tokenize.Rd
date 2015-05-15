\name{tokenize}
\alias{tokenize}
\title{
Tokenization and transliteration of character strings based on an orthography profile
}
\description{
To process strings it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription/transliteration'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile.

This function is the main function to produce, test and apply orthography profiles.
}
\usage{
tokenize(strings, 
  profile = NULL, transliterate = NULL,
  parsing = "global", ordering = c("size", "context"),
  sep = " ", missing = "\u2047", normalize = "NFC",
  literal = TRUE, case.insensitive = FALSE, silent = FALSE,
  file.out = NULL)
}
\arguments{
  \item{strings}{
  Vector of strings to the tokenized.
}
  \item{profile}{
  Orthography profile specifying the graphemes for the tokenization, and possibly any replacements of the available graphemes. Can be a reference to a file or an R object. If NULL then the orthography profile will be created on the fly using the defaults of \code{write.profile}.
}
  \item{transliterate}{
  Default \code{NULL}, meaning no transliteration is to be performed. Alternatively, specify the name of the column in the orthography profile that should be used for replacement.
}
  \item{parsing}{
  Method to be used for parsing the strings into graphemes. Currently two options are implemented: \code{global} and \code{linear}. See Details for further explanation.
}
  \item{ordering}{
  Method for ordering. Currently three different methods are implemented, which can be combined (see Details below): \code{size}, \code{context}, and \code{frequency}. Use \code{NULL} to prevent ordering and use the top to bottom order as specified in the orthography profile.
}
  \item{sep}{
  Separator to be inserted between graphemes. Defaults to space. This function assumes that the separator specified here does not occur in the data. If it does, unexpected things might happen. Consider removing the chosen seperator from your strings first, e.g. by using \code{\link{gsub}}.
}
  \item{missing}{
  Character to be inserted at transliteration when no transliteration is specified. Defaults to DOUBLE QUESTION MARK at U+2047.
}
  \item{normalize}{
  Which normalization to use, defaults to "NFC". Other option is "NFD". Any other input will result in no normalisation being performed.
}
  \item{literal}{
  Logical: should contexts be considered in the tokenization? By default, this function does not assume any contexts to be specified, i.e. match literal. If \code{context = TRUE} then the function assumes that there are columns called \code{left} (for the left context) and \code{right} (for the right context), and optionally a column called \code{class} (for the specification of grapheme-classes) in the orthography profile. These are hard-coded names, so please adapt your orthography profile accordingly. The columns \code{left} and \code{right} allow for regular expression to specify context.

When \code{context = FALSE} internally the matching of graphemes is done exact, i.e. without using regular expressions. When \code{context = TRUE} ICU-style regular expression (see \code{strgi:stringi_search_regex}) are used, so any reserved characters have to be escaped in the orthography profile. Specifically, add a slash "\" before any occurrence of the characters \code{".|()[]{}^$*+?"} in your profile (except of course when these characters are used in their regular expression meaning).
}
  \item{case.insensitive}{
  By default, there is no case matching, so "A" and "a" are different graphemes. The option \code{case.incensitive = TRUE} applies a simple ICU "u_toupper" as made available in the \code{stringi} package. This should give the desired results in most cases, though for special orthographies the case mapping might need to be specified explicitly. In that case, consider writing a profile specifying the case mapping.
}
  \item{silent}{
  Logical: by default missing characters in the strings are reported with a warning. use \code{silent = TRUE} to supress these warnings.
  }
  \item{file.out}{
  Filename for results to be written. No suffix should be specified, as various different files with different suffixes are produced (see Details below).
}
}
\details{
Given a set of graphemes, there are at least two different methods to tokenize strings. The first is called \code{global} here: this approach takes the first grapheme, matches this grapheme globally at all places in the string, and then turns to the next string. The other approach is called \code{linear} here: this approach walks through the string from left to right. At the first character it looks through all graphemes whether there is any match, and then walks further to the end of the match and starts again. In some special cases these two methods can lead to different results (see Examples for an example).

The ordering or the lines in the ortography profile is of crucial importance, and different orderings will lead to radically different results. To simply use the top to bottom ordering as specified in the profile, use \code{order = NULL}. Currently, there are three different ordering strategies implemented: \code{size}, \code{context}, and \code{frequency}. By specifying more than one in a vector, these orderings are used to break ties, e.g. \code{c("size, frequency")} will first order by size, and for those with the same size, it will order by frequency. For lines that are still tied (i.e. the have the same size and frequency) the order will fall back to the reverse order as attested in the profile.

\itemize{
\item \code{size}: order the lines in the profile by the size of the grapheme, largest first. Size is measured by number of Unicode characters after normalization as specified in the option \code{normalize}. For example, \code{é} has a size of 1 with \code{normalize = "NFC"}, but a size of 2 with \code{normalize = "NFD"}.

\item \code{context:} order the lines by whether they have any context specified, lines with context coming first. Note that this only works when the option \code{context = TRUE} is also chosen.

\item \code{frequency}: order the lines by the frequency with which they match in the specified strings before tokenization, least frequent coming first. This frequency of course depends crucially on the available strings, so it will lead to different orderings when applied to different data. Also note that this frequency is (necessarily) measured before graphemes are identified, so these ordering frequencies are not the same as the final frequencies shown in the outpur. Frequency of course also strongly differs on whether context is used for the matching through \code{context = TRUE}.
}
}
\value{
Without specificatino of \code{file.out}, the function \code{tokenize} will return a list of three:
\item{strings}{a dataframe with the original and the tokenized/transliterated strings}
\item{profile}{a dataframe with the graphemes with added frequencies. The dataframe is ordered according to the order that resulted from the specifications in \code{ordering}.}
\item{errors}{a dataframe with all original strings that contain unmatched parts.}
\item{missing}{a dataframe with the graphemes that are missing from the original orthography profilr, as indicated in the errors. Note that the report of missing characters does currently not lead to correct results for transliterated strings.}

When \code{file} is specified, these three tables will be written to three different tab-separated files (with header lines): \code{file_strings.tsv} for the strings, \code{file_profile.tsv} for the orthrography profile, \code{file_errors.tsv} for the strings that have unidentifyable parts, and \code{file_missing.tsv} for the graphemes that seem to be missing. When there is nothing missing, then no file for the missing strings is produced.
}
\references{
Moran & Cysouw (forthcoming)
}
\note{
Note that overlapping matching does not (yet) work with regex matching when \code{context = TRUE}. That means that for example "aa" is only found once in "aaa". In some special cases this might lead to problems that might have to be explicitly specified in the profile, e.g. a grapheme "aa" with a left context "a". See examples below.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
See also \code{\link{write.profile}} for preparing a skeleton orthography profile.
}
\examples{
% \dontrun{
% # gives error in R CMD check, because of cyrillic characters 
% # simple example with interesting warning and error reporting
% tokenize("AАBВ",c("A","B")
% }
# make an ad-hoc orthography profile
profile <- cbind(
    graphemes = c("a","ä","n","ng","ch","sch"), 
    trans = c("a","e","n","N","x","sh"))
# tokenization
tokenize(c("nana", "änngschä", "ach"), profile)
# with replacements and an error message
tokenize(c("Naná", "änngschä", "ach"), profile, transliterate = "trans")

# different results of ordering
tokenize("aaa", c("a","aa"), order = NULL)
tokenize("aaa", c("a","aa"), order = "size")

\dontrun{
# regexmatching does not catch overlap in some special cases
# this results in a warning instead of just parsing "ab bb" in the following example
profile <- write.profile("ab bb", info = FALSE, editing = TRUE, sep = " ")
tokenize("abbb", profile , context = TRUE, order = NULL)
}

# different parsing methods can lead to different results
tokenize("abc", c("bc","ab","a","c"), order = NULL, parsing = "global")$strings
tokenize("abc", c("bc","ab","a","c"), order = NULL, parsing = "linear")$strings
}

