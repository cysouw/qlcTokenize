\name{tokenize}
\alias{tokenise}

\title{
Tokenization of character strings based on an orthography profile
}
\description{
To process strings it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile.

This function is the main function to produce, test and apply orthography profiles.
}
\usage{
tokenize(strings, orthography.profile = NULL, replace = FALSE,
  graphemes = "graphemes", patterns = "patterns", replacements = NULL,
  sep = "\u00B7", normalize = "NFC", 
  traditional.output = TRUE, file = NULL)
}
\arguments{
  \item{strings}{
  vector of strings to the tokenized.
  }
  \item{orthography.profile}{
  orthography profile specifying the graphemes for the tokenization, and possibly any replacements of the available graphemes. Can be a filename or an object as returned by \code{read.orthography.profile}. If NULL then the orthography profile will be created on the fly using the defaults of \code{write.orthography.profile}.
  }
  \item{replace}{
  logical: should orthographic replacement be performed after tokenization. Defaults to FALSE.
  }
  \item{graphemes}{
  name (or number) of the column in the orthography profile listing the graphemes to be used for the tokenization.
  }
  \item{patterns}{
  name (or number) of the column in the orthography profile listing the patterns to be used for the regular expressions that can optionally be used after base-tokenisation.
  }
  \item{replacements}{
  name (or number) of the column in the orthography profile listing the replacements.
  }
  \item{sep}{
  separator to be inserted between graphemes. Defaults to U+00B7 ("MIDDLE DOT"). In the default setting, the usage of this separator remains invisible to the user. However, when \code{traditional.output = FALSE}, then this separator becomes visible in the output.
  }
  \item{normalize}{
  which normalization to use, defaults to "NFC". Other option is "NFD". Any other input will result in no normalisation being performed.
  }
  \item{traditional.output}{
  logical, defaults to TRUE. When TRUE, then the tokenized strings are presented with spaces between graphemes and space-hash-space (\code{" # "}) instead of spaces between words in the original strings. When FALSE, spaces in the original strings are treated as regular characters, and the separator (see above) is used as character to mark the tokenization.
  }
  \item{file}{
  filename for results to be written. No suffix should be specified, as various different files with different suffixes are produced.
  }
} 
\details{
The \code{tokenize} function will tokenize (and possibly replace, i.e. transliterate) strings into graphemes. First, the graphemes from the \code{.prf} table will used for the tokenization , starting from the largest graphemes (in unicode-code-point count using NFC normalisation by default). Any unmatched sequences in the data will be reported with a warning. Any rules specified in the \code{.rules} file will be applied at the end.
}
\value{
Without specificatino of \code{file}, the function \code{tokenize} will return a list of three:
\item{strings}{the vector with the parsed strings}
\item{orthography.profile}{}
\item{warnings}{a table with all original strings and the unmatched parts}

When \code{file} is specified, these three tables will be written to three different files, \code{file.txt} for the strings, \code{file.prf} for the orthrography profile, and \code{file_warnings.txt} for the warnings. Note that when replacements are made (i.e. when \code{replace = TRUE}), then no orthography profile is produced. Likewise, when there are not warnings, then no file with warning is produced.
}
\author{
Michael Cysouw
}
\examples{
# make an ad-hoc orthography profile
gr <- cbind(c("a","ä","n","ng","ch","sch"),c("a","e","n","N","x","sh"))
colnames(gr) <- c("graphemes","replacements")
( op <- list(graphs = gr, rules = NULL) )

# tokenization
tokenize(c("nana", "änngschä", "ach"), op, graphemes = "graphemes")
# with replacements and an error message
tokenize(c("Naná", "änngschä", "ach"), op, graphemes = "graphemes", replacements = "replacements")
}
