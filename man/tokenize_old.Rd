\name{tokenize_old}
\alias{tokenize_old}

\title{
Tokenization of character strings based on an orthography profile
}
\description{
To process strings it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile.

This function is the main function to produce, test and apply orthography profiles.
}
\usage{
tokenize_old(strings, 
  orthography.profile = NULL, transliterate = FALSE,
  graphemes = "graphemes", replacements = "replacements",
  sep = " ", sep.replacement = "#", missing = "\u2047",
  normalize = "NFC", size.order = TRUE, context = FALSE,
  global.match = TRUE, file = NULL)  
}
\arguments{
  \item{strings}{
  vector of strings to the tokenized.
  }
  \item{orthography.profile}{
  orthography profile specifying the graphemes for the tokenization, and possibly any replacements of the available graphemes. Can be a filename or an object as returned by \code{read.orthography.profile}. 
  
  If NULL then the orthography profile will be created on the fly using the defaults of \code{write.orthography.profile}.
  }
  \item{transliterate}{
  logical: should orthographic transliteration be performed after tokenization. Defaults to FALSE.
  }
  \item{graphemes}{
  name (or number) of the column in the orthography profile listing the graphemes to be used for the tokenization. Defaults to "graphemes"
  }
  \item{replacements}{
  name (or number) of the column in the orthography profile listing the replacements. Defaults to "replacements"
  }
  \item{sep}{
  separator to be inserted between graphemes. Defaults to space. 
  }
  \item{sep.replacement}{
  if the specified separator (by default: space) already occurs in the data, what should it be replaced with (by default: hash) ?
  }
  \item{missing}{
  character to be inserted at transliteration when no transliteration is specified. Defaults to DOUBLE QUESTION MARK at U+2047.
  }
  \item{normalize}{
  which normalization to use, defaults to "NFC". Other option is "NFD". Any other input will result in no normalisation being performed.
  }
  \item{size.order}{
  by default graphemes will be identified largest first. If FALSE then the order as specified in the orthography profile will be used.
  }
  \item{context}{
  when context = TRUE then the profile is assumed to have columns named "left" and "right" specifying the context for transliteration.
  }
  \item{global.match}{
  how should the tokenization be performed. By default, use global match, i.e. each grapheme will be replaced throughout the whole string before the next grapheme is taken up. If FALSE, then the replacements will be performed along each string, similar in result to a finite state transducer, i.e. which grapheme matches the start, then proceed along the string.
  }
  \item{file}{
  filename for results to be written. No suffix should be specified, as various different files with different suffixes are produced.
  }
} 
\details{
The \code{tokenize} function will tokenize (and replace, i.e. transliterate) strings into graphemes. First, the graphemes from the \code{.prf} table will used for the tokenization , starting from the largest graphemes (in unicode-code-point count using NFC normalisation by default). Any unmatched sequences in the data will be reported with a warning. Any rules specified in the \code{.rules} file will be applied at the end.
}
\value{
Without specificatino of \code{file.out}, the function \code{tokenize} will return a list of three:
\item{strings}{the vector with the parsed strings}
\item{profile}{a dataframe with the graphemes and some more information}
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
tokenize_old(
    c("nana", "änngschä", "ach")
    , op
    , graphemes = "graphemes")
    
# with replacements and an error message
tokenize_old(
    c("Naná", "änngschä", "ach")
    , op
    , graphemes = "graphemes"
    , replacements = "replacements")
}
