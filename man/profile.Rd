\name{orthography profile}
\alias{orthography}
\alias{write.orthography.profile}
\alias{read.orthography.profile}
\title{
Writing and reading of orthography profiles
}
\description{
To process strings, it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile. Function to write and read orthography profiles are provided here.
}
\usage{
write.orthography.profile(strings, 
  replacements = TRUE, sep = NULL, file = NULL, info = TRUE)

read.orthography.profile(file, 
  graphemes = "graphemes",  replacements = "replacements")
}
\arguments{
  \item{strings}{
  vector of strings to the tokenized.
  }
  \item{graphemes}{
  name (or number) of the column in the orthography profile listing the graphemes to be used for the tokenization. 
  }
  \item{replacements}{
  for writing profiles: logical (should a column with replacements be added?). For reading profiles:string with name (or number) of the column in the orthography profile listing the replacements. 
  }
  \item{sep}{
  separator to separate the strings. When NULL (by default), then unicode character definitions are used to split. When \code{sep} is specified, strings are split by this separator.
  }
  \item{file}{
  filename for profiles to be written, or for profile to be read. When writing, filenames not ending in \code{.prf} get this suffix attached. When reading, specificiation of names without the suffix \code{.prf} also works.
  }
  \item{info}{
  logical: should extra Unicode-info (codepoints and Unicode names) be added in the generation of an orthography profile? Defaults to \code{FALSE}.
  }
} 
\details{
To produce an orthography profile, consider using \code{write.orthography.profile} to produce a useful starting point. This function will take a vector of strings and produce a table with all graphemes and their frequency. Combining diacritics and spacing modifier letters are combined with their preceding characters to obtain a reasonable first guess at available graphemes. There is no attempt made to recognize 'tailored' multigraphs (like 'sch' of 'aa'). Such multigraphs can be specified manually in the output file of this function.

The function \code{read.orthography.profile} can be used to read any saved orthography profile into R, though mostly this function will be used internally by \code{tokenize}. An orthography profile currently consists of minimally a tab-separated table with a column of graphemes to be separated, typically using a \code{.prf} suffix. Further columns with replacements can be specified. When further rule-based changes are needed (for complex orthographic regularities), these can be specified as regex 'pattern' and 'replacement' in a separate 2-column tab-separated file with the same name, but using a \code{.rules} suffix.
}
\value{
\code{write.orthography.profile} produces a dataframe with all grapheme(-clusters) and frequencies. By default, a column 'replacements' is added, which is identical to the graphemes. This column is a useful starting point to specify orthographic changes to be used by \code{tokenize}. Also by default the Unicode codepoints and names are added to make it easier to find encoding inconsistencies in the provided data. When \code{file} is specified, the profile will be written to this file.

\code{read.orthography profile} reads a profile from disk, possible also included any rules file.
}
\author{
Michael Cysouw
}
\examples{
# produce statistics
example <- "nana änngschä ach"
write.orthography.profile(example)

# make a better orthography profile
gr <- cbind(c("a","ä","n","ng","ch","sch"),c("a","e","n","N","x","sh"))
colnames(gr) <- c("graphemes","replacements")
( op <- list(graphs = gr, rules = NULL) )
}
