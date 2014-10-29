
# ================
# tokenize strings
# ================

tokenize <- function(strings, orthography.profile = NULL, replace = FALSE,
                graphemes = "graphemes", patterns = "patterns", replacements = "replacements",
                sep = "\u00B7", normalize = "NFC", 
                space.replacement = TRUE, file = NULL) {
  
  # normalization
  if (normalize == "NFC" | normalize == "nfc") {
    transcode <- stri_trans_nfc
  } else if (normalize == "NFD" | normalize == "nfd") {
    transcode <- stri_trans_nfd
  } else {
    transcode <- identity
  }
  
  # prepare strings, and normalize NFC everything by default
  originals <- as.vector(strings)
  strings <- transcode(originals)
  
  # read orthography profile (or make new one)
  if (is.null(orthography.profile)) {
    # make new orthography profile   
    graphs  <- write.orthography.profile(strings, info = FALSE)
    profile <- list(graphs = graphs, rules = NULL)    
  } else if (is.character(orthography.profile)) {
    # read profile from file
    profile <- read.orthography.profile(orthography.profile
                                        , graphemes
                                        , patterns
                                        , replacements)
  } else {
    # in case orthography profile is an R object
    profile <- orthography.profile
  }
  
 
  
  # do grapheme-splitting
  if(!is.null(profile$graphs)) {
    # normalise characters in profile, just to be sure
    graphs <- transcode(profile$graphs[,graphemes])
    
    # possibly add space to graphs
    if (space.replacement) { graphs <- c(" ", graphs) }
    
    # order graphs to size
    graphs_parts <- strsplit(graphs, split = "")
    graphs_length <- sapply(graphs_parts, length)
    graph_order <- order(graphs_length, decreasing = TRUE)
    
    # check for missing graphems in orthography profile
    # and take care of multigraphs
    # just take some high unicode range 
    # and replace all graphemes with individual characters
    check <- strings
    for (i in graph_order) { 
      check <- gsub(graphs[i],"",check, fixed = TRUE)
      strings <- gsub(pattern = graphs[i]
                      , replacement = intToUtf8(1110000 + i)
                      , strings, fixed = TRUE)    
    }
    
    # check for missing graphems in orthography profile and produce warning
    check <- stri_replace_all_regex(check, "(\\p{DIACRITIC})", " $1")
    leftover <- check != ""
    if (sum(leftover) > 0) {
      warning("There are characters in the data that are not in the orthography profile. Check warnings for a table with all problematic strings.")
      problems <- cbind(originals[leftover],check[leftover])
      colnames(problems) <- c("original strings","unmatched parts")
      rownames(problems) <- which(leftover)
    }
    
    # parse strings
    # and put them back with separator
    strings <- strsplit(strings, split = "")  
    strings <- sapply(strings, function(x){paste(x, collapse = sep)})
    
    # replace orthography if specified
    if (replace) {
      graphs <- profile$graphs[,replacements]
      graphs <- transcode(graphs)
      graphs[graphs == "NULL"] <- ""
      # possibly add space to graphs
      if (space.replacement) { graphs <- c(" ", graphs) }
    }
    
    # put back the multigraphs-substitution characters
    for (i in graph_order) {
      strings <- gsub(pattern = intToUtf8(1110000 + i)
                      , replacement = graphs[i]
                      , strings, fixed = TRUE)
    }
    
    # remove superfluous spaces at start and end
    strings <- gsub(pattern = paste("^", sep, sep = ""), replacement = "", strings)
    strings <- gsub(pattern = paste(sep, "$", sep = ""), replacement = "", strings)
    
  } else {
    # with no graphemes-specified, nothing is parsed
    # also no error message returned
    leftover <- 0
  }
  
  # make traditional output when asked
  if (space.replacement){
    strings <- gsub(" ","#",strings)
    strings <- gsub(sep," ",strings)
    sep <- " # | "
  }
  
  # apply regexes when specified in the orthography profile
  if(!is.null(profile$rules)) {
    for (i in 1:nrow(profile$rules) ) {
      regex <- transcode(as.character(profile$rules[i,]))
      strings <- gsub(pattern = regex[1], replacement = regex[2], strings)
    }
  }
  
  # prepare results
  # first: combine orignal strings and tokenized strings in a dataframe
  tokenization <- as.data.frame(cbind(originals = originals, tokenized = strings))
  profile <- write.orthography.profile(strings, sep = sep, info = TRUE)
  
  # various options for output
  if (is.null(file)) {
    # output as R object (list)
    
    if (is.null(orthography.profile)) {
      # user didn't specify an orthography profile, so give one in return
      return(list(strings = tokenization, orthography.profile = profile, warnings =  NULL))
      
    } else {
      if (replace) {
        # with replacements, no orthography profile can be returned in a sensible way
        if (sum(leftover) == 0 ) {
          # no errors, just return tokenization
          return(list(strings = tokenization, orthography.profile = NULL, warnings = NULL))
        } else {
          # with errors, add table with errors
          return(list(strings = tokenization, orthography.profile = NULL, warnings = problems))
        }      
      } else {        
        # with OP, but without replacements, an orthography profile is returned as well
        if (sum(leftover) == 0 ) {
          # no errors, return tokenization and OP
          return(list(strings = tokenization, orthography.profile = profile, warnings = NULL))
        } else { 
          # with errors, add table with errors
          return(list(strings = tokenization, orthography.profile = profile, warnings = problems))
        } 
      }
    }
    
  } else {
    # output as file(s)
    
    # file with tokenization is always returned
    write.table(tokenization
                , file = paste(file, "_tokenized.csv", sep = "")
                , quote = FALSE, sep = "\t", row.names = FALSE)
    
    if (!replace) {
      # additionally write orthography profile when no replacements are made
      write.orthography.profile(strings, sep = sep, info = TRUE
                                , file = paste(file, "_profile.prf", sep=""))
    }
    
    if (sum(leftover) > 0 ) {
      # additionally write table with warnings
      write.table(problems
                  , file = paste(file, "_warnings.csv", sep = "")
                  , quote = FALSE, sep = "\t", row.names = FALSE)
    }
  }
}

# alternative name of function
tokenise <- tokenize

