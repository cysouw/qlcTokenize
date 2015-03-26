# ================
# tokenize strings
# ================

tokenize <- function(strings, orthography.profile = NULL
                     , transliterate = FALSE
                     , graphemes = "graphemes", replacements = "replacements"
                     , sep = " ", sep.replacement = "#"
                     , normalize = "NFC", use.size.order = TRUE
                     , global.match = TRUE
                     , file = NULL) {
  # ---------------
  # preprocess data
  # ---------------
  
  # use abstract separator internally
  user.sep <- sep
  sep <- intToUtf8(1110000)
  
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

  # remove regex special characters in profile
  regexchars <- c(".","|","(",")","[","]","{","}","^","$","*","+","?")
  for (i in 1:length(regexchars)) {
    strings <- gsub(regexchars[i], intToUtf8(1110000 + i), strings, fixed = TRUE)
  }
 
  # --------------------
  # read or make profile
  # --------------------
  
  # read orthography profile (or make new one)
  if (is.null(orthography.profile)) {
    # make new orthography profile   
    graphs  <- write.orthography.profile(strings, info = FALSE)
    profile <- list(graphs = graphs, rules = NULL)    
  } else {
    # read profile from file
    profile <- read.orthography.profile(orthography.profile
                                        , graphemes
                                        , replacements)
    # when there is no prf-file, make a default profile
    if(is.null(profile$graphs)) {
      profile$graphs <- write.orthography.profile(strings, info = FALSE)
    }
  } 
    
  # normalise characters in profile, just to be sure
  graphs <- transcode(profile$graphs[,graphemes])
  
  # order graphs to size
  if (use.size.order) {
    graphs_parts <- strsplit(graphs, split = "")
    graphs_length <- sapply(graphs_parts, length)
    graph_order <- order(graphs_length, decreasing = TRUE)
  } else {
    graph_order <- 1:length(graphs)
  }
  
  # -------------------------------------------------
  # tokenize data, either global or along the strings
  # -------------------------------------------------
  
  if (global.match) {
    
    # replace strings by random unicode range in order of size of grapheme clusters
    for (i in graph_order) { 
      strings <- gsub(pattern = graphs[i]
                      , replacement = intToUtf8(1110020 + i)
                      , strings
                      , fixed = TRUE
                      )    
    }
    
    # parse strings now is easy, because every grapheme is one unicode character
    strings <- strsplit(strings, split = "") 
    
    # and put them back with separator
    strings <- sapply(strings, function(x){paste(x, collapse = sep)})
    
    # put back the multigraphs-substitution characters
    for (i in graph_order) {
      strings <- gsub(pattern = intToUtf8(1110020 + i)
                      , replacement = graphs[i]
                      , strings
                      , fixed = TRUE
                      )
    }
    
  } else {
    
    # -------------------------------------------------------------
    # finite-state transducer behaviour when "global.match = FALSE"
    # -------------------------------------------------------------
    
    # order graphs by size and add start regex
    gr <- paste("^", graphs[graph_order], sep = "")
    
    # the actual attempt to implement a transducer using simple loops
    # highly inefficient for large datasets
    make.tokens <- function(string) {
      result <- ""  
      while(nchar(string) != 0) {
        for (i in gr) {
          if (grepl(i, string)) {
            result <- paste(result, substr(i, 2, nchar(i)), sep = sep)
            string <- sub(i, "", string)
            break
          } else if (i == tail(gr,1)) {
            result <- paste(result, substr(string, 1, 1), sep= sep)
            string <- substr(string, 2, nchar(string))
          }
        }
      }
      result <- substr(result, 2, nchar(result))
      return(result)
    }  
    
    strings <- sapply(strings,make.tokens)
    names(strings) <- NULL
  }

  # ---------------
  # post processing
  # ---------------  
  
  # put back regexchars
  for (i in 1:length(regexchars)) {
    strings <- gsub(intToUtf8(1110000 + i), regexchars[i], strings, fixed = TRUE)
    graphs <- gsub(intToUtf8(1110000 + i), regexchars[i], graphs, fixed = TRUE)
  }
  
  # apply rules when specified in the orthography profile
  if(!is.null(profile$rules)) {
    for (i in 1:nrow(profile$rules) ) {
      regex <- transcode(as.character(profile$rules[i,]))
      regex <- gsub(pattern = " ", replacement = sep, regex)
      strings <- gsub(pattern = regex[1], replacement = regex[2], strings)
    }
  }
  
  # -------------------------------------------------    
  # check for missing graphems in orthography profile
  # -------------------------------------------------
  
  # implementation: just again take some high unicode range 
  # and replace all graphemes with individual characters
  check <- strings
  for (i in graph_order) { 
    check <- gsub(graphs[i],"",check, fixed = TRUE)
    strings <- gsub(pattern = graphs[i]
                    , replacement = intToUtf8(1110020 + i)
                    , strings
                    , fixed = TRUE
                    )    
  }
  
  # check for missing graphems in orthography profile and produce warning
  # first remove separators
  check <- gsub(paste(sep, "+", sep = ""), "", check)
  # remember unique problem characters
  problem.characters <- unique(unlist(strsplit(check, "")))
  # add spaces to show lone diacritics
  check <- stri_replace_all_regex(check, "(\\p{DIACRITIC})", " $1")
  # where are leftover characters?
  leftover <- check != ""
  # prepare error message
  if (sum(leftover) > 0) {
    warning(paste("\nThe character(s):\n", paste(problem.characters, collapse = " "), "\nare found in the input data, but are not in the orthography profile.\nCheck output$warnings for a table with all problematic strings."))
    problems <- cbind(originals[leftover],check[leftover])
    colnames(problems) <- c("original strings","unmatched parts")
    rownames(problems) <- which(leftover)
  }

  # --------------------------------
  # replace orthography if specified
  # --------------------------------
  
  if (transliterate) {
    
    # replace problem characters with questionmark in tokenization
    for (i in problem.characters) {
      strings <- gsub(i, "?", strings)
    }
    
    # use replacement graphs to put back
    graphs <- profile$graphs[,replacements]
    graphs <- transcode(graphs)
    graphs[graphs == "NULL"] <- ""
  }
  
  # put back the multigraphs-substitution characters
  for (i in graph_order) {
    strings <- gsub(pattern = intToUtf8(1110020 + i)
                    , replacement = graphs[i]
                    , strings
                    , fixed = TRUE
                    )
  }

  # ---------------
  # prepare results
  # ---------------

  # first: make new profile of actual tokenization
  profile <- write.orthography.profile(strings, sep = sep, info = TRUE)

  # second: combine orignal strings and tokenized strings in a dataframe
  strings <- gsub(user.sep, sep.replacement, strings)
  strings <- gsub(sep, user.sep, strings)
  tokenization <- as.data.frame(cbind(originals = originals, tokenized = strings))

  # ---------------
  # output internal
  # ---------------
  
  # various options for output
  if (is.null(file)) {
    # output as R object (list)
    
    if (is.null(orthography.profile)) {
      # user didn't specify an orthography profile, so give one in return
      return(list(strings = tokenization
                  , profile = profile
                  , warnings =  NULL))
      
    } else {
      if (transliterate) {
        # with replacements, no orthography profile can be returned in a sensible way
        if (sum(leftover) == 0 ) {
          # no errors, just return tokenization
          return(list(strings = tokenization
                      , profile = NULL
                      , warnings = NULL))
        } else {
          # with errors, add table with errors
          return(list(strings = tokenization
                      , profile = NULL
                      , warnings = problems))
        }      
      } else {        
        # with OP, but without replacements, 
        # an orthography profile is returned as well
        if (sum(leftover) == 0 ) {
          # no errors, return tokenization and OP
          return(list(strings = tokenization
                      , profile = profile
                      , warnings = NULL))
        } else { 
          # with errors, add table with errors
          return(list(strings = tokenization
                      , profile = profile
                      , warnings = problems))
        } 
      }
    }
    
  } else {
    
    # ---------------
    # output to files
    # ---------------
    
    # file with tokenization is always returned
    write.table(tokenization
                , file = paste(file, ".tokenized", sep = "")
                , quote = FALSE, sep = "\t", row.names = FALSE)
    
    if (!transliterate) {
      # additionally write orthography profile when no replacements are made
      write.table(profile
                  , file = paste(file, ".prf", sep="")
                  , quote = FALSE, sep = "\t", row.names =  FALSE)
    }
    
    if (sum(leftover) > 0 ) {
      # additionally write table with warnings
      write.table(problems
                  , file = paste(file, ".warnings", sep = "")
                  , quote = FALSE, sep = "\t", row.names = FALSE)
    }
  }
}

# ----------------------------
# alternative name of function
# ----------------------------

tokenise <- tokenize
