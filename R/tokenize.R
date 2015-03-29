# ================
# tokenize strings
# ================

tokenize <- function(strings, orthography.profile = NULL
                     , transliterate = FALSE
                     , graphemes = "graphemes", replacements = "replacements"
                     , sep = " ", sep.replacement = "#", missing = "\u2047"
                     , normalize = "NFC"
                     , size.order = TRUE, context = FALSE, global.match = TRUE
                     , file = NULL) {

  # --------------------------------------------
  # help functions to deal with regex characters
  # --------------------------------------------
  
  regexchars <- c(".","|","(",")","[","]","{","}","^","$","*","+","?")
  
  replace.regex <- function(x) {  
    for (i in 1:length(regexchars)) {
      x <- gsub(regexchars[i]
                , intToUtf8(1110000 + i)
                , x
                , fixed = TRUE
                )
    }
    return(x)
  }
  
  restore.regex <- function(x) {
    for (i in 1:length(regexchars)) {
      x <- gsub(pattern = intToUtf8(1110000 + i)
                , replacement = regexchars[i]
                , x
                , fixed = TRUE
                )
    }
    return(x)
  }
   
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
  
  # keep original strings, and normalize NFC everything by default
  originals <- as.vector(strings)
  strings <- transcode(originals)

  # replace regex special characters in strings
  strings <- replace.regex(strings)
 
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
    if (context) {
      profile <- read.orthography.profile(orthography.profile
                                          , graphemes
                                          , replacements
                                          , left.context = "left"
                                          , right.context = "right")  
    } else {
      profile <- read.orthography.profile(orthography.profile
                                        , graphemes
                                        , replacements)  
    }
    # when there is no prf-file (i.e. there is only a rules file), 
    # then make a default profile
    if(is.null(profile$graphs)) {
      profile$graphs <- write.orthography.profile(strings, info = FALSE)
    }
  } 
    
  # normalise characters in profile, just to be sure
  graphs <- transcode(profile$graphs[,graphemes])
  
  # order graphs to size
  if (size.order) {
    graphs_parts <- strsplit(graphs, split = "")
    graphs_length <- sapply(graphs_parts, length)
    graph_order <- order(graphs_length, decreasing = TRUE)
  } else {
    graph_order <- 1:length(graphs)
  }

  # replace regex special characters in profile
  # this might be necessary when a external file with a profile is used
  graphs <- replace.regex(graphs)
  
  # --------------------------------------------------
  # tokenize data, either global when global.match = T
  # --------------------------------------------------
  
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
    
  # -------------------------------------------------------
  # finite-state transducer behaviour when global.match = F
  # -------------------------------------------------------
    
  } else {
        
    # order graphs by size and add start-of-string regex
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
  
  # ------------------
  # tokenization rules
  # ------------------
  
  # apply rules when specified in the orthography profile
  if(!is.null(profile$rules)) {
    
    # put back regexchars: user will have to take care for them in their regexes
    strings <- restore.regex(strings)
    graphs <- restore.regex(graphs)
    
    # do the tokenization rules
    for (i in 1:nrow(profile$rules) ) {
      regex <- transcode(as.character(profile$rules[i,]))
      regex <- gsub(pattern = user.sep
                    , replacement = sep
                    , regex
                    , fixed = TRUE
                    )
      strings <- gsub(pattern = regex[1]
                      , replacement = regex[2]
                      , strings
                      , perl = TRUE
                      )
      }
    
    # add new graphs that appear in tokenization
    new_strings <- profile$rules[,2]
    new_graphs <- unique(unlist(strsplit(new_strings, user.sep)))
    graphs <- union(graphs, new_graphs)
    
    # again replace regex special characters in strings and graphs
    strings <- replace.regex(strings)
    graphs <- replace.regex(graphs)
    
  }
  
  # -------------------------------------------------    
  # check for missing graphems in orthography profile
  # -------------------------------------------------
  
  # prepare regexes with context from profile
  
  start <- paste("(?<=^|", sep, ")", sep = "")
  end <- paste("(?=", sep, "|$)", sep = "")
  
  if (context) {
  
    left <- transcode(profile$graphs[,"left"])
    right <- transcode(profile$graphs[,"right"])
    left[left != ""] <- paste("(?<=", left[left != ""], sep, ")", sep = "")
    right[right != ""] <- paste("(?=",  sep, right[right != ""], ")", sep = "")
    contexts <- paste(start, left, graphs, right, end, sep = "")
    
  } else {
    contexts <- paste (start, graphs, end, sep = "")
  }
    
  # order contexts to size to get complex regexes firts
  context_parts <- strsplit(contexts, split = "")
  context_length <- sapply(context_parts, length)
  context_order <- order(context_length, decreasing = TRUE)
  
  # implementation: just again take some high unicode range 
  # and replace all graphemes with individual characters
  # while checking for remainders
  check <- strings
  for (i in context_order) { 
    check <- gsub(pattern = contexts[i]
                  , replacement = ""
                  , check
                  , perl = T
                  )
    strings <- gsub(pattern = contexts[i]
                    , replacement = intToUtf8(1110020 + i)
                    , strings
                    , perl = T
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
    warning(paste("\nThe following character(s):\n"
                  , paste(problem.characters, collapse = " ")
                  , "\nare found in the input data, but are not in the orthography profile.\nCheck output$missing for a table with all problematic strings."
                  ))
    problems <- cbind(originals[leftover],check[leftover])
    colnames(problems) <- c("originals","unmatched")
    rownames(problems) <- which(leftover)
  } else {
    problems  <- NULL
  }
  
  # --------------------------------
  # replace orthography if specified
  # --------------------------------
  
  if (transliterate) {
    
    # replace problem characters with questionmark in tokenization
    for (i in problem.characters) {
      strings <- gsub(i, missing, strings)
    }
    
    # use replacement graphs to put back
    graphs <- profile$graphs[,replacements]
    graphs <- transcode(graphs)
    graphs[graphs == "NULL"] <- ""
  }

  # ------------------------------------
  # put back the substitution characters
  # ------------------------------------

  for (i in context_order) {
    strings <- gsub(pattern = intToUtf8(1110020 + i)
                    , replacement = graphs[i]
                    , strings
               #    , perl = TRUE
                    , fixed = TRUE
                    )
  }
  
  # put back regexchars
  strings <- restore.regex(strings)
  graphs <- restore.regex(graphs)
  
  # ---------------
  # prepare results
  # ---------------

  # make new profile of actual tokenization
  if (transliterate) {
    profile <- write.orthography.profile(strings, replacements = FALSE
                                         , sep = sep, info = TRUE)
  } else {
    profile <- write.orthography.profile(strings, replacements = TRUE
                                         , sep = sep, info = TRUE)
  }
  # remove missing character from profile in case of unmatched transliterations
  profile <- profile[profile$graphemes != missing,]
  
  # insert user specified separator
  strings <- gsub(user.sep, sep.replacement, strings)
  strings <- gsub(sep, user.sep, strings)

  # combine orignal strings and tokenized strings in a dataframe
  tokenization <- as.data.frame(cbind(originals = originals, tokenized = strings))

  # --------------
  # output as list
  # --------------
  
  if (is.null(file)) {
   
    return(list(  strings = tokenization
                , profile = profile
                , missing = problems
                ))
    
  # ---------------
  # output to files
  # ---------------

  } else {
        
    # file with tokenization is always returned
    write.table(  tokenization
                , file = paste(file, ".tokenized", sep = "")
                , quote = FALSE, sep = "\t", row.names = FALSE)
    
    # file with orthography profile: user has to watch out for overwrite!
    write.table(  profile
                , file = paste(file, ".prf", sep="")
                , quote = FALSE, sep = "\t", row.names =  FALSE)
    
    # additionally write table with warnings when they exist
    if (sum(leftover) > 0 ) {      
      write.table(  problems
                  , file = paste(file, ".missing", sep = "")
                  , quote = FALSE, sep = "\t", row.names = FALSE)
    }
  }
}

# ----------------------------
# alternative name of function
# ----------------------------

tokenise <- tokenize
