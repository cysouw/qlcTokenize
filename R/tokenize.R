# ================
# tokenize strings
# ================

tokenize <- function(strings
                      , orthography.profile = NULL
                      , transliterate = NULL
                      , parsing = "global"
                      , ordering = c("size","context")
                      , sep = " "
                      , missing = "\u2047"
                      , normalize = "NFC"
                      , context = FALSE
                      , case.insensitive = FALSE
                      , silent = FALSE
                      , file.out = NULL) {
 
  # ---------------
  # preprocess data
  # ---------------
  
	# separators
	internal_sep <- intToUtf8(1110000)
	user_sep <- sep

	# normalization
	if (normalize == "NFC") {
	  transcode <- stri_trans_nfc
	} else if (normalize == "NFD") {
	  transcode <- stri_trans_nfd
	} else {
	  transcode <- identity
	}
	
	# keep original strings, and normalize NFC everything by default
	originals <- as.vector(strings)
	strings <- transcode(originals)

  # collapse strings for easier coding
  all <- paste(strings, collapse = internal_sep)
  all <- paste(internal_sep, all, internal_sep, sep = "")
  
  # --------------------
  # read or make profile
  # --------------------
  
  # read orthography profile (or make new one)
  if (is.null(orthography.profile)) {
    # make new orthography profile
    if (normalize == "NFC") {
      profile  <- write.profile(strings, info = FALSE)  
    } else if (normalize == "NFD") {
      profile  <- write.profile(strings, normalize = "NFD", sep = "", info = FALSE)
    } else {
      profile  <- write.profile(strings, sep = "", info = FALSE) 
    }
  } else if (is.null(dim(orthography.profile))) {
    if (length(orthography.profile) > 1) {
      # assume that the strings are graphemes
      profile <- data.frame(graphemes = orthography.profile, stringsAsFactors = FALSE)
    } else {
    # read profile from file
    profile <- read.table(orthography.profile
                          , sep = "\t"
                          , quote = ""
                          , header = T
                          , fill = T
                          , colClasses = "character")
    }
  } else {
    # assume the profile is a suitable R object
    profile <- orthography.profile
  }

  # normalise characters in profile, just to be sure
  graphs <- transcode(profile[,"graphemes"])
  if (!is.null(transliterate)) {
    trans <- transcode(profile[,transliterate])
  }
  
  # -----------------------------------------
  # prepare regexes with context from profile
  # -----------------------------------------
 
  if (context) {
    
    # normalise them too
    left <- transcode(profile[,"left"])
    right <- transcode(profile[,"right"])

    # replace regex boundaries with internal separator
    right <- gsub("\\$", internal_sep, right)
    left <- gsub("^\\^", internal_sep, left)
    
    # make classes if there is anything there
    if (sum(profile[,"class"] != "") > 0) {
      
      classes <- unique(profile[,"class"])
      classes <- classes[classes != ""]
      groups <- sapply(classes,function(x){
        graphs[profile[,"class"] == x]
        })
      classes.regex <- sapply(groups,function(x){
        paste( "((", paste( x, collapse = ")|(" ), "))", sep = "")
        })
      
      for (i in classes) {
        left <- gsub(i, classes.regex[i], left, fixed = TRUE)
        right <- gsub(i, classes.regex[i], right, fixed = TRUE)
      }
    }
    
    # add lookahead/lookbehind syntax and combine everything together
    left[left != ""] <- paste("(?<=", left[left != ""], ")", sep = "")
    right[right != ""] <- paste("(?=", right[right != ""], ")", sep = "")
    # replace dot at start with internal separator
    left <- gsub("(?<=.", paste0("(?<!", internal_sep), left, fixed =  TRUE)
    
    contexts <- paste(left, graphs, right, sep = "")
        
  } else {
    contexts <- graphs
  }

  # -----------------
  # reorder graphemes
  # -----------------
  
  if (!is.null(ordering)) {
    
    size <- nchar(graphs)
    
    if (context) {
      context_availability <- (left != "" | right != "")
      if (!is.na(ordering["frequency"])) {
        frequency <- - sapply(contexts, function(x) {
                        stri_count_regex(all
                             , pattern = x
                             , case_insensitive = case.insensitive)})
      } else {
        frequency <- rep(T, times = length(graphs)) 
      }
    } else {
      context_availability <- rep(T, times = length(graphs))   
      if (!is.na(ordering["frequency"])) {
        frequency <- - sapply(contexts, function(x) {
                       stri_count_fixed(all
                             , pattern = x
                             , case_insensitive = case.insensitive
                             , overlap =  TRUE)})
      } else {
        frequency <- rep(T, times = length(graphs)) 
      }
    }
    
    # order according to dimensions chosen by user in "ordering"    
    dimensions <- list(size = size
                       , context = context_availability
                       , frequency = frequency)
    graph_order <- rev(do.call(order, dimensions[ordering]))

  } else {
    graph_order <- 1:length(graphs)
  }
  
  # change order and add internal separator
  graphs <- graphs[graph_order]
  graphs <- c(graphs, internal_sep)

  contexts <- contexts[graph_order]
  contexts <- c(contexts, internal_sep)
  
  if (!is.null(transliterate)) {
    trans <- trans[graph_order]
    trans <- c(trans, internal_sep)
  }
  
  # --------------
  # regex matching
  # --------------
  
  if (context) {
  	matches <- sapply(contexts, function(x) {
      stri_locate_all_regex(all
                            , pattern = x
                            , case_insensitive = case.insensitive
                            )[[1]]
      })
  } else {
    matches <- sapply(contexts, function(x) {
      stri_locate_all_fixed(all
                            , pattern = x
                            , case_insensitive = case.insensitive
                            , overlap = TRUE
                            )[[1]]
    })
  }
  
  # --------------------------------------
  # tokenize data, either global or linear
  # --------------------------------------

	if (!is.na(pmatch(parsing,"global"))) {

    # preparation
		taken <- c(NA)
    breaks <- c(NA)
    breakcounter <- 1
    frequency <- rep.int(x = 0, times = length(contexts))

		# just loop through all regex matches 
    # first in order of graphs, then in order of matches
		for ( i in 1:length(matches) ) {
			for ( j in 1:nrow(matches[[i]]) ) {
        r <- matches[[i]][j,]
				# if there is a match, and there is nothing yet, then take the grapheme
        if (is.na(r[1])) {
          break
        } else {
  				if ( prod(is.na(taken[r[1]:r[2]])) == 1 ) {
  					taken[r[1]:r[2]] <- i
            breaks[r[1]:r[2]] <- breakcounter
            breakcounter <- breakcounter + 1
            frequency[i] <- frequency[i] + 1
  				}
				}
			}
		}
		
		# some post-processing
		# identify NA: those are missing graphemes
		taken[is.na(taken)] <-  i+1
		breaks[is.na(breaks)] <- (breakcounter):(breakcounter + sum(is.na(breaks)) - 1)
    
		# identify where the next grapheme starts
		taken <- taken[c(diff(breaks) != 0, TRUE)]
	
		# remove internal separator at start and end
		# they were added to identify NAs at start or finish
		taken <- tail(head(taken,-1),-1)
		
		# replace count-numbers with the corresponding graphs
		if (!is.null(transliterate)) {
		  taken <- paste( c(trans, missing)[taken], collapse = user_sep )
		} else {
			taken <- paste( c(graphs, missing)[taken], collapse = user_sep )
		}

  # --------------------------------------------------------
  # finite-state transducer behaviour when parsing = "linear
  # --------------------------------------------------------	
	
	} else if (!is.na(pmatch(parsing,"linear"))) {
		
    # preparations
		all.matches <- do.call(rbind,matches)[,1]
		position <- 1
		taken <- c()
    frequency <- rep.int(x = 0, times = length(contexts))
		
		graphs_match_list <- rep(graphs, times = sapply(matches, dim)[1,])
    contexts_match_list <- rep(1:length(contexts), times = sapply(matches, dim)[1,])
    
		if (!is.null(transliterate)) {
			trans_match_list <- rep(trans, times = sapply(matches, dim)[1,])
		} else {
      trans_match_list <- graphs_match_list
		}
		
		# loop through all positions and take first match
		while(position <= nchar(all)) {
			
			hit <- which(all.matches == position)[1]
			if (is.na(hit)) {
				taken <- c(taken, missing)
				position <- position + 1
			} else {
				taken <- c(taken, trans_match_list[hit])
				position <- position + nchar(graphs_match_list[hit])
        rule <- contexts_match_list[hit]
        frequency[rule] <- frequency[rule] + 1
			}
		}
		
		taken <- tail(head(taken,-1),-1)
		taken <- paste(taken, collapse =  user_sep)
		
	} else {
    stop(paste0("The tokenization method \"",parsing,"\" is not defined"))
	}
	
  # ----------------------
  # preparation of results
  # ----------------------
  
	# Split string by internal separator
  result <- strsplit(taken, 
                     split =  paste(sep, sep, sep = internal_sep)
                     )[[1]]

  if (is.null(transliterate)) {
    strings.out <- data.frame(
      cbind(originals = originals, tokenized = result)
      , stringsAsFactors = FALSE)
  } else {
    strings.out <- data.frame(
      cbind(originals = originals, transliterated = result)
      , stringsAsFactors = FALSE)
  }
    
  # Make a list of missing and throw warning
  whichProblems <- grepl(pattern = missing, x = result)
  problems <- strings.out[whichProblems,]
  colnames(problems) <- c("originals", "errors")

  if ( nrow(problems) > 0) {
    
    # make a profile for missing characters
    if (normalize == "NFD") {
      split <- ""
    } else {
      split <- NULL
    }
    
    if (case.insensitive) {
      o <- write.profile(stri_trans_tolower(strings[whichProblems])
                         , info = FALSE
                         , sep = split
                         )$graphemes
    } else {
      o <- write.profile(strings[whichProblems]
                         , info = FALSE
                         , sep = split
                         )$graphemes
    }
    
    e <- write.profile(result[whichProblems]
                       , info = FALSE
                       , sep = split
                       )$graphemes
    missing <- setdiff(o,e)
    problemChars <- write.profile(missing)
    
    if ( !silent ) {
      warning("\nThere were unknown characters found in the input data.\nCheck output$missing for a table with all problematic strings.")
      
    }
  } else {
    problems <- NULL
    problemChars <- NULL
  }
  
  # Remove counter for internal separator
  # Reorder profile according to order and add frequency of rule-use
  frequency <- head(frequency, -1)
  profile <- data.frame(profile[graph_order,], stringsAsFactors = FALSE)
  if (ncol(profile) == 1) {
    colnames(profile) <- "graphemes"
  }
  profile.out <- cbind(frequency, profile)
  
  # --------------
  # output as list
  # --------------
  
  if (is.null(file.out)) {
    
    return(list(strings = strings.out
                , profile = profile.out
                , errors = problems
                , missing = problemChars
                ))
    
  } else {   
    
  # ---------------
  # output to files
  # ---------------
    
    # file with tokenization is always returned
    write.table(  strings.out
                  , file = paste(file.out, "_strings.tsv", sep = "")
                  , quote = FALSE, sep = "\t", row.names = FALSE)
    
    # file with orthography profile
    write.table(  profile.out
                  , file = paste(file.out, "_profile.tsv", sep="")
                  , quote = FALSE, sep = "\t", row.names =  FALSE)
    
    # additionally write tables with errors when they exist
    if ( !is.null(problems) ) {      
      write.table(  problems
                    , file = paste(file.out, "_errors.tsv", sep = "")
                    , quote = FALSE, sep = "\t", row.names = FALSE)   
      write.table(  problemChars
                    , file = paste(file.out, "_missing.tsv", sep = "")
                    , quote = FALSE, sep = "\t", row.names = FALSE)
    }
  } 
}
  



