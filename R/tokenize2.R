# ================
# tokenize strings
# ================

#strings <- c("qtestY","testing","testedY")
#graphs <- c("ste","ti","te","st","ng","s","q","d","X")

tokenize2 <- function(strings, orthography.profile = NULL, transliterate = FALSE
                     , sep = " ", sep.replacement = "\u00B7", missing = "\u2047"
                     , replacements = "replacements", normalize = "NFC"
                     , size.order = FALSE, global.match = TRUE, context = FALSE
                     , file.out = NULL) {
 
  # ---------------
  # preprocess data
  # ---------------
  
	# separators
	internal_sep <- intToUtf8(1110000)
	user_sep <- sep

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

  # collapse strings for easier coding
  all <- paste(strings, collapse = internal_sep)
  all <- paste(internal_sep, all, internal_sep, sep = "")
  
  # --------------------
  # read or make profile
  # --------------------
  
  # read orthography profile (or make new one)
  if (is.null(orthography.profile)) {
    # make new orthography profile   
    profile  <- write.orthography.profile2(strings, info = FALSE)  
  } else {
    # read profile from file
    profile <- read.table(orthography.profile, sep = "\t", quote = ""
                          , header = T, fill = T, colClasses = "character")
    
  } 
  
  # -----------------------------------------
  # prepare regexes with context from profile
  # -----------------------------------------

  # normalise characters in profile, just to be sure
  graphs <- transcode(profile[,"graphemes"])
  
  if (transliterate) {
    trans <- transcode(profile[,replacements])
  }
  
  if (context) {
    
    left <- transcode(profile[,"left"])
    right <- transcode(profile[,"right"])
    
    # make classes if there is anything there
    if (sum(profile[,"class"] != "") > 0) {
      classes <- unique(profile[,"class"])
      classes <- classes[classes != ""]
      gr <- sapply(classes,function(x){graphs[profile[,"class"]==x]})
      cl <- sapply(gr,function(x){
        paste("((",paste(x,collapse=")|("),"))",sep="")
      })
      
      for (i in classes) {
        left <- gsub(i,cl[i],left)
        right <- gsub(i,cl[i],right)
      }
    }
    
    left[left != ""] <- paste("(?<=", left[left != ""], ")", sep = "")
    right[right != ""] <- paste("(?=", right[right != ""], ")", sep = "")
    contexts <- paste(left, graphs, right, sep = "")
    contexts <- gsub("$",internal_sep,contexts,fixed=T)
    contexts <- gsub("^",internal_sep,contexts,fixed=T)
    
  } else {
    contexts <- graphs
  }

  # --------------
  # reorder graphs
  # --------------
  
  # order graphs to size
  if (size.order) {
    graphs_parts <- strsplit(graphs, split = "")
    graphs_length <- sapply(graphs_parts, length)
    graph_order <- rev(order(graphs_length, decreasing = FALSE))
    
    graphs <- graphs[graph_order]
    contexts <- contexts[graph_order]
    
    if (transliterate) {
      trans <- trans[graph_order]
    }
  } 
  
  # --------------
  # regex matching
  # --------------
  
  # add internal separator so they are not categorized as missing
	contexts <- c(contexts, internal_sep)
	
	# do regex matching
	# order of graphs is important here!
	matches <- sapply(contexts,function(x){gregexpr(x,all,perl=T)[[1]]})

  # --------------------------------------------------
  # tokenize data, either global when global = T
  # --------------------------------------------------

	if (global.match) {

		taken <- c(NA)
		count <- 1

		# just loop through all regex matches in order of graphs
		for ( i in matches ) {
			len <- attr(i, "match.length")[1]-1
			for ( j in i ) {
				# if there is a match, and there is nothing yet, then take the grapheme
				if (j != -1 && prod(is.na(taken[j:(j+len)])) == 1) {
					taken[j:(j+len)] <- count
				}
			}
			count <- count+1
		}
		
		# some post-processing
		# identify NA: those are missing graphemes
		taken[is.na(taken)] <-  length(graphs)+1
		
		# identify where the next grapheme starts
		taken <- taken[c(diff(taken) != 0, TRUE)]
	
		# remove internal separator at start and end
		# they were added to identify NAs at start or finish
		taken <- tail(head(taken,-1),-1)
		
		# replace count-numbers with the corresponding graphs
		if (transliterate) {
		  taken <- paste( c(trans,internal_sep,missing)[taken], collapse = user_sep )
		} else {
			taken <- paste( c(graphs,internal_sep,missing)[taken], collapse = user_sep )
		}

  # -------------------------------------------------------
  # finite-state transducer behaviour when global = F
  # -------------------------------------------------------		
	
	} else {
		
		order <- unlist(matches)
		position <- 1
		taken <- c()
		
		if (transliterate) {
			print("not yet implemented")
		} else {
			graphs_order <- rep(graphs, times = sapply(matches,length))
		}
		
		# loop through all positions and take first match
		while(position <= nchar(all)) {
			
			hit <- which(order == position)[1]
			if (is.na(hit)) {
				taken <- c(taken, missing)
				position <- position+1
			} else {
				g <- graphs_order[hit]
				taken <- c(taken, g)
				position <- position + nchar(g)
			}
		}
		
		taken <- tail(head(taken,-1),-1)
		taken <- paste(taken, collapse =  user_sep)
		
	}
	
	# Split string by internal separator
	result <- strsplit(taken, split =  paste(sep, sep, sep = internal_sep))[[1]]	
	return(result)
	
}


