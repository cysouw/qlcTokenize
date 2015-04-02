# Orthography Profiles
# An orthography profile is assumed to be a list of (1) a character matrix and (2) some regex-expression as a list

# =================================================================
# write orthography profile with frequencies for further processing
# =================================================================

write.orthography.profile <- function(strings, 
      replacements = TRUE, sep = NULL, file = NULL, info = TRUE) {
  
  # prepare naming of file
  if (!is.null(file)) {
    if (substr(file, nchar(file)-3, nchar(file)) != ".prf") {
      filename <- paste(file, ".prf", sep = "")
    }
  }
  
  # split using unicode definitions
  # except when 'sep' is specified, then split by sep
  if (is.null(sep)) {
    splitted <- stri_split_boundaries(strings, type = "character")
  } else {
    splitted <- strsplit(strings, sep)
    # remove empty characters
    splitted <- sapply(splitted, function(x){x[x != ""]}, simplify = FALSE)
  }
    
  # prepare result
  summary <- table(unlist(splitted))
  chars <- names(summary)
  
  # add a column for editing replacements when 'replacements = TRUE'
  if (replacements) {
    result <- cbind(chars, chars, summary)
    colnames(result) <- colnames(result) <- c("graphemes", 
                                              "replacements", 
                                              "frequency")
  } else {
    result <- cbind(chars, summary)
    colnames(result) <- colnames(result) <- c("graphemes", 
                                              "frequency")   
  }
  rownames(result) <- NULL

  # add codepoints and Unicode names when info = TRUE
  if (info) {    
    codepoints <- sapply(chars, function(x) {
      paste(stri_trans_general(unlist(strsplit(x,"")), "Any-Hex/Unicode")
            , collapse = ", ")
    })
    names <- sapply(chars, function(x) {
      paste(stri_trans_general(unlist(strsplit(x,"")), "Any-Name")
            , collapse = ", ")
    })
    names <- gsub("\\N{", "", names, fixed= TRUE)
    names <- gsub("}", "", names, fixed = TRUE)
    result <- cbind(result, codepoints, names)
  }

  # return statistics as data frame, or write to file when "file" is specified
  if (is.null(file)) {
    result <- as.data.frame(result)
    # remove rownames to avoid confusion
    rownames(result) <- NULL
    return(result)
  } else {
    write.table(result, file = file, quote = FALSE, sep = "\t", row.names = FALSE)
  }
}
  
# ========================
# read orthography profile
# ========================

read.orthography.profile <- function(file, 
      graphemes = "graphemes", replacements = "replacements",
      left.context = NULL, right.context = NULL) {

  # when "file" is an R object
  if (!is.character(file)) {
    if (length(file) == 2) {
      # assume that it is already a valid orthography profile
      return (file)
    } else if (is.data.frame(file)) {
      # assume that it is a dataframe with graphemes
      if (ncol(file) > 1) {
        graphs <- file[,c(graphemes, replacements), drop = FALSE]
      } else {
        graphs <-  file
      }
      rules <-  NULL
      return( list(graphs = graphs, rules = rules) )
    } else {
      stop("no valid orthography information")
    }
  } else {
  # read from file
    
    # prepare naming of files
    if (substr(file, nchar(file)-2, nchar(file)) == "prf") {
      filename <- substr(file, 1, nchar(file)-4)
    } else {
      filename <- file
    }
    
    # prepare table
    graphemesFile <- paste(filename, ".prf", sep = "")
    if (file.exists(graphemesFile)) {
      graphs <- read.table(graphemesFile, sep = "\t", header = TRUE, 
                       colClasses = "character", quote = "", fill = TRUE)
      if (ncol(graphs) > 1) {
        graphs <- graphs[, c(graphemes, replacements, left.context, right.context), drop = FALSE]
      }
    } else {
      graphs = NULL
    }
  
    # prepare rules
    rulesFile <- paste(filename, ".rules", sep = "")
    if (file.exists(rulesFile)) {
      rules <- read.table(rulesFile, sep = "\t", header = FALSE,
                          colClasses = "character", quote = "")
    } else {
      rules  <- NULL
    }
    
    # return orthography profile as a list of two
    return( list(graphs = graphs, rules = rules) )
  }
}
