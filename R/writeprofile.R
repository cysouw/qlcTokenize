
# =================================================================
# write orthography profile with frequencies for further processing
# =================================================================

write.profile <- function(strings
                         , normalize = NULL
                         , info = TRUE
                         , editing = FALSE
                         , sep = NULL
                         , file.out = NULL) {
  
  # normalization
  if (is.null(normalize)) {
    transcode <- identity
  } else if (normalize == "NFC") {
    transcode <- stri_trans_nfc
  } else if (normalize == "NFD") {
    transcode <- stri_trans_nfd
    # default splitting in NFD is set to sep = ""
    if (is.null(sep)) {
      sep <- ""
    }
  } 
  strings <- transcode(strings)
  
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
  frequency <- table(unlist(splitted))
  chars <- names(frequency)
  
  # add columns for editing when 'editing = TRUE'
  if (editing) {
    graphemes <- cbind(  left = ""
                       , graphemes = chars
                       , right = ""
                       , class = ""
                       , replacements = chars)
  } else {
    graphemes <- chars
  }

  # add frequency, codepoints and Unicode names when info = TRUE
  if (info) {    
    codepoints <- sapply(chars, function(x) {
      paste(stri_trans_general(unlist(strsplit(x,"")), "Any-Hex/Unicode")
            , collapse = ", ")})
    
    names <- sapply(chars, function(x) {
      paste(stri_trans_general(unlist(strsplit(x,"")), "Any-Name")
            , collapse = ", ")})   
    names <- gsub("\\N{", "", names, fixed= TRUE)
    names <- gsub("}", "", names, fixed = TRUE)
    
    graphemes <- cbind(graphemes, frequency, codepoints, names)
  }

  # return result as data frame, or write to file when "file" is specified
  if (is.null(file.out)) {
    result <- as.data.frame(graphemes, stringsAsFactors = FALSE)
    rownames(result) <- NULL
    return(result)
  } else {
    write.table(graphemes
                , file = file.out
                , quote = FALSE
                , sep = "\t"
                , row.names = FALSE)
  }
}
