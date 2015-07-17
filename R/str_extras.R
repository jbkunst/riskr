#' Slugify
#' @description Slugify a string.
#' @param string A string
#' @export
str_slug <- function(string, sep = "-"){
  string %>% 
    tolower() %>% 
    gsub("(?!')[[:punct:]]", " ", ., perl = TRUE) %>% 
    iconv(to = "ASCII//TRANSLIT") %>% 
    gsub("\\s+", " ", .) %>% 
    gsub("^\\s+|\\s+$", "", .) %>%  
    gsub(" ", sep, .)
}

#' str_left
#' @description A description.
#' #' @param string A string
#' @export
str_left <- function(string, nchars){
  substring(string, 0, nchars)
}

#' str_right
#' @description A description.
#' @param string A string
#' @export
str_right <- function(string, nchars){
  substring(string, nchar(string) - nchars + 1, nchar(string))
}

#' str_first_upper
#' @description A description.
#' @param string A string
#' @export
str_first_upper <- function(string){
  string <- tolower(string)
  paste(toupper(substring(string, 1, 1)),substring(string, 2, nchar(string)), sep = "")
}

#' str_pattern
#' @description A description.
#' @param string A string
#' @export
str_pattern <- function(string, pattern){
  require("stringr")
  string[str_detect(string, pattern)]
}

#' str_capitalize
#' @description A description.
#' @param string A string
#' @export
str_capitalize <- function(string){
  # http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string/6365349#6365349
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(string), perl = TRUE)
}

#' str_clean
#' @description A description.
#' @param string A string
#' @export
str_clean <- function(string,
                      remove.punct = TRUE,
                      remove.digit = TRUE,
                      remove.accent = TRUE,
                      trim = TRUE,
                      to.lower = TRUE){
  require("stringr")
  if (remove.punct)     string <- gsub("[[:punct:]]", "", string)
  if (remove.digit)     string <- gsub("[[:digit:]]", "", string)
  if (remove.accent)    string <- str_remove_tilde(string)
  if (to.lower)         string <- tolower(string)
  if (trim)             string <- str_trim(string)
  
  return(string)
}

#' str_is_email
#' @description A description.
#' @param string A string
#' @export
str_is_email <- function(string){
  email_pattern <- "^([a-zA-Z0-9]+[a-zA-Z0-9._%-]*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,4})$"
  grepl(email_pattern, string)
}