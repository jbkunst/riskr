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

#' @export
str_left <- function(string, nchars){
  substring(string, 0, nchars)
}

#' @export
str_right <- function(string, nchars){
  substring(string, nchar(string) - nchars + 1, nchar(string))
}

#' @export
str_first_upper <- function(string){
  string <- tolower(string)
  paste(toupper(substring(string, 1, 1)),substring(string, 2, nchar(string)), sep = "")
}

#' @export
str_pattern <- function(string, pattern){
  require("stringr")
  string[str_detect(string, pattern)]
}

#' @export
str_capitalize <- function(string){
  # http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string/6365349#6365349
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(string), perl = TRUE)
}

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

#' @export
str_is_email <- function(string){
  email_pattern <- "^([a-zA-Z0-9]+[a-zA-Z0-9._%-]*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,4})$"
  grepl(email_pattern, string)
}

#' @export
str_rm_tilde <- function(string){
  string <- gsub("Ä|Á", "A", gsub("Ë|É", "E", gsub("Ï|Í", "I", gsub("Ö|Ó", "O", gsub("Ü|Ú", "U", string)))))
  string <- gsub("ä|á", "a", gsub("ë|é", "e", gsub("ï|í", "i", gsub("ö|ó", "o", gsub("ü|ú", "u", string)))))
  string <- gsub("Ñ", "N", gsub("ñ", "n", string))
  string
}