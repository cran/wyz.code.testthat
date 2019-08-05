transformValuesInCode <- function(values_) {
  gsub('^\\s*list\\(\\s*(.*?)\\s*\\)\\s*$', '\\1', deparse(substitute(values_)), perl = TRUE)
}