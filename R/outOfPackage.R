abort <- function(msg_s_1, ...) {
  stop(paste(msg_s_1, ...))
}

catn <- function(...) cat(..., '\n')

strBracket <- function(text_s_n) {
  paste0('[', text_s_n, ']')
}
