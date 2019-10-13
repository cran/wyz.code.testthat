abort <- function(msg_s_1, ...) {
  stop(paste(msg_s_1, ...))
}

catn <- function(...) cat(..., '\n')

strBracket <- function(text_s_n) {
  paste0('[', text_s_n, ']')
}

guardExecution <- function(yourExpression_ex, instrumentWarnings_b = TRUE) {
  if (instrumentWarnings_b) {
    tryCatch(yourExpression_ex,
             error = function(e) e,
             warning = function(w) w)
  } else {
    tryCatch(yourExpression_ex,
             error = function(e) e)
  }
}
