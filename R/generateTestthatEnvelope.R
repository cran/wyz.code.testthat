generateTestthatEnvelope <- function(entries_s, label_s_1 = 'some tests') {

  c(
    paste0("test_that('", label_s_1, "', {"),
    entries_s,
    '})\n'
  )
}

