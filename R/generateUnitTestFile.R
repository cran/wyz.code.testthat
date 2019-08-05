generateUnitTestFile <- function(filename_s_1, content_s,
                                 overwrite_b_1 = FALSE,
                                 verbose_b_1 = TRUE) {

  applyFilenameConvention <- function() {
    desired_suffix <- '.R'
    desired_prefix <- 'test_'
    bn <- basename(filename_s_1)
    dn <- dirname(filename_s_1)
    good_prefix <- grepl(paste0('^', desired_prefix), bn, perl = TRUE)
    good_suffix <- grepl(paste0(desired_suffix, '$'), bn, perl = TRUE)
    if (good_prefix && good_suffix) return(filename_s_1)
    if (good_prefix) return(file.path(dn, paste0(bn, desired_suffix)))
    if (good_suffix) return(file.path(dn, paste0(desired_prefix, bn)))
    file.path(dn, paste0(desired_prefix, bn, desired_suffix))
  }

  fn <- applyFilenameConvention()
  b <- overwrite_b_1 || !file.exists(fn)
  if (b) {
    writeLines(content_s, con = fn)
    if (verbose_b_1) catn('wrote file', fn)
  }
  list(filename = fn, overwritten = b)
}
