reifyObject <- function(object_o_1, sourceFile_s_1, sourcePackage_s_1) {
  getRObjectClassKind <-  function(object_) {
    if (!is.object(object_)) return(NA)
    if (isS4(object_)) {
      if (is(object_, 'refClass')) return('RC')
      return('S4')
    }
    on <- class(object_)
    if ('R6' %in% on) return('R6')
    if (is.environment(object_)) return('environment')
    if (typeof(object_) == 'list') return('S3')
    'unknown'
  }

  manageSingleStrings <- function(text_s) {
    ty <- typeof(text_s)
    if (ty == 'language') return(as.character(as.expression(text_s)))
    if (ty == 'integer') return(paste0(as.character(as.expression(text_s)), 'L'))
    if (length(text_s) != 1) return(text_s)
    if (!is.character(text_s[1])) return(text_s)
    paste0('"', gsub('"', '\\"', text_s, fixed = TRUE), '"')
  }

  normalizeSpaces <- function(text_s, manageSpecialCharacters_b = TRUE) {
    p <- if (manageSpecialCharacters_b) '[\\s\\b]' else '\\s'
    gsub(paste0('^', p, '+', '|', p, '+', '$'), '',
         gsub(paste0(p, '+'), ' ', text_s, perl = TRUE),
         perl = TRUE)
  }

  on <- getRObjectClassKind(object_o_1)
  if (is.na(on)) return(NA)

  classnames <- class(object_o_1)
  cn <- setdiff(classnames, c('environment', 'R6'))[1]

  fn <- guardExecution({get(cn)})
  if (on == 'R6') fn <- fn$new
  if (!is.function(fn))
    abort('unable to retrieve object signature for object', strBracket(cn))
  fo <- formals(fn)
  ag <- sapply(seq_len(length(fo)), function(k) {
    paste(names(fo[k]), ifelse(is.symbol(fo[[k]]), '', paste('=', manageSingleStrings(fo[[k]]))))
  }, simplify = FALSE)

  list(to_source = call('source', call('system.file', sourceFile_s_1, package = sourcePackage_s_1)),
       to_reify = if (length(ag) > 0)
         call(cn, unlist(normalizeSpaces(ag)))
       else
         call(cn)
  )
}
