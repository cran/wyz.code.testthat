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

  on <- getRObjectClassKind(object_o_1)
  if (is.na(on)) return(NA)

  classnames <- class(object_o_1)
  cn <- setdiff(classnames, c('environment', 'R6'))[1]

  fn <- guardExecution({get(cn, mode = 'function')})
  if (on == 'R6') fn <- fn$new
  #cat('class', strBracket(cn), 'classnames',
  #    paste(classnames, sep = '', collapse = ', '), '\n')
  if (!is.function(fn))
    abort('unable to retrieve object signature for object', strBracket(cn))
  fo <- formals(fn)

  list(to_source = call('source', call('system.file', sourceFile_s_1, package = sourcePackage_s_1)),
       to_reify = if (length(fo) > 0)
         call(cn, unlist(fo))
       else
         call(cn)
  )
}
