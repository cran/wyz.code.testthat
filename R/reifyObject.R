reifyObject <- function(object_o_1, sourceFile_s_1, sourcePackage_s_1) {
  on <- getObjectClassKind(object_o_1)
  if (is.na(on)) return(NA)

  classnames <- class(object_o_1)
  cn <- setdiff(classnames, c('environment', 'R6'))[1]

  fn <- guardExecution({get(cn)}) # beware R6 constructor is not a function
  # but an environment, so do not use mode = 'function' with get here.
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
