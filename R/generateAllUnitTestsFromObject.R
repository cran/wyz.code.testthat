gautfo <- generateAllUnitTestsFromObject <- function(object_o_1,
                                                     sourceFile_s_1,
                                                     sourcePackage_s_1,
                                                     targetFolder_s_1,
                                                     overwriteFile_b_1 = TRUE,
                                                     verbose_b_1 = FALSE) {
  if (!dir.exists(targetFolder_s_1))
    abort('target folder', strBracket(targetFolder_s_1),
          'does not exit. You must create it.')

  tcd <- copy(retrieveTestCaseDefinitions(object_o_1))
  frt <- copy(retrieveFunctionReturnTypes(object_o_1))
  cn <- setdiff(class(object_o_1), c('environment', 'R6'))[1]

  if (!is.data.table(tcd))
    return(paste('Class', strBracket(cn), 'apparently owns no test instrumentation.',
                 'No test created.'))

  if (!is.data.table(frt))
    return(paste('Class', strBracket(cn), 'apparently owns no function return type instrumentation.',
                 'No test created.'))

  uf <- unique(tcd$function_name)
  filenames <- file.path(targetFolder_s_1, paste0(cn, '-', uf))
  names(filenames) <- uf

  objectreification <- reifyObject(object_o_1, sourceFile_s_1, sourcePackage_s_1)
  src_uni <- list(
    objectreification$to_source,
    call('<-', quote(object_o_1), objectreification$to_reify)
  )

  function_name <- NULL # data.table NSE issue with Rcmd check
  testthatFactory_f_1 = testthatFactory()
  tcd[, `:=`(k = .I)]
  ut <- sapply(uf, function(fn) {
    ff <- tcd[function_name == fn]
    sapply(defineEvaluationModes()[c(1, 3)], function(de) {
      sapply(seq_len(nrow(ff)), function(k) {
        b <- de == defineEvaluationModes()[1]
        em <- if (b) {
          ff[k]$standard_evaluation
        } else  {
          ff[k]$type_checking_enforcement
        }

        dx <- call('do.call', object_o_1[[fn]], ff[k]$test_case[[1]]$getParams())
        dd <- call('EvaluationMode', de)
        df <- call('<-', ifelse(b, quote(emsre), quote(emtce)), dd)
        dc <- call('runTestCase', quote(object_o_1), ff[k]$k,
                   ifelse(b, quote(emsre), quote(emtce)))
        rtcname <- paste0('rtc', ifelse(b, 'sre', 'tce'), ff[k]$k)
        di <- call('<-', as.symbol(rtcname), dc)
        dz <- call('$', call('[[', call('[[', as.symbol(rtcname), 1), 1), 'value')

        syn <- call('$', as.symbol(rtcname), 'synthesis')
        st <- call('$', syn, 'status')

        cs <- if (em != 'failure') {
          list(st, call('$', syn, 'value_check'))
        } else {
          list(st,
               call('$', syn, 'execution_evaluation'),
               call('&&', call('$', syn, 'function_return_check'),
                    call('$', syn, 'parameter_check'))
          )
        }

        rtc <- testthatFactory_f_1(cs, em)
        list(
          pre = if (k == 1) c(df, di) else list(di),
          label = fn, #paste(fn, de, sep = ' - '),
          comment = paste(paste0('test ', ff[k]$k),
                          ff[k]$test_case[[1]]$getDescription(), em,
                          sep = ' - '),
          rtc = rtc)
      }, simplify = FALSE, USE.NAMES = FALSE)
    }, simplify = FALSE, USE.NAMES = FALSE)
  }, simplify = FALSE, USE.NAMES = FALSE)

  call2text <- function(z) as.character(as.expression(z))

  rv <- sapply(seq_len(length(ut)), function(k) {
    entries <- unlist(lapply(ut[[k]], function(l) {
      c(
        unlist(lapply(l, function(p) {
          sapply(p$pre, call2text)
        })),
        "", # for presentation purpose
        generateTestthatEnvelope(unlist(lapply(l, function(p) {
          c(paste('\n#', p$comment), call2text(p$rtc[[1]]),
            call2text(p$rtc[[2]]), '')
        })), l[[1]]$label)
      )
    }))
    entries <- c(unlist(lapply(src_uni, call2text)), entries)
    generateUnitTestFile(filenames[k], entries, overwriteFile_b_1, verbose_b_1)
  }, simplify = FALSE)
  list(class = cn, filenames = rbindlist(rv))
}
