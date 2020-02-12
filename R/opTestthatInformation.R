opTestthatInformation <- function() {

  stratum <- buildIdentityList(c('core', paste0('layer_', 1:3)))
  phasing <- buildIdentityList(c('design', 'build', 'test', 'run', 'maintain', 'evolve', 'transversal'))
  intent <- buildIdentityList(c('parts_building', 'parts_assembly', 'quality_control', 'statistics', 'feedback',
                                'content_generation', 'utilities'))
  category <- buildIdentityList(c('function', 'class', 'data'))
  nature <- buildIdentityList(c('exported', 'internal'))

  buildList <- function(name_s_1, category_s_1, nature_s_1,
                        stratum_s_1, phasing_s_1, intent_s_1) {
    list(name = name_s_1, category = category_s_1,
         nature = nature_s_1, stratum = stratum_s_1, phasing = phasing_s_1,
         intent = intent_s_1
    )
  }

  dt <- data.table::rbindlist(list(
    buildList("generateAllUnitTestsFromObject", category$FUNCTION, nature$EXPORTED,
              stratum$CORE, phasing$BUILD, intent$PARTS_ASSEMBLY),
    buildList("gautfo", category$FUNCTION, nature$EXPORTED,
              stratum$CORE, phasing$BUILD, intent$PARTS_ASSEMBLY),
    buildList("generateTestthatEnvelope", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_2, phasing$RUN, intent$PARTS_ASSEMBLY),
    buildList("generateUnitTestFile", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$PARTS_ASSEMBLY),
    buildList("reifyObject", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("testthatFactory", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("transformValuesInCode", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("opTestthatInformation",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$FEEDBACK)
  ))

  name <- NULL # nse
  dt[order(name)]
}