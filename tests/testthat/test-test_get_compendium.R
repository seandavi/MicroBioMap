test_that("getting compendium works as expected", {
  cpd = get_compendium()
  expect_s4_class(cpd, 'TreeSummarizedExperiment')
  expect_gt(nrow(cpd),1000)
  expect_gt(ncol(cpd),1000)
  expect_contains(assayNames(cpd), 'counts')
})
