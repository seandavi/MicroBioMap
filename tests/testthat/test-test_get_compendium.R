test_that("getting compendium works as expected", {
    skip_on_ci()
    cpd <- getCompendium()
    expect_s4_class(cpd, "TreeSummarizedExperiment")
    expect_gt(nrow(cpd), 1000)
    expect_gt(ncol(cpd), 1000)
    expect_contains(assayNames(cpd), "counts")
})
