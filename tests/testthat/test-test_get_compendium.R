test_that("getting compendium with defaults works as expected", {
    skip_on_ci()
    cpd <- getCompendium()
    expect_s4_class(cpd, "TreeSummarizedExperiment")
    expect_gt(nrow(cpd), 1000)
    expect_gt(ncol(cpd), 1000)
    expect_contains(assayNames(cpd), "counts")
    # v1.0.1 corrected the name of this taxon:
    expect_equal(max(counts(cpd)['Bacteria.Firmicutes.Clostridia.Eubacteriales.(unclassified).Alkalibaculum',]), 16)
    expect_error(max(counts(cpd)['Bacteria.Firmicutes.Clostridia.Eubacteriales.Alkalibaculum.NA',]))
})

test_that("getting compendium with specified version works as expected", {
    skip_on_ci()
    cpd <- getCompendium('1.0.0')
    expect_s4_class(cpd, "TreeSummarizedExperiment")
    expect_gt(nrow(cpd), 1000)
    expect_gt(ncol(cpd), 1000)
    expect_contains(assayNames(cpd), "counts")
    expect_equal(max(counts(cpd)['Bacteria.Firmicutes.Clostridia.Eubacteriales.Alkalibaculum.NA',]), 16)
    expect_error(max(counts(cpd)['Bacteria.Firmicutes.Clostridia.Eubacteriales.(unclassified).Alkalibaculum',]))
})
