test_that("sc1:returns pval.zero among the list of sc1 functions", {
    expect_true("pval.zero"%in%names(GladiaTOX:::sc1_mthds()))
})

