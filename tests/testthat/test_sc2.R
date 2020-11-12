test_that("sc2:returns pval.zero among the list of sc2 functions", {
    expect_true("bmad6"%in%names(GladiaTOX:::sc2_mthds()))
})

