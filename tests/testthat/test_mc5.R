test_that("mc5:returns bmad5 among the list of mc2 functions", {
    expect_true("bmad5"%in%names(GladiaTOX:::mc5_mthds()))
})

