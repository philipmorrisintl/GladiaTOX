test_that("mc2_mthds:returns log10 among the list of mc2 functions", {
    tmp <- names(GladiaTOX:::mc2_mthds())
    expect_true("log10"%in%tmp)
})

