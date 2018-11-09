test_that("mc6:returns pintool among the list of mc2 functions", {
    expect_true("pintool"%in%names(mc6_mthds()))
})

