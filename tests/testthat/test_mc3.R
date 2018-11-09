test_that("mc3:returns resp.log2 among the list of mc2 functions", {
    expect_true("resp.log2"%in%names(mc3_mthds()))
})

