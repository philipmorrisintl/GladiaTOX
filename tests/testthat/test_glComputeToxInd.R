test_that("glComputeToxInd:returns correct values", {
    tcplConfDefault()
    options(warn=-1)
    tmp <- glComputeToxInd(asid=1L)
    expect_equal(nrow(tmp), 8)
    options(warn=0)
})
