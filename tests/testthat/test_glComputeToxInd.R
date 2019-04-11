test_that("glComputeToxInd:returns correct values", {
    options(warn=-1)
    tmp <- glComputeToxInd(asid=1L)[chnm=="acrylamide"]
    expect_equal(tmp$V1, 0.2048870242)
    options(warn=0)
})
