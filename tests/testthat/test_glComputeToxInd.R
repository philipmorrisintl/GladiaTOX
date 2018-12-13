test_that("glComputeToxInd:returns correct values", {
    options(warn=-1)
    tmp <- glComputeToxInd(asid=1L)[chnm=="crotonaldehyde"]
    expect_equal(tmp$V1, 0.64616077)
    options(warn=0)
})
