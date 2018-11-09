test_that("clComputeToxInd:returns correct values", {
    tmp <- glComputeToxInd(asid=1L)[chnm=="crotonaldehyde"]
    expect_equal(tmp$V1, 0.64616077)
})
