test_that("gtoxAICProb:returns the correct value", {
    tmp <- gtoxAICProb(80, 85, 95)[[1]]
    expect_equal(tmp[[1]], 0.9236697)
})

