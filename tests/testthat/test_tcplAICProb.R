test_that("tcplAICProb:returns the correct value", {
    tmp <- tcplAICProb(80, 85, 95)[[1]]
    expect_equal(tmp[[1]], 0.9236697)
})

