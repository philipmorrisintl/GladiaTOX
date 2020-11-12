test_that("is.odd:has the expected behavior", {
    tmp <- GladiaTOX:::is.odd(2)
    expect_false(tmp)
    tmp <- GladiaTOX:::is.odd(3)
    expect_true(tmp)
})

