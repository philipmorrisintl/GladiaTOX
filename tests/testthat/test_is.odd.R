test_that("is.odd:has the expected behavior", {
    tmp <- is.odd(2)
    expect_false(tmp)
    tmp <- is.odd(3)
    expect_true(tmp)
})

