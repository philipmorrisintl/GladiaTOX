context("getsplit")

test_that("getsplit:split word correctly", {
    expect_equal(GladiaTOX:::getsplit("toto_tata", "_", 1), "toto")
})

