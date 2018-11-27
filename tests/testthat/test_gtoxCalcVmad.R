context("Calculate Vmad")

test_that("gtoxCalcVmad:returns the correct value", {
    expect_true((gtoxCalcVmad(inputs = 10L) - 0.1827221) < 10e-6)
})

