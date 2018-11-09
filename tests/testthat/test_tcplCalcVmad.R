context("Calculate Vmad")

test_that("tcplCalcVmad:returns the correct value", {
    expect_true((tcplCalcVmad(inputs = 10L) - 0.1827221) < 10e-6)
})

