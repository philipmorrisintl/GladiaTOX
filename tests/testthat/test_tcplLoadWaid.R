context("Check assay well table")

test_that("tcplLoadWaid:is a data.table", {
    tmp <- tcplLoadWaid()
    expect_true("data.table"%in%class(tmp))
})

test_that('tcplLoadWaid:has 19 columns"', {
    tmp <- tcplLoadWaid()
    expect_equal(ncol(tmp), 19)
    })
