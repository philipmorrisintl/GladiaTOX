context("Check assay well table")

test_that("gtoxLoadWaid:is a data.table", {
    tmp <- gtoxLoadWaid()
    expect_true("data.table"%in%class(tmp))
})

test_that('gtoxLoadWaid:has 19 columns"', {
    tmp <- gtoxLoadWaid()
    expect_equal(ncol(tmp), 19)
    })
