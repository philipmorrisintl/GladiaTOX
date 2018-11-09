context("Check assay component table")

test_that("tcplLoadAcid:is a data.table", {
    tmp <- tcplLoadAcid()
    expect_true("data.table"%in%class(tmp))
})

test_that('tcplLoadAcid:contains "GSH content_Cell count_24h"', {
    tmp <- tcplLoadAcid()
    expect_true(any(tmp$acnm%in%"GSH content_Cell count_24h"))
    })
