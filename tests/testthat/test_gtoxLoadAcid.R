context("Check assay component table")

test_that("gtoxLoadAcid:is a data.table", {
    tmp <- gtoxLoadAcid()
    expect_true("data.table"%in%class(tmp))
})

test_that('gtoxLoadAcid:contains "GSH content_Cell count_24h"', {
    tmp <- gtoxLoadAcid()
    expect_true(any(tmp$acnm%in%"GSH content_Cell count_24h"))
    })
