context("Check assay endpoint table")

test_that("tcplLoadAeid:is a data.table", {
    tmp <- tcplLoadAeid()
    expect_true("data.table"%in%class(tmp))
})

test_that('tcplLoadAeid:contains "GSH content_Cell count_24h_dn"', {
    tmp <- tcplLoadAeid()
    expect_true(any(tmp$aenm%in%"GSH content_Cell count_24h_dn"))
    })
