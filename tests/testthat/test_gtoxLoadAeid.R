context("Check assay endpoint table")

test_that("gtoxLoadAeid:is a data.table", {
    tmp <- gtoxLoadAeid()
    expect_true("data.table"%in%class(tmp))
})

test_that('gtoxLoadAeid:contains "GSH content_Cell count_24h_dn"', {
    tmp <- gtoxLoadAeid()
    expect_true(any(tmp$aenm%in%"GSH content_Cell count_24h_dn"))
    })
