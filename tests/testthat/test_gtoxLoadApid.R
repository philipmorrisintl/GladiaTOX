context("Check assay plate table")

test_that("gtoxLoadApid:is a data.table", {
    tcplConfDefault()
    tmp <- gtoxLoadApid()
    expect_true("data.table"%in%class(tmp))
})

test_that("gtoxLoadApid:contains a u_boxtrack:S-000049111", {
    tcplConfDefault()
    tmp <- gtoxLoadApid()
    expect_true(any(tmp$u_boxtrack%in%"S-000049111"))
    })
