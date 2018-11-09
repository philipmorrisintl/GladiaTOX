context("Check assay plate table")

test_that("tcplLoadApid:is a data.table", {
    tmp <- tcplLoadApid()
    expect_true("data.table"%in%class(tmp))
})

test_that("tcplLoadApid:contains a u_boxtrack:S-000049111", {
    tmp <- tcplLoadApid()
    expect_true(any(tmp$u_boxtrack%in%"S-000049111"))
    })
