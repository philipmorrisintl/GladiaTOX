context("Check assay chemical table")

test_that("tcplLoadChem:is a data.table", {
    tmp <- tcplLoadChem()
    expect_true("data.table"%in%class(tmp))
})

test_that('tcplLoadChem:contains "chromium"', {
    tmp <- tcplLoadChem()
    expect_true(any(tmp$chnm%in%"chromium"))
    })

test_that('tcplLoadChem:returns unique elements when include.spid parameter is 
          set to FALSE', {
    tmp <- tcplLoadChem(include.spid=FALSE)
    expect_false(any(duplicated(tmp)))
})
