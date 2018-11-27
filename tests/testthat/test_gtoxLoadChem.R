context("Check assay chemical table")

test_that("gtoxLoadChem:is a data.table", {
    tmp <- gtoxLoadChem()
    expect_true("data.table"%in%class(tmp))
})

test_that('gtoxLoadChem:contains "chromium"', {
    tmp <- gtoxLoadChem()
    expect_true(any(tmp$chnm%in%"chromium"))
    })

test_that('gtoxLoadChem:returns unique elements when include.spid parameter is 
          set to FALSE', {
    tmp <- gtoxLoadChem(include.spid=FALSE)
    expect_false(any(duplicated(tmp)))
})
