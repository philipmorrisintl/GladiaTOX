test_that("gtoxLoadAsid:return a data.table with a registered study", {
    tmp <- gtoxLoadAsid()
    expect_equal(class(tmp), c('data.table','data.frame'))
    expect_equal(nrow(tmp), 1)
    expect_equal(tmp$asnm, "SampleStudy")
})

