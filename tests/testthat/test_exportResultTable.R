test_that("exportResultTable:exports result file", {
    outfile <- tempfile()
    expect_false(file.exists(outfile))
    exportResultTable(asid=1L, stats=c("modl_acc", "modl_ga"), outfile=outfile)
    expect_true(file.exists(outfile))
})
