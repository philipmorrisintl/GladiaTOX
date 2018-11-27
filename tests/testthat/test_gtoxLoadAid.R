test_that("gtoxLoadAid:contains an anm:Cytotoxicity (TIER1)_4h of aid:1", {
    tmp <- gtoxLoadAid()
    expect_equal(tmp[aid==3]$anm, "Cytotoxicity (TIER1)_4h")
})

