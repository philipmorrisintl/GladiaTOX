test_that("tcplLoadAid:contains an anm:Cytotoxicity (TIER1)_4h of aid:1", {
    tmp <- tcplLoadAid()
    expect_equal(tmp[aid==3]$anm, "Cytotoxicity (TIER1)_4h")
})

