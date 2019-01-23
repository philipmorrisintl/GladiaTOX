test_that("assignDefaultMthds:returns a warning when applied on an not existing 
          study", {
              tcplConfDefault()
              expect_warning(assignDefaultMthds(asid=2),
                             "Error updating the following ids: NA")
})
