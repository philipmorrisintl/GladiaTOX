test_that("assignDefaultMthds:returns a warning when applied on an not existing 
          study", {
              expect_warning(assignDefaultMthds(asid=2),
                             "Error updating the following ids: NA")
})
