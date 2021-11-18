test_that("AUCtrap inputs are checked", {
  expect_error(AUCtrap(0:3, c(3,4,1,2), method="xyz"))
  expect_error(AUCtrap(0:4, c(3,4,1,2), method="AUC"))
})

test_that("Total AUC calculation works", {
  expect_equal(AUCtrap(0:3, c(3,4,1,2), method="AUC"), 7.5)
  expect_equal(AUCtrap(0:3, c(3,4,1,2)), 7.5)
})
