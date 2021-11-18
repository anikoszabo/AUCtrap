test_that("AUCtrap inputs are checked", {
  expect_error(AUCtrap(0:3, c(3,4,1,2), method="xyz"))
  expect_error(AUCtrap(0:4, c(3,4,1,2), method="AUC"))
})

test_that("Total AUC calculation works", {
  ex <- AUCtrap(0:3, c(3,4,1,2), method="AUC")
  expect_equal(ex$value, 7.5)
  expect_equal(ex$method, "AUC")
  expect_equal(AUCtrap(0:3, c(3,4,1,2)), ex)
  ex2 <- AUCtrap(rev(0:3), rev(c(3,4,1,2)), method="AUC")
  expect_equal(ex, ex2)
})

test_that("Methods for 'auctrap' class work",{
  ex <- AUCtrap(0:3, c(3,4,1,2), method="AUC")
  expect_output(print(ex, digits=3), "7.5 using method AUC")
})
