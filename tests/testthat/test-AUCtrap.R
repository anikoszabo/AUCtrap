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

test_that("Formula method works",{
  dd <- data.frame(Time = 0:4, Value = c(3,4,1,2,10), X = rnorm(5))
  ex2 <- AUCtrap(Value ~ Time, data=dd, subset=Time < 4)
  ex0 <- AUCtrap(x=0:3, y=c(3,4,1,2))
  expect_equal(ex2, ex0)
  expect_error(AUCtrap(Value ~ Time + X, data=dd))
  expect_error(AUCtrap(Value ~ as.character(Time), data=dd))
})

test_that("Methods for 'auctrap' class work",{
  ex <- AUCtrap(0:3, c(3,4,1,2), method="AUC")
  expect_output(print(ex, digits=3), "7.5 using method AUC")
})
