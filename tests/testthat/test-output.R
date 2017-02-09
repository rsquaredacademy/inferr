context('output')

test_that('output from runs_test matches the expected output', {

  reg <- lm(mpg ~ disp, data = mtcars)
  actual <- runs_test(residuals(reg))
  expected <- cat("Runs Test\n Total Cases:  32 \n Test Value :  -0.9630856 \n
  Cases < Test Value:  16 \n Cases > Test Value:  16 \n Number of Runs:  11 \n
  Expected Runs:  17 \n Variance (Runs):  7.741935 \n z Statistic:  -2.156386 \n
   p-value:  0.03105355 ")
  expect_output(print(actual), expected)

})
