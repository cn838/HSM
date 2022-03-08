

#test example data for the correct number of observed crashes in the before periods

testthat::test_that("Basic Input Data check", {
  expect_equal(sum(HSM::HSM_9_35(data = X9_10_seg, segment=TRUE, group = FALSE)[[9]]), 122)
  expect_equal(sum(HSM::HSM_9_35(data = X9_10_seg, segment=TRUE, group = FALSE)[["Observed Crashes in the After Period"]]), 30)
  print("Basic input data matches.")
})





testthat::test_that("values from Eq. 9A.1-11", {
  expect_equal(sum(HSM::HSM_9_35(data = X9_10_seg, segment=TRUE, group = FALSE)[["Variance (9A.1-11)"]]), 11.161 )
  print("the cumulative sum of the Variance terms match the expected output")
})
