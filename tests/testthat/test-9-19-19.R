test_that("Predicted average crashes frequency in before period works", {
  expect_equal(sum(HSM::HSM_9_35(data = X9_10_seg, segment=TRUE, group = FALSE)[[12]])
, 96.19)
})


