# testthat::test_dir("tests/testthat")

mat <- matrix(rnorm(25), ncol = 5)
dist1 <- get_distance(c(1,3,2,5,4), mat)
dist2 <- get_distance(c(1,3,2,5,5), mat)
optimal_indices <- minimize_distance(mat)

test_that("get_distance() output is correct", {
  expect_type(dist1, 'double')
  expect_true(dist2 > 1e5)
})

test_that("minimize_distance() output is correct", {
  expect_vector(optimal_indices)
  expect_type(optimal_indices, 'double')
  expect_equal(0, sum(is.na(optimal_indices)))
  expect_equal(0, sum(duplicated(optimal_indices)))
})
