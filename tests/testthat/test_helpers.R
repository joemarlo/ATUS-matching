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

# define clusters
clus1 <- sample(1:5, 100, replace = T)
clus2 <- sample(1:5, 100, replace = T)
clus1_labels <- sort(unique(clus1))
clus2_labels <- sort(unique(clus2))
mapping <- sample(clus2_labels, length(clus1_labels), replace = FALSE) # defines the clusters from clus1 that match to clus2
cluster_two_relabeled <- swap_labels(clus1, clus2, mapping)

# TODO: double check everything
test_that("swap_labels() output is correct", {
  expect_vector(cluster_two_relabeled)
  expect_type(cluster_two_relabeled, 'integer')
  expect_true(sum(table(clus2, cluster_two_relabeled) > 0) == n_distinct(clus2))
  expect_true(sum(table(clus2, cluster_two_relabeled)) == length(clus2))
})

