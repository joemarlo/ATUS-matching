# testthat::test_dir("tests/testthat")
# source("analyses/helpers_analyses.R")
set.seed(44)

mat <- matrix(rnorm(25), ncol = 5)
dist1 <- get_distance(c(1,3,2,5,4), mat)
dist2 <- get_distance(c(1,3,2,5,5), mat)

test_that("get_distance() output is correct", {
  expect_type(dist1, 'double')
  expect_true(dist2 > 1e5)
})

optimal_indices_1 <- minimal_distance(mat)
mat_n_m <- matrix(rnorm(20), ncol = 4)
optimal_indices_2 <- minimal_distance(mat_n_m)
mat_m_n <- matrix(rnorm(20), ncol = 5)
optimal_indices_3 <- minimal_distance(mat_m_n)
mat_m_n_2 <- matrix(rnorm(24), ncol = 6)
optimal_indices_4 <- minimal_distance(mat_m_n_2)

# note: these tests do not account for duplicate matching when k_t1=k_t2
test_that("minimal_distance() output is correct", {
  # k_t1 == k_t2
  expect_vector(optimal_indices_1)
  expect_type(optimal_indices_1, 'double')
  expect_equal(0, sum(is.na(optimal_indices_1)))
  expect_equal(0, sum(duplicated(optimal_indices_1)))
  
  # k_t1 < k_t2
  expect_vector(optimal_indices_2)
  expect_type(optimal_indices_2, 'double')
  expect_equal(0, sum(is.na(optimal_indices_2)))
  expect_equal(0, sum(duplicated(optimal_indices_2)))
  
  # k_t1 > k_t2
  expect_vector(optimal_indices_3)
  expect_type(optimal_indices_3, 'double')
  expect_equal(1, sum(is.na(optimal_indices_3)))
  expect_equal(0, sum(duplicated(optimal_indices_3)))
  
  expect_vector(optimal_indices_4)
  expect_type(optimal_indices_4, 'double')
  expect_equal(2, sum(is.na(optimal_indices_4)))
  expect_equal(1, sum(duplicated(optimal_indices_4)))
})

# define clusters
clus1 <- sample(1:5, 100, replace = T)
clus2 <- sample(1:5, 100, replace = T)
clus1_labels <- sort(unique(clus1))
clus2_labels <- sort(unique(clus2))
mapping <- sample(clus2_labels, length(clus1_labels), replace = FALSE) # defines the clusters from clus1 that match to clus2
cluster_two_relabeled <- swap_labels(clus1, clus2, mapping)

# simulating duplicates
clus2_2 <- sample(1:4, 100, replace = T)
mapping[4] <- NA
cluster_two_relabeled_2 <- swap_labels(clus1, clus2_2, mapping)

# ISSUE here where swap_labels() is not correctly assigning cluster ids when there are duplicates

test_that("swap_labels() output is correct", {
  expect_vector(cluster_two_relabeled)
  expect_type(cluster_two_relabeled, 'integer')
  expect_true(sum(table(clus2, cluster_two_relabeled) > 0) == n_distinct(clus2))
  expect_true(sum(table(clus2, cluster_two_relabeled)) == length(clus2))
  
  expect_equal(n_distinct(clus2_2), n_distinct(cluster_two_relabeled_2))
  expect_equal(sum(!is.na(clus2_2)), sum(!is.na(cluster_two_relabeled_2)))
})

