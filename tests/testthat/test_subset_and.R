testthat::context("subset_and")

testthat::test_that("subset_and produces correct results", {
  sl1 <- c(TRUE, FALSE, TRUE, FALSE)
  sl2 <- c(TRUE, FALSE, FALSE, TRUE)
  sl <- sl1 & sl2

  si1 <- which(sl1)
  si2 <- which(sl2)
  si <- as.integer(intersect(si1, si2))

  testthat::expect_identical(subset_and(sl1, sl2), sl)
  testthat::expect_identical(subset_and(si1, si2), si)
  testthat::expect_identical(subset_and(si1, sl2), si)
  testthat::expect_identical(subset_and(sl1, si2), si)
  testthat::expect_identical(subset_and(NULL, NULL), NULL)
  testthat::expect_identical(subset_and(NULL, si2), si2)
  testthat::expect_identical(subset_and(si1, NULL), si1)
  testthat::expect_identical(subset_and(NULL, sl2), sl2)
  testthat::expect_identical(subset_and(sl1, NULL), sl1)
})


