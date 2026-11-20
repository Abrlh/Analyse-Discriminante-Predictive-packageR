test_that("check_X accepts valid data and rejects invalid inputs", {
  expect_silent(check_X(iris))
  expect_error(check_X(as.matrix(iris[, 1:4])), "must be a data.frame")
  expect_error(check_X(iris[0, ]), "has no rows")
  expect_error(check_X(data.frame(d = Sys.Date())), "Unsupported column type")
})

test_that("check_y validates the target variable", {
  expect_silent(check_y(iris$Species, iris))
  expect_error(check_y(as.character(iris$Species), iris), "must be a factor")
  expect_error(check_y(iris$Species[1:100], iris), "same length")
  y_na <- iris$Species; y_na[c(1, 5)] <- NA
  expect_error(check_y(y_na, iris), "missing value")
  expect_error(check_y(iris$Species[1:50], iris[1:50, ]), "at least 2")
})

test_that("check_class_sizes warns about small classes", {
  expect_silent(check_class_sizes(iris$Species))
  y_small <- factor(c(rep("A", 10), "B", rep("C", 5)))
  expect_warning(check_class_sizes(y_small), "B \\(n = 1\\)")
})

test_that("check_dimensions detects n - K < p", {
  expect_silent(check_dimensions(n = 150, p = 4, K = 3))
  expect_error(check_dimensions(n = 10, p = 20, K = 2), "Not enough observations")
})

test_that("check_constant detects constant columns", {
  expect_silent(check_constant(iris))
  df <- data.frame(a = 1:5, b = rep(3, 5), c = rep("x", 5))
  expect_error(check_constant(df), "b, c")
  expect_error(check_constant(data.frame(a = 1:3, b = c(5, 5, NA))), "constant: b")
})

test_that("check_fitted and check_compatible protect predict", {
  expect_silent(check_fitted(TRUE))
  expect_error(check_fitted(FALSE), "not fitted")
  kinds <- c(Sepal.Length = "numeric", Species = "categorical")
  expect_silent(check_compatible(iris, kinds))
  expect_error(check_compatible(iris[, 1:4], kinds), "missing from")
  iris2 <- iris; iris2$Sepal.Length <- as.character(iris2$Sepal.Length)
  expect_error(check_compatible(iris2, kinds), "same type")
})