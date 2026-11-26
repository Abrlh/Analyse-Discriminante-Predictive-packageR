test_that("train_test_split returns samples of the right size", {
  sp <- train_test_split(iris[, 1:4], iris$Species, test_size = 0.3, seed = 123)
  expect_equal(nrow(sp$X_test), 45)
  expect_equal(nrow(sp$X_train), 105)
  expect_length(sp$y_test, 45)
  expect_length(sp$y_train, 105)
})

test_that("the split is stratified by default", {
  sp <- train_test_split(iris[, 1:4], iris$Species, test_size = 0.3, seed = 123)
  expect_equal(as.vector(table(sp$y_test)), c(15, 15, 15))
})

test_that("training and test samples are disjoint and cover all rows", {
  sp <- train_test_split(iris[, 1:4], iris$Species, seed = 1)
  train_rows <- setdiff(seq_len(150), sp$test_index)
  expect_length(intersect(train_rows, sp$test_index), 0)
  expect_equal(sort(c(train_rows, sp$test_index)), 1:150)
  expect_equal(sp$X_test, iris[sp$test_index, 1:4])
})

test_that("the same seed gives the same split", {
  sp1 <- train_test_split(iris[, 1:4], iris$Species, seed = 42)
  sp2 <- train_test_split(iris[, 1:4], iris$Species, seed = 42)
  sp3 <- train_test_split(iris[, 1:4], iris$Species, seed = 43)
  expect_equal(sp1$test_index, sp2$test_index)
  expect_false(identical(sp1$test_index, sp3$test_index))
})

test_that("a non-stratified split also works", {
  sp <- train_test_split(iris[, 1:4], iris$Species, test_size = 0.3,
                         seed = 123, stratify = FALSE)
  expect_equal(nrow(sp$X_test), 45)
})

test_that("invalid inputs are rejected", {
  expect_error(train_test_split(iris[, 1:4], iris$Species, test_size = 0), "strictly between")
  expect_error(train_test_split(iris[, 1:4], iris$Species, test_size = 1.5), "strictly between")
  expect_error(train_test_split(iris[, 1:4], as.character(iris$Species)), "must be a factor")
  # 1 individu par classe : 10 % de 1 arrondi donne 0, l'échantillon test serait vide
  expect_error(train_test_split(iris[c(1, 51), 1:4], droplevels(iris$Species[c(1, 51)]),
                                test_size = 0.1), "empty")
})
