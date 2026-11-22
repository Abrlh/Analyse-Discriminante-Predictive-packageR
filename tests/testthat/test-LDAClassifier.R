test_that("constructor sets defaults and rejects invalid values", {
  m <- LDAClassifier$new()
  expect_equal(m$method, "classic")
  expect_equal(m$selection, "none")
  expect_false(m$is_fitted)
  expect_error(LDAClassifier$new(method = "toto"))
  expect_error(LDAClassifier$new(selection = "toto"))
})

test_that("print works before fit and predict fails before fit", {
  m <- LDAClassifier$new()
  expect_output(print(m), "not fitted yet")
  expect_error(m$predict(iris), "not fitted")
})

test_that("fit stops at the LDA step while LDA_Classic is not implemented", {
  m <- LDAClassifier$new()
  expect_error(m$fit(iris[, 1:4], iris$Species), "not implemented")
  expect_false(m$is_fitted)
})

test_that("fit, predict and predict_proba orchestrate the pipeline", {
  with_fake_lda({
    m <- LDAClassifier$new()
    expect_invisible(m$fit(iris[, 1:4], iris$Species))
    expect_true(m$is_fitted)
    expect_equal(m$classes, levels(iris$Species))
    expect_equal(m$selected_vars, names(iris)[1:4])

    pred <- m$predict(iris)
    expect_s3_class(pred, "factor")
    expect_length(pred, 150)
    expect_equal(levels(pred), levels(iris$Species))

    P <- m$predict_proba(iris)
    expect_equal(dim(P), c(150, 3))
    expect_equal(colnames(P), levels(iris$Species))
    expect_equal(unname(rowSums(P)), rep(1, 150))

    # La classe prédite est celle de probabilité maximale
    expect_equal(as.character(pred), colnames(P)[max.col(P)])

    expect_output(print(m), "fitted on 150")
  })
})

test_that("predictions keep the size of X when rows contain NA", {
  with_fake_lda({
    m <- LDAClassifier$new()$fit(iris[, 1:4], iris$Species)
    new <- iris[1:5, 1:4]
    new$Sepal.Length[2] <- NA

    expect_warning(pred <- m$predict(new), "missing")
    expect_length(pred, 5)
    expect_true(is.na(pred[2]))
    expect_false(anyNA(pred[-2]))

    expect_warning(P <- m$predict_proba(new), "missing")
    expect_true(all(is.na(P[2, ])))
  })
})

test_that("categorical predictors go through the whole pipeline", {
  with_fake_lda({
    df <- iris[, 1:4]
    df$size <- ifelse(iris$Petal.Length > 4, "big", "small")
    m <- LDAClassifier$new()$fit(df, iris$Species)
    expect_true("size_small" %in% m$selected_vars)
    expect_length(m$predict(df), 150)
  })
})
