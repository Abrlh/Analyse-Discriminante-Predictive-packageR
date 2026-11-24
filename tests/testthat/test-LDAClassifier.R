test_that("constructor sets defaults and rejects invalid values", {
  m <- LDAClassifier$new()
  expect_equal(m$method, "classic")
  expect_equal(m$selection, "none")
  expect_false(m$is_fitted)
  expect_error(LDAClassifier$new(method = "toto"))
  expect_error(LDAClassifier$new(selection = "toto"))
})

test_that("methods behave correctly before fit", {
  m <- LDAClassifier$new()
  expect_output(print(m), "not fitted yet")
  expect_error(m$predict(iris), "not fitted")
  expect_error(m$summary(), "not fitted")

  # Les propriétés renvoient NULL avant le fit, sans planter
  expect_null(m$confusion)
  expect_null(m$accuracy)
  expect_null(m$coefficients)
})

test_that("fit gives a clear error while the LDA classes are not available", {
  m <- LDAClassifier$new()
  expect_error(m$fit(iris[, 1:4], iris$Species), "not available yet")
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

test_that("fit computes the confusion matrix and accuracy, score and summary work", {
  with_fake_lda({
    m <- LDAClassifier$new()$fit(iris[, 1:4], iris$Species)

    # Matrice 3 x 3 contenant les 150 individus
    expect_equal(dim(m$confusion), c(3, 3))
    expect_equal(sum(m$confusion), 150)

    # Le taux est la proportion de la diagonale (bonnes prédictions)
    expect_equal(m$accuracy, sum(diag(m$confusion)) / 150)

    # score() sur les mêmes données redonne le taux d'apprentissage
    expect_equal(m$score(iris, iris$Species), m$accuracy)

    # summary() affiche le résumé
    expect_output(m$summary(), "Confusion matrix")

    # Lecture seule
    expect_error(m$accuracy <- 1, "read-only")
  })
})