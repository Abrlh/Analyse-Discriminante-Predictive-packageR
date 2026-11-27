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

test_that("the real LDA gives correct results on iris", {
  m <- LDAClassifier$new()
  m$fit(iris[, 1:4], iris$Species)
  expect_true(m$is_fitted)

  # Une LDA sur iris classe correctement 98 % des individus
  expect_equal(m$score(iris[, 1:4], iris$Species), 0.98)

  # Probabilités : une ligne par individu, chaque ligne somme à 1
  P <- m$predict_proba(iris[1:10, 1:4])
  expect_equal(dim(P), c(10, 3))
  expect_equal(unname(rowSums(P)), rep(1, 10))
})

test_that("selection thresholds are passed to the selection", {
  strict <- LDAClassifier$new(selection = "forward", slentry = 1e-7)
  strict$fit(iris[, 1:4], iris$Species)
  expect_lt(length(strict$selected_vars), 4)
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

test_that("evaluate returns the confusion matrix and accuracy on new data", {
  with_fake_lda({
    set.seed(1)
    test_idx <- sample(150, 45)
    m <- LDAClassifier$new()$fit(iris[-test_idx, 1:4], iris$Species[-test_idx])
    ev <- m$evaluate(iris[test_idx, ], iris$Species[test_idx])

    expect_equal(dim(ev$confusion), c(3, 3))
    expect_equal(sum(ev$confusion), 45)
    expect_equal(ev$accuracy, sum(diag(ev$confusion)) / 45)
    expect_equal(m$score(iris[test_idx, ], iris$Species[test_idx]), ev$accuracy)
  })
})

test_that("a categorical variable is kept as a whole after selection", {
  df <- iris
  df$taille <- cut(df$Sepal.Width, 3, labels = c("petite", "moyenne", "grande"))
  m <- LDAClassifier$new(selection = "stepwise")
  m$fit(df[, c(1, 3, 4, 6)], df$Species)

  priv <- m$.__enclos_env__$private
  out <- priv$complete_dummies(c("Petal.Length", "taille_grande"))
  expect_setequal(out, c("Petal.Length", "taille_moyenne", "taille_grande"))
})

test_that("Wilks' lambda and partial F tests match the reference values on iris", {
  m <- LDAClassifier$new()
  m$fit(iris[, 1:4], iris$Species)

  w <- m$wilks
  expect_equal(w$lambda, 0.02344, tolerance = 1e-3)
  expect_equal(w$F, 199.15, tolerance = 1e-3)
  expect_equal(c(w$df1, w$df2), c(8, 288))

  vt <- m$variable_tests
  expect_equal(vt$F, c(4.72, 21.94, 35.59, 24.90), tolerance = 1e-3)
})