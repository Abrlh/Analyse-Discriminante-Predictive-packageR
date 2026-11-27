df_train <- data.frame(
  age     = c(25, 40, 31, NA, 52),
  couleur = c("bleu", "vert", "rouge", "bleu", "vert")
)

test_that("fit_transform removes incomplete rows and encodes categoricals", {
  d <- Preprocessor$new()
  expect_warning(res <- d$fit_transform(df_train), "1 row")
  expect_equal(res$rows, c(1, 2, 3, 5))
  expect_equal(colnames(res$Z), c("age", "couleur_rouge", "couleur_vert"))
  expect_equal(res$Z[, "couleur_vert"], c(0, 1, 0, 1))
  expect_equal(unname(d$kinds), c("numeric", "categorical"))
  expect_equal(d$levels$couleur, c("bleu", "rouge", "vert"))
})

test_that("active fields are read-only", {
  d <- Preprocessor$new()
  suppressWarnings(d$fit_transform(df_train))
  expect_error(d$kinds <- "test", "read-only")
})

test_that("transform reapplies the training encoding", {
  d <- Preprocessor$new()
  suppressWarnings(d$fit_transform(df_train))
  new <- data.frame(id = 1:4,
                    couleur = c("rouge", "jaune", "bleu", "vert"),
                    age = c(30, 45, NA, 60))
  expect_warning(expect_warning(res <- d$transform(new), "missing"), "jaune")
  expect_equal(res$rows, c(1, 2, 4))
  expect_equal(colnames(res$Z), c("age", "couleur_rouge", "couleur_vert"))
  expect_equal(unname(res$Z[2, ]), c(45, 0, 0))   # jaune -> référence
})

test_that("transform fails when not fitted or incompatible", {
  new <- data.frame(age = 30, couleur = "bleu")
  expect_error(Preprocessor$new()$transform(new), "not fitted")
  d <- Preprocessor$new()
  suppressWarnings(d$fit_transform(df_train))
  expect_error(d$transform(new["age"]), "missing from")
})