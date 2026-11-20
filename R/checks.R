#' Check the predictor data frame
#'
#' Internal helper. Stops with an informative message if `X` is not a
#' non-empty data frame with unique column names and supported column types.
#'
#' @param X Object supplied by the user as predictors.
#' @return `invisible(TRUE)` if all checks pass.
#' @noRd
check_X <- function(X) {

  # 1. X doit être un dataframe
  if (!is.data.frame(X)) {
    stop("`X` must be a data.frame, not an object of class '",
         class(X)[1], "'.", call. = FALSE)
  }

  # 2. X ne doit pas être vide
  if (nrow(X) == 0) {
    stop("`X` has no rows.", call. = FALSE)
  }
  if (ncol(X) == 0) {
    stop("`X` has no columns.", call. = FALSE)
  }

  # 3. Les noms de colonnes doivent exister et être uniques 
  if (any(names(X) == "") || anyDuplicated(names(X)) > 0) {
    stop("All columns of `X` must have unique, non-empty names.", call. = FALSE)
  }

  # 4. Chaque colonne doit être numérique, facteur, caractère ou logique 
  ok_type <- vapply(X, function(col) {
    is.numeric(col) || is.factor(col) || is.character(col) || is.logical(col)
  }, logical(1))

  if (!all(ok_type)) {
    stop("Unsupported column type in `X`: ",
         paste(names(X)[!ok_type], collapse = ", "),
         ". Only numeric, factor, character or logical columns are allowed.",
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Check the target variable
#'
#' Internal helper. Stops with an informative message if `y` is not a factor
#' of the same length as the number of rows of `X`, without missing values,
#' and with at least two observed classes.
#'
#' @param y Object supplied by the user as the target variable.
#' @param X The predictor data frame (already checked by `check_X()`).
#' @return `invisible(TRUE)` if all checks pass.
#' @noRd
check_y <- function(y, X) {

  # 1. y doit être un factor
  if (!is.factor(y)) {
    stop("`y` must be a factor, not an object of class '", class(y)[1], "'. ", "Use `as.factor(y)` to convert it.", call. = FALSE)
  }

  # 2. y doit avoir autant de valeurs que X a de lignes
  if (length(y) != nrow(X)) {
    stop("`y` has ", length(y), " values but `X` has ", nrow(X), " rows. ", "They must have the same length.", call. = FALSE)
  }

  # 3. y ne doit pas contenir de valeurs manquantes
  if (anyNA(y)) {
    stop("`y` contains ", sum(is.na(y)), " missing value(s). ", "Remove these observations before fitting.", call. = FALSE)
  }

  # 4. Au moins 2 classes réellement présentes (et pas seulement déclarées)
  n_classes <- length(unique(y))
  if (n_classes < 2) {
    stop("`y` must contain at least 2 different classes, found ", n_classes, ".", call. = FALSE)
  }

  invisible(TRUE)
}

#' Check the number of observations per class
#'
#' Internal helper. Emits a warning if some classes contain fewer than
#' `min_size` observations, since their mean is then poorly estimated.
#'
#' @param y A factor (already checked by `check_y()`).
#' @param min_size Minimum recommended number of observations per class.
#' @return `invisible(TRUE)`, possibly with a warning.
#' @noRd
check_class_sizes <- function(y, min_size = 2) {

  # Effectifs par classe (uniquement les classes présentes)
  sizes <- table(droplevels(y))
  small <- sizes[sizes < min_size]

  # Avertissement (non bloquant) pour les classes trop petites
  if (length(small) > 0) {
    warning("The following class(es) have fewer than ", min_size,
            " observations: ",
            paste0(names(small), " (n = ", small, ")", collapse = ", "),
            ". Their estimates may be unreliable.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Check that there are enough observations for the number of variables
#'
#' Internal helper. The pooled within-class covariance matrix W (p x p) has
#' rank at most n - K, so it cannot be inverted when n - K < p.
#'
#' @param n Number of observations.
#' @param p Number of variables after encoding of categorical predictors.
#' @param K Number of classes.
#' @return `invisible(TRUE)` if the check passes.
#' @noRd
check_dimensions <- function(n, p, K) {

  # W est de rang au plus n - K : elle n'est pas inversible si n - K < p
  if (n - K < p) {
    stop("Not enough observations: n - K = ", n - K, " but p = ", p,
         " variables (after encoding). The within-class covariance matrix ",
         "cannot be inverted. Use variable selection or method = \"highdim\".",
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Check for constant columns
#'
#' Internal helper. Stops with an informative message if some columns of `X`
#' take a single value (ignoring missing values). Such columns carry no
#' information and make the within-class covariance matrix singular.
#'
#' @param X The predictor data frame (already checked by `check_X()`).
#' @return `invisible(TRUE)` if no column is constant.
#' @noRd
check_constant <- function(X) {

  # Une colonne est constante si elle a au plus 1 valeur distincte (hors NA)
  is_const <- vapply(X, function(col) {
    length(unique(col[!is.na(col)])) <= 1
  }, logical(1))

  # Arrêt en nommant les colonnes concernées
  if (any(is_const)) {
    stop("The following column(s) of `X` are constant: ",
         paste(names(X)[is_const], collapse = ", "),
         ". Remove them before fitting.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Determine the kind of a column
#'
#' Internal helper. Numeric columns are "numeric"; factor, character and
#' logical columns are "categorical".
#'
#' @param col A column of a data frame.
#' @return Either `"numeric"` or `"categorical"`.
#' @noRd
column_kind <- function(col) {
  if (is.numeric(col)) "numeric" else "categorical"
}

#' Check that the model has been fitted
#'
#' Internal helper. Stops if a method requiring a fitted model is called
#' before `$fit()`.
#'
#' @param is_fitted Logical flag stored by the model (`TRUE` after `$fit()`).
#' @return `invisible(TRUE)` if the model is fitted.
#' @noRd
check_fitted <- function(is_fitted) {

  # Le modèle doit avoir été entraîné avant predict / summary
  if (!isTRUE(is_fitted)) {
    stop("The model is not fitted yet. Call `$fit(X, y)` first.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Check that new data are compatible with the training data
#'
#' Internal helper. Stops if some training variables are missing from `X`,
#' or if their kind (numeric / categorical) differs from training.
#' Extra columns in `X` are allowed and will be ignored.
#'
#' @param X New predictor data frame (already checked by `check_X()`).
#' @param train_kinds Named character vector giving the kind of each training
#'   variable, e.g. `c(age = "numeric", city = "categorical")`.
#' @return `invisible(TRUE)` if `X` is compatible.
#' @noRd
check_compatible <- function(X, train_kinds) {

  # 1. Toutes les variables du fit doivent être présentes
  missing_vars <- setdiff(names(train_kinds), names(X))
  if (length(missing_vars) > 0) {
    stop("The following variable(s) used for fitting are missing from `X`: ",
         paste(missing_vars, collapse = ", "), ".", call. = FALSE)
  }

  # 2. Chaque variable doit être du même genre qu'au fit
  new_kinds <- vapply(X[names(train_kinds)], column_kind, character(1))
  wrong <- names(train_kinds)[new_kinds != train_kinds]
  if (length(wrong) > 0) {
    stop("The following variable(s) do not have the same type as during fitting: ",
         paste0(wrong, " (expected ", train_kinds[wrong], ")", collapse = ", "),
         ".", call. = FALSE)
  }

  invisible(TRUE)
}