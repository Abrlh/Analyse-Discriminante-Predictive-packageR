#' ldapred: predictive linear discriminant analysis
#'
#' @description
#' The `ldapred` package implements predictive linear discriminant analysis
#' (LDA) for a categorical target with two or more classes, with an R6
#' interface in the style of scikit-learn and an interactive Shiny
#' application.
#'
#' @section Main functions:
#' \itemize{
#'   \item [LDAClassifier]: the model. Create it with `LDAClassifier$new()`,
#'     fit it with `$fit(X, y)`, predict with `$predict(X)` and
#'     `$predict_proba(X)`, and describe it with `$print()` and `$summary()`.
#'   \item [train_test_split()]: split a data set into a training sample and
#'     a test sample, to evaluate the model on observations it has never
#'     seen.
#'   \item [run_app()]: launch the interactive application (load a file,
#'     choose the variables, fit the model, explore the results and predict
#'     new observations).
#' }
#'
#' @section Features:
#' \itemize{
#'   \item Numeric and categorical predictors (dummy coding).
#'   \item Classification functions (coefficients of the model).
#'   \item Global evaluation of the model (confusion matrix, accuracy,
#'     Wilks' lambda with Rao's F test) and relevance of each variable
#'     (partial F tests).
#'   \item Variable selection: forward, backward and stepwise, based on
#'     Wilks' lambda (as in SAS PROC STEPDISC).
#'   \item High-dimensional LDA, with a shrinkage of the within-class
#'     covariance matrix (Schäfer and Strimmer, 2005).
#' }
#'
#' @section Getting started:
#' See the examples of [LDAClassifier] for a complete workflow on the
#' `iris` data set.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom R6 R6Class
## usethis namespace: end
NULL
