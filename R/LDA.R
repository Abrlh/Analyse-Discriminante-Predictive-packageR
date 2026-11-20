#' Linear discriminant analysis (abstract base class)
#'
#' Internal abstract R6 class defining the common interface of all LDA
#' variants. Subclasses must implement `$fit()`, `$predict()` and
#' `$predict_proba()`.
#'
#' @noRd
LDA <- R6::R6Class("LDA",

  public = list(
    
    #' @description Fit the model. Abstract.
    #' @param Z Numeric matrix with column names (already encoded, no NA).
    #' @param y Factor of length `nrow(Z)`.
    #' @return The object itself, invisibly.
    fit = function(Z, y) {

      # A faire...

      stop("`fit()` must be implemented by a subclass of LDA.", call. = FALSE)
    },

    #' @description Predicted classes. Abstract.
    #' @param Z Numeric matrix with the same columns as during fitting.
    #' @return A factor of predicted classes.
    predict = function(Z) {

      # A faire...

      stop("`predict()` must be implemented by a subclass of LDA.", call. = FALSE)
    },

    #' @description Posterior class probabilities. Abstract.
    #' @param Z Numeric matrix with the same columns as during fitting.
    #' @return A n x K matrix of probabilities (column names = classes).
    predict_proba = function(Z) {

      # A faire...

      stop("`predict_proba()` must be implemented by a subclass of LDA.", call. = FALSE)
    }
  )
)
