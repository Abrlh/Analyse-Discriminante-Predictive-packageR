#' Classic linear discriminant analysis
#'
#' Internal R6 class (inherits from `LDA`). Standard predictive LDA based on
#' the pooled within-class covariance matrix.
#'
#' @noRd
LDA_Classic <- R6::R6Class("LDA_Classic",
  inherit = LDA,

  public = list(

    #' @description Fit the classic LDA.
    #' @param Z Numeric matrix with column names (already encoded, no NA).
    #' @param y Factor of length `nrow(Z)`.
    #' @return The object itself, invisibly.
    fit = function(Z, y) {

      # A FAIRE (partie LDA)
      
      stop("`LDA_Classic$fit()` is not implemented yet.", call. = FALSE)
    }
  )
)
