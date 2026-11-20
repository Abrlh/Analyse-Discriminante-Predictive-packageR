#' Linear discriminant analysis for high dimension
#'
#' Internal R6 class (inherits from `LDA`). LDA variant suited to many or
#' collinear variables. Selected with `method = "highdim"`.
#'
#' @noRd
LDA_HighDim <- R6::R6Class("LDA_HighDim",
  inherit = LDA,

  public = list(

    #' @description Fit the high-dimension LDA.
    #' @param Z Numeric matrix with column names (already encoded, no NA).
    #' @param y Factor of length `nrow(Z)`.
    #' @return The object itself, invisibly.
    fit = function(Z, y) {

      # A FAIRE (partie LDA)
      
      stop("`LDA_HighDim$fit()` is not implemented yet.", call. = FALSE)
    }
  )
)
