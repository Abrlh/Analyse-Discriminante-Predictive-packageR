#' Backward variable selection
#'
#' Internal R6 class (inherits from `Selection`). Starts with all variables and
#' removes, at each step, the variable with the smallest partial F, as long as
#' its p-value is above `alpha`.
#'
#' @noRd
SelectionBackward <- R6::R6Class("SelectionBackward",
  inherit = Selection,

  public = list(

    #' @description Run the selection.
    #' @param Z Numeric matrix with column names (already encoded, no NA).
    #' @param y Factor of length `nrow(Z)`.
    #' @return Character vector of the selected column names of `Z`.
    select = function(Z, y) {

      # A FAIRE (partie sélection)
      
      stop("`SelectionBackward$select()` is not implemented yet.", call. = FALSE)
    }
  )
)
