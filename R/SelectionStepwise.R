#' Stepwise variable selection
#'
#' Internal R6 class (inherits from `Selection`). Forward selection in which,
#' after each addition, previously added variables that are no longer
#' significant can be removed.
#'
#' @noRd
SelectionStepwise <- R6::R6Class("SelectionStepwise",
  inherit = Selection,

  public = list(

    #' @description Run the selection.
    #' @param Z Numeric matrix with column names (already encoded, no NA).
    #' @param y Factor of length `nrow(Z)`.
    #' @return Character vector of the selected column names of `Z`.
    select = function(Z, y) {

      # A FAIRE (partie sélection)
      
      stop("`SelectionStepwise$select()` is not implemented yet.", call. = FALSE)
    }
  )
)
