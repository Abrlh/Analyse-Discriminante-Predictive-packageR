#' Forward variable selection
#'
#' Internal R6 class (inherits from `Selection`). Starts with no variable and
#' adds, at each step, the variable with the largest partial F, as long as its
#' p-value is below `alpha`.
#'
#' @noRd
SelectionForward <- R6::R6Class("SelectionForward",
  inherit = Selection,

  public = list(

    #' @description Run the selection.
    #' @param Z Numeric matrix with column names (already encoded, no NA).
    #' @param y Factor of length `nrow(Z)`.
    #' @return Character vector of the selected column names of `Z`.
    select = function(Z, y) {

      # A FAIRE (partie sélection)
      
      stop("`SelectionForward$select()` is not implemented yet.", call. = FALSE)
    }
  )
)
