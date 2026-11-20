#' Variable selection (abstract base class)
#'
#' Internal abstract R6 class defining the common interface of all variable
#' selection strategies based on partial F tests (Wilks' lambda).
#' Subclasses must implement `$select()`, which has to fill the private fields
#' `.selected_vars` and `.history`, and return the selected variable names.
#'
#' `history` is a data frame with one row per step and the columns `step`,
#' `variable`, `action` ("added" or "removed"), `F` and `p_value`.
#'
#' @noRd
Selection <- R6::R6Class("Selection",

  public = list(

    #' @description Select variables. Abstract: must be implemented by subclasses.
    #' @param Z Numeric matrix with column names (already encoded, no NA).
    #' @param y Factor of length `nrow(Z)` (no NA, no empty level).
    #' @return Character vector of the selected column names of `Z`.
    select = function(Z, y) {

      # A Faire...
      
      stop("`select()` must be implemented by a subclass of Selection.",call. = FALSE)
    }
  )

)
