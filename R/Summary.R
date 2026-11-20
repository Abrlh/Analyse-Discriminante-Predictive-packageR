#' Model summary
#'
#' Internal R6 class gathering the detailed results of a fitted model:
#' global evaluation (Wilks' lambda, Rao's F, confusion matrix), relevance of
#' each variable (partial F tests) and classification functions.
#' Used by `LDAClassifier$summary()`.
#'
#' @noRd
Summary <- R6::R6Class("Summary",

  public = list(

    #' @description Print the detailed summary.
    #' @param ... Unused.
    print = function(...) {

      # A FAIRE : affichage mis en forme des résultats

      cat("Summary: not implemented yet.\n")
      invisible(self)
    }
  )

)
