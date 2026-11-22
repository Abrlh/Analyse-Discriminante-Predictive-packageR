#' Model summary
#'
#' Internal R6 class gathering the detailed results of a fitted model:
#' global evaluation (confusion matrix, accuracy) and, later, Wilks' lambda,
#' partial F tests and classification functions.
#' Created at the end of `LDAClassifier$fit()` and used by
#' `LDAClassifier$summary()`.
#'
#' @noRd
Summary <- R6::R6Class("Summary",

  public = list(

    #' @description Compute the summary of a fitted model.
    #' @param Z Encoded training matrix used by the LDA.
    #' @param y Training target (factor).
    #' @param pred Predicted classes of the training data (factor with the
    #'   same levels as `y`).
    #' @param lda Fitted LDA object.
    initialize = function(Z, y, pred, lda) {

      # Évaluation globale : qualité du classement (apprentissage)
      private$.confusion <- table(Observed = y, Predicted = pred)
      private$.accuracy <- mean(pred == y)

      # A FAIRE : Wilks, F de Rao, F partiels et coefficients, calculés à
      # partir de Z et de lda. On ne stocke que les résultats, jamais Z.
    },

    #' @description Print the detailed summary.
    #' @param ... Unused.
    #' @return The object itself, invisibly.
    print = function(...) {
      cat("=== Model summary ===\n\n")

      cat("Global evaluation (training data)\n")
      cat(sprintf("  Accuracy   : %.1f %%\n", 100 * private$.accuracy))
      cat(sprintf("  Error rate : %.1f %%\n\n", 100 * (1 - private$.accuracy)))

      cat("Confusion matrix (rows: observed, columns: predicted)\n")
      print(private$.confusion)

      cat("\nWilks' lambda, partial F tests and coefficients: not available yet.\n")
      invisible(self)
    }
  ),

  private = list(
    .confusion = NULL,   # matrice de confusion
    .accuracy = NULL     # taux de bonne classification
  ),

  active = list(

    #' @field confusion Confusion matrix (rows: observed, columns: predicted).
    confusion = function(value) {
      if (!missing(value)) stop("`confusion` is read-only.", call. = FALSE)
      private$.confusion
    },

    #' @field accuracy Proportion of correctly classified observations.
    accuracy = function(value) {
      if (!missing(value)) stop("`accuracy` is read-only.", call. = FALSE)
      private$.accuracy
    }
  )
)