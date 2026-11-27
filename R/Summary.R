#' Global test of Wilks' lambda (Rao's F approximation)
#'
#' Tests the equality of the class means. Rao's approximation turns
#' Wilks' lambda into an F statistic.
#'
#' @param lambda Wilks' lambda of the model.
#' @param n Number of observations.
#' @param p Number of variables.
#' @param K Number of classes.
#' @return A list with `lambda`, `F`, `df1`, `df2` and `p_value`.
#' @noRd
wilks_test <- function(lambda, n, p, K) {
  q <- K - 1
  # t vaut 1 dans les petits cas (p = 1 ou K = 2)
  t <- if (p^2 + q^2 - 5 > 0) sqrt((p^2 * q^2 - 4) / (p^2 + q^2 - 5)) else 1
  w <- n - 1 - (p + K) / 2
  df1 <- p * q
  df2 <- w * t - (p * q - 2) / 2
  l <- lambda^(1 / t)
  F_stat <- (1 - l) / l * df2 / df1
  list(lambda = lambda, F = F_stat, df1 = df1, df2 = df2,
      p_value = stats::pf(F_stat, df1, df2, lower.tail = FALSE))
}

#' Confusion matrix and accuracy
#'
#' Shared by `Summary` (training data) and `LDAClassifier$evaluate()`
#' (new data), so that both are computed in exactly the same way.
#'
#' @param y Observed classes (factor).
#' @param pred Predicted classes (factor with the same levels as `y`).
#' @return A list with `confusion` (rows: observed, columns: predicted)
#'   and `accuracy` (proportion of correctly classified observations).
#' @noRd
confusion_accuracy <- function(y, pred) {
  confusion <- table(Observed = y, Predicted = pred)
  list(confusion = confusion,
       accuracy = sum(diag(confusion)) / sum(confusion))
}

#' Model summary
#'
#' Internal R6 class gathering the detailed results of a fitted model:
#' global evaluation (confusion matrix, accuracy), classification functions,
#' Wilks' lambda with Rao's F test and the relevance of each variable.
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
      ca <- confusion_accuracy(y, pred)
      private$.confusion <- ca$confusion
      private$.accuracy <- ca$accuracy

      # Coefficients des fonctions de classement, fournis par la LDA
      private$.coefficients <- lda$coefficients

      # Évaluation globale et pertinence des variables (lambda de Wilks).
      # W et T : matrices de dispersion intra-classe et totale.
      vars <- colnames(Z)
      n <- nrow(Z)
      p <- length(vars)
      K <- nlevels(y)

      # Les tests de Wilks demandent n - K >= p (W inversible). En grande
      # dimension, ils ne sont pas calculés (NULL) plutôt que faux.
      if (n - K >= p) {
        WT <- calc_W_T(Z, y)

        # Test global : lambda de Wilks du modèle + F de Rao
        private$.wilks <- wilks_test(wilks(WT$W, WT$T, vars), n, p, K)

        # Pertinence : ce que chaque variable apporte en plus de toutes les autres
        tests <- lapply(vars, function(j) {
          s <- partial_stats(WT$W, WT$T, setdiff(vars, j), j, n, K)
          data.frame(variable = j, lambda_partial = s$lambda_p,
                     R2_partial = 1 - s$lambda_p, F = s$F, p_value = s$p_value)
        })
        private$.variable_tests <- do.call(rbind, tests)
      }
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

      cat("\nClassification functions (one column per class)\n")
      if (is.null(private$.coefficients)) {
        cat("  Not available.\n")
      } else {
        print(round(private$.coefficients, 4))
      }

      w <- private$.wilks
      if (is.null(w)) {
        cat("\nWilks' lambda and partial F tests: not available",
            "(more variables than n - K, high-dimensional case).\n")
        return(invisible(self))
      }
      cat("\nGlobal test (H0: equal class means)\n")
      cat(sprintf("  Wilks' lambda : %.4f\n", w$lambda))
      cat(sprintf("  Rao's F       : %.2f  (df = %g, %g)\n", w$F, w$df1, w$df2))
      cat(sprintf("  p-value       : %s\n", format.pval(w$p_value, digits = 3)))

      cat("\nRelevance of the variables (given all the others)\n")
      vt <- private$.variable_tests
      vt[, -1] <- signif(vt[, -1], 4)
      print(vt, row.names = FALSE)
      invisible(self)
    }
  ),

  private = list(
    .confusion = NULL,      # matrice de confusion
    .accuracy = NULL,       # taux de bonne classification
    .coefficients = NULL,   # coefficients des fonctions de classement
    .wilks = NULL,          # test global : lambda de Wilks + F de Rao
    .variable_tests = NULL  # pertinence de chaque variable
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
    },

    #' @field coefficients Coefficients of the classification functions
    #'   (one column per class, first row = intercept).
    coefficients = function(value) {
      if (!missing(value)) stop("`coefficients` is read-only.", call. = FALSE)
      private$.coefficients
    },

    #' @field wilks Global test: Wilks' lambda with Rao's F (list).
    wilks = function(value) {
      if (!missing(value)) stop("`wilks` is read-only.", call. = FALSE)
      private$.wilks
    },

    #' @field variable_tests Relevance of each variable (data frame):
    #'   partial lambda, partial R2, F and p-value given all other variables.
    variable_tests = function(value) {
      if (!missing(value)) stop("`variable_tests` is read-only.", call. = FALSE)
      private$.variable_tests
    }

  )
)