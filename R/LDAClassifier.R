#' Predictive linear discriminant analysis classifier
#'
#' @description
#' R6 class implementing predictive linear discriminant analysis with a
#' scikit-learn style interface: `$fit()`, `$predict()`, `$predict_proba()`,
#' `$print()` and `$summary()`.
#'
#' @details
#' Categorical predictors (factor, character or logical columns) are dummy
#' coded, the first level of each variable being used as the reference.
#' Rows with missing values are removed before fitting.
#'
#' @export
LDAClassifier <- R6::R6Class("LDAClassifier",

  public = list(

    # Constructeur ***

    #' @description Create a new classifier.
    #' @param method LDA variant: `"classic"` (default) or `"highdim"`
    #'   (for many or collinear variables).
    #' @param selection Variable selection strategy: `"none"` (default),
    #'   `"forward"`, `"backward"` or `"stepwise"`.
    #' @return A new `LDAClassifier` object.
    initialize = function(method = c("classic", "highdim"),
                          selection = c("none", "forward", "backward", "stepwise")) {
      # Vérifie que la valeur fait partie des choix autorisés
      private$.method <- match.arg(method)
      private$.selection <- match.arg(selection)
    },

    # Methode fit ***

    #' @description Fit the model on training data.
    #' @param X Data frame of predictors (numeric and/or categorical columns).
    #' @param y Factor giving the class of each row of `X` (at least 2 classes).
    #' @return The object itself, invisibly (allows chaining).
    fit = function(X, y) {

      # 0. Le modèle n'est considéré entraîné qu'à la toute fin
      private$.is_fitted <- FALSE

      # 1. Vérifications sur les données brutes
      check_X(X)
      check_y(y, X)
      check_constant(X)

      # 2. Codage des données (retire les lignes avec NA)
      private$.data <- Data$new()
      prep <- private$.data$fit_transform(X)
      Z <- prep$Z

      # 3. y aligné sur les lignes gardées, puis retrait des modalités vides
      y <- droplevels(y[prep$rows])
      if (nlevels(y) < 2) {
        stop("After removing rows with missing values, `y` contains fewer ",
             "than 2 classes.", call. = FALSE)
      }
      check_constant(X[prep$rows, , drop = FALSE])

      # 4. Effectifs par classe (avertissement si trop petits)
      check_class_sizes(y)

      # 5. Sélection de variables (optionnelle)
      vars <- colnames(Z)
      if (private$.selection != "none") {
        private$.selector <- switch(private$.selection,
          forward  = SelectionForward$new(),
          backward = SelectionBackward$new(),
          stepwise = SelectionStepwise$new()
        )
        vars <- private$.selector$select(Z, y)
        if (length(vars) == 0) {
          stop("No variable was selected.", call. = FALSE)
        }
      }

      # 6. Assez d'individus pour la LDA classique
      if (private$.method == "classic") {
        check_dimensions(n = nrow(Z), p = length(vars), K = nlevels(y))
      }

      # 7. Entraînement de la LDA choisie
      private$.lda <- switch(private$.method,
        classic = LDA_Classic$new(),
        highdim = LDA_HighDim$new()
      )
      private$.lda$fit(Z[, vars, drop = FALSE], y)

      # 8. Mémorisation des informations du modèle
      private$.selected_vars <- vars
      private$.classes <- levels(y)
      private$.n <- nrow(Z)
      private$.n_vars <- ncol(X)
      private$.class_counts <- table(y)

      # 9. Résumé du modèle (évaluation sur les données d'apprentissage)
      pred_train <- private$.lda$predict(Z[, vars, drop = FALSE])
      pred_train <- factor(as.character(pred_train), levels = levels(y))
      private$.summary <- Summary$new(Z = Z[, vars, drop = FALSE], y = y,
                                      pred = pred_train, lda = private$.lda)

      # 10. Le modèle est entraîné seulement si tout a réussi
      private$.is_fitted <- TRUE

      invisible(self)
    },

    # Methode predict ***

    #' @description Predict the class of new observations.
    #' @param X Data frame containing the variables used for fitting (extra
    #'   columns are ignored).
    #' @return A factor of length `nrow(X)` with the classes of the training
    #'   target. Rows with missing values get `NA`.
    predict = function(X) {
      prep <- private$prepare(X)

      # Résultat de la taille de X, rempli de NA par défaut
      out <- factor(rep(NA_character_, nrow(X)), levels = private$.classes)

      # Prédictions placées aux lignes complètes
      if (length(prep$rows) > 0) {
        out[prep$rows] <- as.character(private$.lda$predict(prep$Z))
      }
      out
    },

    # Methode predict_proba ***

    #' @description Posterior class membership probabilities.
    #' @param X Data frame containing the variables used for fitting (extra
    #'   columns are ignored).
    #' @return A numeric matrix with `nrow(X)` rows and one column per class.
    #'   Rows with missing values contain `NA`.
    predict_proba = function(X) {
      prep <- private$prepare(X)

      # Matrice de la taille de X, remplie de NA par défaut
      P <- matrix(NA_real_, nrow = nrow(X), ncol = length(private$.classes),
                  dimnames = list(NULL, private$.classes))

      # Probabilités placées aux lignes complètes, colonnes dans l'ordre des classes
      if (length(prep$rows) > 0) {
        P_lda <- private$.lda$predict_proba(prep$Z)
        P[prep$rows, ] <- P_lda[, private$.classes, drop = FALSE]
      }
      P
    },

    # Methode score ***

    #' @description Proportion of correctly classified observations on new
    #'   data (e.g. a test sample).
    #' @param X Data frame containing the variables used for fitting.
    #' @param y Factor of the true classes, of length `nrow(X)`.
    #' @return A number between 0 and 1. Rows that cannot be predicted
    #'   (missing values) are ignored.
    score = function(X, y) {
      check_fitted(private$.is_fitted)
      check_X(X)
      check_y(y, X)
      pred <- self$predict(X)
      mean(as.character(pred) == as.character(y), na.rm = TRUE)
    },

    # Methode print ***

    #' @description Print a short description of the model.
    #' @param ... Unused.
    #' @return The object itself, invisibly.
    print = function(...) {
      cat("<LDAClassifier>\n")
      cat("  Method    :", private$.method, "\n")
      cat("  Selection :", private$.selection, "\n")

      # Avant le fit : on s'arrête là (print ne doit jamais planter)
      if (!private$.is_fitted) {
        cat("  Status    : not fitted yet\n")
        return(invisible(self))
      }

      counts <- private$.class_counts
      cat("  Status    : fitted on", private$.n, "observations\n")
      cat("  Variables :", private$.n_vars, "original,",
          length(private$.selected_vars), "encoded used by the model\n")
      cat("  Classes   :",
          paste0(names(counts), " (", counts, ")", collapse = ", "), "\n")
      invisible(self)
    },

    # Methode summary ***

    #' @description Print a detailed summary of the fitted model.
    #' @param ... Unused.
    #' @return The summary object, invisibly.
    summary = function(...) {
      check_fitted(private$.is_fitted)
      print(private$.summary)
      invisible(private$.summary)
    }
  ),

  private = list(
    .method = NULL,         # "classic" ou "highdim"
    .selection = NULL,      # "none", "forward", "backward" ou "stepwise"
    .data = NULL,           # objet Data (codage mémorisé)
    .selector = NULL,       # objet Selection (si sélection demandée)
    .lda = NULL,            # objet LDA entraîné
    .selected_vars = NULL,  # colonnes de Z utilisées par la LDA
    .classes = NULL,        # modalités de y
    .n = NULL,              # nombre d'individus utilisés
    .n_vars = NULL,         # nombre de variables d'origine
    .class_counts = NULL,   # effectifs par classe
    .summary = NULL,        # objet Summary (créé à la fin du fit)
    .is_fitted = FALSE,

    # Code commun à predict() et predict_proba() :
    # vérifie, code les nouvelles données et garde les colonnes du modèle
    prepare = function(X) {
      check_fitted(private$.is_fitted)
      check_X(X)
      prep <- private$.data$transform(X)
      prep$Z <- prep$Z[, private$.selected_vars, drop = FALSE]
      prep
    }
  ),

  active = list(

    #' @field method LDA variant used.
    method = function(value) {
      if (!missing(value)) stop("`method` is read-only.", call. = FALSE)
      private$.method
    },

    #' @field selection Variable selection strategy used.
    selection = function(value) {
      if (!missing(value)) stop("`selection` is read-only.", call. = FALSE)
      private$.selection
    },

    #' @field is_fitted `TRUE` once `$fit()` has succeeded.
    is_fitted = function(value) {
      if (!missing(value)) stop("`is_fitted` is read-only.", call. = FALSE)
      private$.is_fitted
    },

    #' @field selected_vars Encoded variables used by the model.
    selected_vars = function(value) {
      if (!missing(value)) stop("`selected_vars` is read-only.", call. = FALSE)
      private$.selected_vars
    },

    #' @field classes Classes of the target variable.
    classes = function(value) {
      if (!missing(value)) stop("`classes` is read-only.", call. = FALSE)
      private$.classes
    },

    #' @field coefficients Coefficients of the classification functions
    #'   (`NULL` until provided by the LDA classes).
    coefficients = function(value) {
      if (!missing(value)) stop("`coefficients` is read-only.", call. = FALSE)
      # A FAIRE : renvoyer les coefficients de la LDA (format à convenir)
      NULL
    },

    #' @field confusion Confusion matrix on the training data
    #'   (rows: observed classes, columns: predicted classes; `NULL` before
    #'   fitting).
    confusion = function(value) {
      if (!missing(value)) stop("`confusion` is read-only.", call. = FALSE)
      private$.summary$confusion
    },

    #' @field accuracy Proportion of correctly classified training observations
    #'   (resubstitution rate, optimistic; `NULL` before fitting).
    accuracy = function(value) {
      if (!missing(value)) stop("`accuracy` is read-only.", call. = FALSE)
      private$.summary$accuracy
    }
  )
)