#' Predictive linear discriminant analysis classifier
#'
#' @description
#' R6 class implementing predictive linear discriminant analysis (LDA) for a
#' categorical target with two or more classes. Its interface follows
#' scikit-learn: create the object with `$new()`, fit it with `$fit(X, y)`,
#' then predict new observations with `$predict(X)` (classes) or
#' `$predict_proba(X)` (membership probabilities). `$print()` gives a short
#' description and `$summary()` a detailed one.
#'
#' @details
#' **Model.** Each class \eqn{k} is assumed to follow a multivariate normal
#' distribution with mean \eqn{\mu_k} and a covariance matrix \eqn{W} common
#' to all classes. Bayes' rule then gives one linear classification function
#' per class,
#' \deqn{d_k(x) = \ln \pi_k - \frac{1}{2} \mu_k^\top W^{-1} \mu_k + (W^{-1} \mu_k)^\top x,}
#' where \eqn{\pi_k} is the proportion of class \eqn{k} in the training data.
#' An observation is assigned to the class with the highest score, and the
#' membership probabilities are the softmax of the scores.
#'
#' **Workflow of `$fit()`.** The data are checked, rows with missing values
#' are removed (with a warning), categorical predictors are encoded, the
#' variables are optionally selected, the LDA is fitted and the evaluation
#' indicators are computed on the training data.
#'
#' **Categorical predictors.** Factor, character and logical columns are
#' dummy coded: one 0/1 column per level, except the first level, used as the
#' reference (columns named `variable_level`). Unknown levels met in new data
#' are treated as the reference level, with a warning. After a variable
#' selection, a categorical variable is kept as a whole: if one of its dummy
#' columns is selected, all of them are kept.
#'
#' **Variable selection** (`selection`). Stepwise discriminant analysis based
#' on Wilks' lambda, as in SAS PROC STEPDISC: `"forward"` adds the most
#' significant variable while its p-value is at most `slentry`,
#' `"backward"` removes the least significant variable while its p-value is
#' greater than `slstay`, and `"stepwise"` alternates both (it requires
#' `slentry <= slstay`). The selection is done on the training data only.
#'
#' **High dimension** (`method = "highdim"`). When there are many or highly
#' correlated variables, \eqn{W} is badly estimated or cannot be inverted.
#' The high-dimensional LDA shrinks it toward its diagonal,
#' \eqn{W_\lambda = (1 - \lambda) W + \lambda \mathrm{diag}(W)}, with an
#' intensity \eqn{\lambda} estimated from the data (Schäfer and Strimmer,
#' 2005) or set with `shrinkage`. The classic LDA needs \eqn{n - K \ge p}.
#'
#' @section Reading the results:
#' \itemize{
#'   \item `coefficients`: one column per class; the first row (`intercept`)
#'     is the constant of the classification function, the other rows the
#'     coefficient of each variable. The larger the coefficient of a variable
#'     for a class, the more large values of this variable favour this class.
#'   \item `confusion` and `accuracy`: classification of the training data.
#'     This resubstitution rate is optimistic; use [train_test_split()] and
#'     `$evaluate()` or `$score()` on a test sample for an honest estimate.
#'   \item `wilks`: global test of the equality of the class means. Wilks'
#'     lambda is between 0 and 1, the closer to 0 the better the classes are
#'     separated. Rao's F approximation gives the p-value: a small p-value
#'     means that the classes are significantly separated.
#'   \item `variable_tests`: relevance of each variable given all the
#'     others (partial lambda, partial R2, F statistic and p-value). A large
#'     p-value indicates a variable that brings nothing more than the others.
#'   \item `selection_history`: one row per step of the selection (variable
#'     entered or removed, Wilks' lambda of the model, partial R2, F, p-value).
#' }
#'
#' @references
#' Rakotomalala R., *Analyse discriminante linéaire prédictive*, course
#' notes, Université Lumière Lyon 2.
#'
#' Rao C. R. (1951), An asymptotic expansion of the distribution of Wilks'
#' criterion, *Bulletin of the International Statistical Institute*, 33,
#' 177-180.
#'
#' Schäfer J., Strimmer K. (2005), A shrinkage approach to large-scale
#' covariance matrix estimation and implications for functional genomics,
#' *Statistical Applications in Genetics and Molecular Biology*, 4(1).
#'
#' SAS Institute, *The STEPDISC Procedure*, SAS/STAT User's Guide.
#'
#' @seealso [train_test_split()] to build a test sample, [run_app()] for the
#'   interactive application.
#'
#' @examples
#' # Training and test samples
#' sp <- train_test_split(iris[, 1:4], iris$Species, test_size = 0.3, seed = 1)
#'
#' # Fit the model and describe it
#' model <- LDAClassifier$new()
#' model$fit(sp$X_train, sp$y_train)
#' model
#' model$summary()
#'
#' # Classification functions, global test and relevance of the variables
#' model$coefficients
#' model$wilks
#' model$variable_tests
#'
#' # Predict new observations and evaluate the model on the test sample
#' head(model$predict(sp$X_test))
#' head(model$predict_proba(sp$X_test))
#' model$evaluate(sp$X_test, sp$y_test)
#'
#' # Stepwise selection of the variables
#' sel <- LDAClassifier$new(selection = "stepwise", slentry = 0.05, slstay = 0.05)
#' sel$fit(iris[, 1:4], iris$Species)
#' sel$selected_vars
#' sel$selection_history
#'
#' # A categorical predictor with the high-dimensional LDA
#' df <- iris[, 1:4]
#' df$size <- cut(iris$Sepal.Width, 3, labels = c("small", "medium", "large"))
#' hd <- LDAClassifier$new(method = "highdim")
#' hd$fit(df, iris$Species)
#' hd$shrinkage
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
    #' @param slentry Significance level to enter a variable (forward and
    #'   stepwise selection). Default 0.15.
    #' @param slstay Significance level to stay in the model (backward and
    #'   stepwise selection). Default 0.15.
    #' @param max_iter Maximum number of iterations of the variable selection
    #'   (positive integer, default 100). The selection stops if it reaches it.
    #' @param shrinkage Shrinkage intensity of the high-dimensional LDA, in
    #'   \[0, 1\], or `NULL` (default) to estimate it. Ignored when
    #'   `method = "classic"`.
    #' @return A new `LDAClassifier` object.
    initialize = function(method = c("classic", "highdim"),
                          selection = c("none", "forward", "backward", "stepwise"),
                          slentry = 0.15, slstay = 0.15, max_iter = 100,
                          shrinkage = NULL) {
      # Vérifie que la valeur fait partie des choix autorisés
      private$.method <- match.arg(method)
      private$.selection <- match.arg(selection)
      # Seuils de la sélection (vérifiés par la classe Selection)
      private$.slentry <- slentry
      private$.slstay <- slstay
      private$.max_iter <- max_iter
      private$.shrinkage <- shrinkage
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
      private$.preprocessor <- Preprocessor$new()
      prep <- private$.preprocessor$fit_transform(X)
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
        private$.selector <- private$new_selector()
        private$.selector$fit(Z, y)
        vars <- private$.selector$selected
        # Stratégie catégorielle : variable gardée en entier si une indicatrice est retenue
        vars <- private$complete_dummies(vars)
        if (length(vars) == 0) {
          stop("No variable was selected.", call. = FALSE)
        }
      }

      # 6. Assez d'individus pour la LDA classique
      if (private$.method == "classic") {
        check_dimensions(n = nrow(Z), p = length(vars), K = nlevels(y))
      }

      # 7. Entraînement de la LDA choisie
      private$.lda <- private$new_lda()
      private$.lda$fit(Z[, vars, drop = FALSE], y)

      # 8. Mémorisation des informations du modèle
      private$.selected_vars <- vars
      private$.classes <- levels(y)
      private$.n <- nrow(Z)
      private$.n_vars <- ncol(X)
      private$.class_counts <- table(y)

      # 9. Résumé du modèle (évaluation sur les données d'apprentissage)
      pred_train <- private$.lda$predict(Z[, vars, drop = FALSE])
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

    # Methode evaluate ***

    #' @description Evaluate the model on new data (e.g. a test sample).
    #' @param X Data frame containing the variables used for fitting.
    #' @param y Factor of the true classes, of length `nrow(X)`.
    #' @return A list with `confusion` (confusion matrix, rows: observed
    #'   classes, columns: predicted classes) and `accuracy` (proportion of
    #'   correctly classified observations). Rows that cannot be predicted
    #'   (missing values) are ignored.
    evaluate = function(X, y) {
      check_fitted(private$.is_fitted)
      check_X(X)
      check_y(y, X)
      pred <- self$predict(X)

      # On ignore les lignes non prédites (valeurs manquantes)
      keep <- !is.na(pred)
      truth <- factor(as.character(y[keep]), levels = private$.classes)
      pred <- factor(as.character(pred[keep]), levels = private$.classes)

      confusion_accuracy(truth, pred)
    },

    # Methode score ***

    #' @description Proportion of correctly classified observations on new
    #'   data (e.g. a test sample).
    #' @param X Data frame containing the variables used for fitting.
    #' @param y Factor of the true classes, of length `nrow(X)`.
    #' @return A number between 0 and 1. Rows that cannot be predicted
    #'   (missing values) are ignored.
    score = function(X, y) {
      self$evaluate(X, y)$accuracy
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
    .preprocessor = NULL,   # objet Preprocessor (codage mémorisé)
    .selector = NULL,       # objet Selection (si sélection demandée)
    .lda = NULL,            # objet LDA entraîné
    .selected_vars = NULL,  # colonnes de Z utilisées par la LDA
    .classes = NULL,        # modalités de y
    .n = NULL,              # nombre d'individus utilisés
    .n_vars = NULL,         # nombre de variables d'origine
    .class_counts = NULL,   # effectifs par classe
    .summary = NULL,        # objet Summary (créé à la fin du fit)
    .is_fitted = FALSE,
    .slentry = NULL,        # seuil d'entrée de la sélection
    .slstay = NULL,         # seuil de maintien de la sélection
    .max_iter = NULL,       # nombre maximal d'itérations de la sélection
    .shrinkage = NULL,      # intensité du shrinkage (LDA grande dimension)

    # Code commun à predict() et predict_proba() :
    # vérifie, code les nouvelles données et garde les colonnes du modèle
    prepare = function(X) {
      check_fitted(private$.is_fitted)
      check_X(X)
      prep <- private$.preprocessor$transform(X)
      prep$Z <- prep$Z[, private$.selected_vars, drop = FALSE]
      prep
    },

    # Stratégie pour les variables catégorielles : une variable est gardée
    # en entier dès qu'au moins une de ses indicatrices a été sélectionnée.
    complete_dummies = function(vars) {
      for (dummies in private$.preprocessor$dummies) {   # indicatrices de chaque variable
        if (any(dummies %in% vars)) {
          vars <- union(vars, dummies)            # on ajoute celles qui manquent
        }
      }
      # Remet les colonnes dans l'ordre de la matrice codée
      intersect(private$.preprocessor$feature_names, vars)
    },

    # Crée l'objet LDA correspondant à la méthode choisie.
    # Le shrinkage ne concerne que la LDA grande dimension.
    new_lda = function() {
      switch(private$.method,
        classic = LDA_classic$new(),
        highdim = LDA_high_dim$new(shrinkage = private$.shrinkage)
      )
    },

    # Crée l'objet de sélection correspondant à la stratégie choisie.
    new_selector = function() {
      selector <- switch(private$.selection,
        forward  = SelectionForward,
        backward = SelectionBackward,
        stepwise = SelectionStepwise
      )
      selector$new(slentry = private$.slentry, slstay = private$.slstay,
                   max_iter = private$.max_iter)
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

    #' @field selection_history Step-by-step history of the variable
    #'   selection (data frame), or `NULL` when no selection was used.
    selection_history = function(value) {
      if (!missing(value)) stop("`selection_history` is read-only.", call. = FALSE)
      if (is.null(private$.selector)) return(NULL)
      private$.selector$history
    },

    #' @field classes Classes of the target variable.
    classes = function(value) {
      if (!missing(value)) stop("`classes` is read-only.", call. = FALSE)
      private$.classes
    },

    #' @field coefficients Coefficients of the classification functions
    #'   (one column per class, first row = intercept; `NULL` before fitting).
    coefficients = function(value) {
      if (!missing(value)) stop("`coefficients` is read-only.", call. = FALSE)
      if (is.null(private$.summary)) return(NULL)   # pas encore entraîné
      private$.summary$coefficients
    },

    #' @field shrinkage Shrinkage intensity used by the high-dimensional LDA
    #'   (`NULL` for the classic LDA or before fitting).
    shrinkage = function(value) {
      if (!missing(value)) stop("`shrinkage` is read-only.", call. = FALSE)
      if (is.null(private$.lda) || private$.method != "highdim") return(NULL)
      private$.lda$shrinkage_intensity
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
    },

    #' @field wilks Global test of the model: Wilks' lambda with Rao's F
    #'   (`NULL` before fitting).
    wilks = function(value) {
      if (!missing(value)) stop("`wilks` is read-only.", call. = FALSE)
      if (is.null(private$.summary)) return(NULL)
      private$.summary$wilks
    },

    #' @field variable_tests Relevance of each variable of the model
    #'   (`NULL` before fitting).
    variable_tests = function(value) {
      if (!missing(value)) stop("`variable_tests` is read-only.", call. = FALSE)
      if (is.null(private$.summary)) return(NULL)
      private$.summary$variable_tests
    }
  )
)

# ---------------------------------------------------------------------------
# Découpage apprentissage / test
# ---------------------------------------------------------------------------

#' Split data into a training sample and a test sample
#'
#' Randomly splits a data set into a training sample, used to fit the model,
#' and a test sample, used to evaluate it on observations it has never seen
#' (see [LDAClassifier]'s `$evaluate()` method). By default the split is
#' stratified: each class of `y` keeps the same proportion in both samples.
#'
#' @details
#' The accuracy computed on the training data (resubstitution rate) is
#' optimistic, since the model has already seen these observations. The
#' accuracy on a test sample is a more honest estimate of the performance on
#' new observations.
#'
#' When `seed` is given, [set.seed()] is called, so the split is reproducible
#' and the global random number generator is modified.
#'
#' @param X Data frame of predictors.
#' @param y Factor giving the class of each row of `X`.
#' @param test_size Proportion of observations in the test sample, strictly
#'   between 0 and 1. Default `0.3`.
#' @param seed Optional integer seed, for a reproducible split. `NULL`
#'   (default) uses the current state of the random number generator.
#' @param stratify Logical. If `TRUE` (default), observations are sampled
#'   separately within each class, so that both samples keep the class
#'   proportions of `y`.
#' @return A list with the elements `X_train`, `X_test` (data frames),
#'   `y_train`, `y_test` (factors) and `test_index` (row numbers of the test
#'   observations in `X`).
#' @examples
#' split <- train_test_split(iris[, 1:4], iris$Species, test_size = 0.3,
#'                           seed = 123)
#' nrow(split$X_train)
#' table(split$y_test)
#' @export
train_test_split <- function(X, y, test_size = 0.3, seed = NULL, stratify = TRUE) {

  # Vérification des entrées
  check_X(X)
  check_y(y, X)
  if (!is.numeric(test_size) || length(test_size) != 1 ||
      test_size <= 0 || test_size >= 1) {
    stop("`test_size` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  # Graine aléatoire (reproductibilité)
  if (!is.null(seed)) set.seed(seed)

  n <- nrow(X)
  if (isTRUE(stratify)) {
    # Tirage séparé dans chaque classe : mêmes proportions dans les deux échantillons
    groups <- split(seq_len(n), y)
    test_index <- unlist(lapply(groups, function(i) {
      i[sample.int(length(i), round(length(i) * test_size))]
    }), use.names = FALSE)
  } else {
    test_index <- sample.int(n, round(n * test_size))
  }
  test_index <- sort(test_index)

  # Les deux échantillons doivent contenir au moins un individu
  if (length(test_index) == 0 || length(test_index) == n) {
    stop("The test sample would be empty or contain all observations. ",
         "Change `test_size`.", call. = FALSE)
  }

  train_index <- setdiff(seq_len(n), test_index)
  list(
    X_train = X[train_index, , drop = FALSE],
    X_test = X[test_index, , drop = FALSE],
    y_train = y[train_index],
    y_test = y[test_index],
    test_index = test_index
  )
}
