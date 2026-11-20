#' Data preprocessing for LDA
#'
#' Internal R6 class that converts a raw predictor data frame into a numeric
#' matrix usable by the LDA classes (dummy coding of categorical variables,
#' removal of incomplete rows), and stores the encoding learned during
#' fitting so that it can be applied identically to new data.
#'
#' @noRd
Data <- R6::R6Class("Data",

  public = list(

    #' @description Learn the encoding from training data and apply it.
    #' @param X Predictor data frame (already checked by `check_X()`).
    #' @return A list with `Z` (numeric matrix) and `rows` (indices of the
    #'   rows of `X` that were kept).
    fit_transform = function(X) {

      # 1. On garde uniquement les lignes complètes (sans NA)
      rows <- which(stats::complete.cases(X))
      n_removed <- nrow(X) - length(rows)
      if (n_removed > 0) {
        warning(n_removed, " row(s) with missing values removed before fitting.", call. = FALSE)
      }
      X <- X[rows, , drop = FALSE]

      # 2. On mémorise le genre de chaque variable (numeric / categorical)
      private$.kinds <- vapply(X, column_kind, character(1))

      # 3. On mémorise les modalités de chaque variable catégorielle
      #    (la première modalité servira de référence)
      cat_vars <- names(private$.kinds)[private$.kinds == "categorical"]
      private$.levels <- lapply(X[cat_vars], function(col) {
        levels(droplevels(as.factor(col)))
      })

      # 4. On code les données et on mémorise les noms des colonnes de Z
      Z <- private$encode(X)
      private$.feature_names <- colnames(Z)
      private$.is_fitted <- TRUE

      list(Z = Z, rows = rows)
    },
    
    #' @description Apply the encoding learned by `fit_transform()` to new data.
    #' @param X New predictor data frame (already checked by `check_X()`).
    #' @return A list with `Z` (numeric matrix of the complete rows) and `rows`
    #'   (indices of the rows of `X` that could be encoded).
    transform = function(X) {

      # 1. L'objet doit avoir été entraîné
      check_fitted(private$.is_fitted)

      # 2. Mêmes variables et même genre qu'au fit
      check_compatible(X, private$.kinds)

      # 3. On garde les variables du fit, dans l'ordre du fit
      X <- X[names(private$.kinds)]

      # 4. Lignes complètes uniquement (les autres auront une prédiction NA)
      rows <- which(stats::complete.cases(X))
      n_na <- nrow(X) - length(rows)
      if (n_na > 0) {
        warning(n_na, " row(s) with missing values cannot be predicted ",
                "(NA will be returned).", call. = FALSE)
      }
      X <- X[rows, , drop = FALSE]

      # 5. Modalités inconnues : codées comme la référence (que des 0)
      for (v in names(private$.levels)) {
        unknown <- setdiff(unique(as.character(X[[v]])), private$.levels[[v]])
        if (length(unknown) > 0) {
          warning("Unknown level(s) in `", v, "`: ",
                  paste(unknown, collapse = ", "),
                  ". They are treated as the reference level '",
                  private$.levels[[v]][1], "'.", call. = FALSE)
        }
      }

      # 6. Même codage qu'au fit
      list(Z = private$encode(X), rows = rows)
    }
  ),

  private = list(

    # État mémorisé au fit
    .kinds = NULL,
    .levels = NULL,
    .feature_names = NULL,
    .is_fitted = FALSE,

    # Transforme X en matrice numérique avec le codage mémorisé
    encode = function(X) {
      num_vars <- names(private$.kinds)[private$.kinds == "numeric"]
      cat_vars <- names(private$.kinds)[private$.kinds == "categorical"]

      # Variables numériques : utilisées telles quelles
      Z_num <- as.matrix(X[num_vars])

      # Variables catégorielles : une indicatrice 0/1 par modalité,
      # sauf la première (référence) pour éviter la colinéarité
      Z_cat <- lapply(cat_vars, function(v) {
        others <- private$.levels[[v]][-1]
        M <- outer(as.character(X[[v]]), others, "==") * 1
        colnames(M) <- paste0(v, "_", others)
        M
      })

      # Assemblage final en une seule matrice numérique
      Z <- do.call(cbind, c(list(Z_num), Z_cat))
      storage.mode(Z) <- "double"
      rownames(Z) <- NULL
      Z
    }
  ),

  active = list(

    #' @field kinds Kind (numeric / categorical) of each training variable.
    kinds = function(value) {
      if (!missing(value)) stop("`kinds` is read-only.", call. = FALSE)
      private$.kinds
    },

    #' @field levels Levels of each categorical variable (first = reference).
    levels = function(value) {
      if (!missing(value)) stop("`levels` is read-only.", call. = FALSE)
      private$.levels
    },

    #' @field feature_names Column names of the encoded matrix.
    feature_names = function(value) {
      if (!missing(value)) stop("`feature_names` is read-only.", call. = FALSE)
      private$.feature_names
    }
  )
)