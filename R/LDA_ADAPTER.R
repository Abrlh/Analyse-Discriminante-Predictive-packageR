# Adaptateur PROVISOIRE entre LDA_classic (première version) et LDAClassifier.
# À supprimer quand LDA_classic aura fit/predict/predict_proba sur plusieurs individus.
LDA_ClassicAdapter <- R6Class(
  "LDA_ClassicAdapter",
  public = list(

    # Apprentissage : seuil = 1 pour garder 100 % des données en apprentissage
    # (le découpage train/test est déjà fait avant, par train_test_split)
    fit = function(Z, y) {
      private$model <- LDA_classic$new()
      private$model$fit(Z, y, seuil = 1)
      invisible(self)
    },

    # Probabilités : on classe les individus un par un,
    # puis on range les probabilités dans un tableau n x K
    predict_proba = function(Z) {
      P <- t(apply(Z, 1, function(x) private$model$predict(x)$proba))
      colnames(P) <- private$model$noms_classes
      rownames(P) <- NULL
      P
    },

    # Classes prédites : pour chaque ligne, la classe la plus probable
    predict = function(Z) {
      P <- self$predict_proba(Z)
      factor(colnames(P)[max.col(P, ties.method = "first")],
             levels = colnames(P))
    }
  ),
    private = list(
    model = NULL   # l'objet LDA_classic entraîné
  ),

  active = list(
    # Coefficients des fonctions de classement :
    # une colonne par classe, ligne 1 = constante, puis une ligne par variable
    coefficients = function() {
      m <- private$model
      if (is.null(m)) return(NULL)
      mu <- m$moyennes_classes                   # K x p : moyennes des classes
      W_inv <- solve(m$covariance_intra)         # p x p : inverse de W
      pi_k <- as.numeric(m$proportions_classes)  # K : proportions des classes

      A <- W_inv %*% t(mu)                            # p x K : coefficients
      const <- log(pi_k) - 0.5 * colSums(t(mu) * A)   # K : constantes

      out <- rbind(const, A)
      rownames(out) <- c("(Intercept)", colnames(mu))
      colnames(out) <- m$noms_classes
      out
    }
  )
)