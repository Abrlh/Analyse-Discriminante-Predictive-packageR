#' @title LDA : Analyse discirminante linéaire prédictive
#' @description Classe abstraite d'un LDA
#' @export
LDA <- R6Class(
  "LDA",
  public = list(
    initialize = function() {
      if (class(self)[1] == "LDA") stop("Classe abstraite")
    },

    #' @description Calibrage de la classe sur les observations X et y
    #' @param X matrix
    #' @param y factor
    fit = function(X, y) {
      private$X <- X
      private$y <- y

      # n , p , les classes et card(classs)
      private$n <- nrow(private$X)
      private$p <- ncol(private$X)
      private$classes <- levels(private$y)
      private$K <- length(private$classes)

      # n_k
      private$n_k <- table(private$y)

      # pi_k
      private$pi_k <- private$n_k / private$n

      sum_xi <- rowsum(private$X, private$y)
      private$mu_k <- sum_xi / as.vector(private$n_k)

      private$W <- private$estimer_W(private$X, private$y)
      private$W_1 <- solve(private$W)

      # coefficients des fonctions de classement : d_k(x) = a_k0 + a_k' x
      A <- private$W_1 %*% t(private$mu_k) # p x K : a_k = W^-1 mu_k en colonnes
      a0 <- log(as.vector(private$pi_k)) - 0.5 * colSums(t(private$mu_k) * A) # a_k0
      private$coeff <- rbind(constante = a0, A) # (p + 1) x K
      invisible(self)
    },
    #' @description Rattache chaque individu de X à l'une des classes
    #' @param X un ou plusieurs individus (un vecteur pour un seul individu, ou une matrice n x p)
    #' @return `factor` de longueur n, avec les mêmes niveaux que y
    predict = function(X) {
      return(private$classer(self$scores(X)))
    },
    #' @description Probabilités d'appartenance de chaque individu de X à chaque classe
    #' @param X un ou plusieurs individus (un vecteur pour un seul individu, ou une matrice n x p)
    #' @return `matrix` n x K
    predict_proba = function(X) {
      scores <- self$scores(X)
      e <- exp(scores - apply(scores, 1, max)) # softmax par ligne 
      return(e / rowSums(e))
    },
    #' @description Scores d_k(x) des fonctions de classement
    #' @param X un ou plusieurs individus (un vecteur pour un seul individu, ou une matrice n x p)
    #' @return `matrix` n x K
    scores = function(X) {
      X <- matrix(as.matrix(X), ncol = private$p) # n x p, aussi pour un seul individu
      return(cbind(1, X) %*% private$coeff) # la colonne de 1 multiplie la constante a_k0
    }
  ),
  active = list(
    #' @field nb_classes Nombre de classes (lecture seule).
    nb_classes = function() private$K,
    #' @field noms_classes Noms des classes (lecture seule).
    noms_classes = function() private$classes,
    #' @field noms_variables Noms des variables explicatives (lecture seule).
    noms_variables = function() colnames(private$X),
    #' @field nb_individus Nombre d'individus d'apprentissage (lecture seule).
    nb_individus = function() private$n,
    #' @field nb_variables Nombre de variables (lecture seule).
    nb_variables = function() private$p,
    #' @field effectifs_classes Effectifs par classe (lecture seule).
    effectifs_classes = function() private$n_k,
    #' @field proportions_classes Proportions des classes (lecture seule).
    proportions_classes = function() private$pi_k,
    #' @field moyennes_classes Moyennes par classe (lecture seule).
    moyennes_classes = function() private$mu_k,
    #' @field covariance_intra Matrice de covariance intra-classe W (lecture seule).
    covariance_intra = function() private$W,
    #' @field donnees_X Variables explicatives d'apprentissage (lecture seule).
    donnees_X = function() private$X,
    #' @field donnees_y Classes d'apprentissage (lecture seule).
    donnees_y = function() private$y
    #' @field coeff_models_predictif coefficients des fonctions de classement
    coeff_models_predictif = private$coeff
  ),
  private = list(
    #' @field X
    X = NULL,
    #' @field y
    y = NULL,
    #' @field n nombre d'individu dans le jeu d'apprentissage
    n = NULL,
    #' @field p nombre de variables dans le jeu d'apprentissage
    p = NULL,
    #' @field K nombre de classe
    K = NULL,

    #' @field classes liste des classes
    classes = NULL,

    #' @field n_k matrice du nombre d'individu par classe
    n_k = NULL,
    #' @field pi_k matrice de proportion observée de la classe y_k par variables
    pi_k = NULL,
    #' @field mu_k matrice de moyenne pour les individus correspondant à la classe y_k par variables
    mu_k = NULL,

    #' @field W matrice de covariance W
    W = NULL,
    #' @field W_1 Matrice de covariance W inverse
    W_1 = NULL,
    #' @field coeff coefficients des fonctions de classement, (p + 1) x K
    coeff = NULL,
    estimer_W = function(X, y) stop("méthode abstraite"),

    #'  Classement des individus à partir de leurs scores
    #'  @param scores matrice n x K des scores d_k(x)
    #'  @return pour chaque individu, la classe qui a le meilleur score (factor)
    classer = function(scores) {
      factor(private$classes[max.col(scores, ties.method = "first")], levels = private$classes) # argmax par individu
    }
  )
)
