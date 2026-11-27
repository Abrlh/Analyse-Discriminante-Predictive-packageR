#' @title LDA : Analyse discirminante linéaire prédictive
#' @description Classe abstraite d'un LDA
#' @export
LDA <- R6Class(
  "LDA",
  public = list(
    initialize = function() {
      if (class(self)[1] == "LDA") stop("Classe abstraite")
    },

    #' @description Calibrage de la classe et découpage pour les observations X et y
    #' @param X matrix
    #' @param y factor
    #' @param seuil proportion de train
    fit = function(X, y, seuil = 0.7, seed = NULL) {
      if (is.null(seed)) {
        set.seed(54)
      } else {
        set.seed(seed)
      }

      # découpage stratifié : seuil % de chaque classe dans le train
      idx <- unlist(lapply(split(seq_along(y), y), function(i) {
        i[sample.int(length(i), round(length(i) * seuil))]
      }))

      private$X_train <- X[idx, , drop = FALSE]
      private$y_train <- y[idx]
      private$X_test <- X[-idx, , drop = FALSE]
      private$y_test <- y[-idx]

      # n , p , les classes et card(classs)
      private$n <- nrow(private$X_train)
      private$p <- ncol(private$X_train)
      private$classes <- levels(private$y_train)
      private$K <- length(private$classes)

      # n_k
      private$n_k <- table(private$y_train)

      # pi_k
      private$pi_k <- private$n_k / private$n

      sum_xi <- rowsum(private$X_train, private$y_train)
      private$mu_k <- sum_xi / as.vector(private$n_k)

      private$W <- private$estimer_W(private$X_train, private$y_train)
      private$W_1 <- solve(private$W)
      invisible(self)
    },
    #' @description Renvoie la classe majoritaire pour omega
    #' @return `matrix`
    predict = function(omega) {
      stop("Méthode abstraite")
    }
  ),
  active = list(
    #' @field nb_classes Nombre de classes (lecture seule).
    nb_classes = function() private$K,
    #' @field noms_classes Noms des classes (lecture seule).
    noms_classes = function() private$classes,
    #' @field noms_variables Noms des variables explicatives (lecture seule).
    noms_variables = function() colnames(private$X_train),
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
    covariance_intra = function() private$W
  ),
  private = list(
    #' @field X_train
    X_train = NULL,
    #' @field y_train
    y_train = NULL,
    #' @field X_test
    X_test = NULL,
    #' @field y_test
    y_test = NULL,
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

    #' @field d Fonction de classement
    d = function(k, x) stop("Méthode abstraite"),

    #' @field W matrice de covariance W
    W = NULL,
    #' @field W_1 Matrice de covariance W transposée
    W_1 = NULL,
    estimer_W = function(X, y) stop("méthode abstraite")
  )
)
