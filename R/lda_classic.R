#' @description
#' Classe d'un modèle d'analyse discriminante linéaire prédictif basique
#'
#' @export
LDA_classic <- R6Class(
  "LDA_classic",
  inherit = LDA,
  public = list(
    #' @param X un ou plusieurs individus (un vecteur pour un seul individu, ou une matrice n x p)
    #' @param d NULL pour utiliser les coefficients, ou nom de la fonction de score à comparer ("d" ou "d_test")
    predict = function(X, d = NULL) {
      if (is.null(d)) {
        return(super$predict(X))
      }
      X <- matrix(as.matrix(X), ncol = private$p) # n x p, aussi pour un seul individu
      f_d <- private[[d]]
      scores <- vapply(private$classes, function(k) f_d(k, X), numeric(nrow(X)))
      scores <- matrix(scores, nrow = nrow(X)) # n x K
      return(private$classer(scores))
    },

    #' @param estimer nom de la fonction d'estimation ("estimer_W" ou "estimer_W_test")
    #' @return la matrice W (à appeler après fit)
    estimer = function(X, y, estimer = "estimer_W") {
      return(private[[estimer]](X, y))
    }
  ),
  private = list(
    estimer_W = function(X, y) {
      X <- as.matrix(X)
      nk <- rowsum(rep(1, nrow(X)), y) # effectif de chaque classe
      G <- rowsum(X, y) / as.vector(nk) # moyenne de chaque classe (K x p)
      Xc <- X - G[match(y, rownames(G)), , drop = FALSE] # écart de chaque ligne à la moyenne de sa classe
      crossprod(Xc) / (private$n - private$K)
    },
    d = function(k, X) {
      m <- private$mu_k[k, ] # vecteur de longueur p
      a <- private$W_1 %*% m # W⁻¹ μ_k, calculé une seule fois
      as.numeric(
        log(private$pi_k[[k]])
        - 0.5 * sum(m * a)
          + X %*% a # un score par individu
      )
    },

    # versions initiales, non optimisées, on garde pour les tests de perf
    estimer_W_test = function(X, y) {
      # V
      V <- lapply(split(as.data.frame(X), y), cov) # split pour couper

      # W
      W <- matrix(0, private$p, private$p)
      for (k in private$classes) {
        W <- W + (private$n_k[[k]] - 1) * V[[k]]
      }
      W / (private$n - private$K)
    },
    d_test = function(k, X) {
      m <- private$mu_k[k, , drop = FALSE] # 1 x p
      as.numeric(log(private$pi_k[[k]]) - 0.5 * m %*% private$W_1 %*% t(m)) +
        as.numeric(m %*% private$W_1 %*% t(X)) # un score par individu
    }
  )
)
