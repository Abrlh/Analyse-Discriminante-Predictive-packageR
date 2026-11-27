#' @description
#' Classe d'un modèle d'analyse discriminante linéaire pour la grande dimension :
#' W est rétrécie vers sa diagonale (shrinkage de Schäfer et Strimmer, 2005)
#'
#' @export
LDA_gde_dim <- R6Class(
  "LDA_gde_dim",
  inherit = LDA,
  active = list(
    #' @field intensite_shrinkage Intensité du shrinkage lambda (lecture seule).
    intensite_shrinkage = function() private$lambda
  ),
  private = list(
    #' @field lambda intensité du shrinkage, entre 0 et 1
    lambda = NULL,

    #' Estimation de W rétrécie vers sa diagonale
    #' @return W_lambda = (1 - lambda) W + lambda diag(W)
    estimer_W = function(X, y) {
      X <- as.matrix(X)
      n <- private$n
      K <- private$K

      # étapes 1 et 2 : résidus intra-classe et W
      nk <- rowsum(rep(1, nrow(X)), y) # effectif de chaque classe
      G <- rowsum(X, y) / as.vector(nk) # moyenne de chaque classe (K x p)
      Xc <- X - G[match(y, rownames(G)), , drop = FALSE] # résidus r_i
      W <- crossprod(Xc) / (n - K)

      # étape 3 : résidus standardisés et corrélations intra-classe
      Z <- sweep(Xc, 2, sqrt(diag(W)), "/")
      ZtZ <- crossprod(Z)
      rho <- ZtZ / (n - K)

      # étape 4 : variance estimée de chaque corrélation
      v_bar <- ZtZ / n
      somme_carres <- crossprod(Z^2) - n * v_bar^2
      var_rho <- n / ((n - K)^2 * (n - 1)) * somme_carres

      # étape 5 : lambda, sur les paires hors diagonale, borné dans [0, 1]
      hors_diag <- row(W) != col(W)
      denom <- sum(rho[hors_diag]^2)
      lambda <- if (denom == 0) 1 else sum(var_rho[hors_diag]) / denom
      private$lambda <- max(0, min(1, lambda))

      # étape 6 : covariances rétrécies, diagonale conservée
      W_lambda <- (1 - private$lambda) * W
      diag(W_lambda) <- diag(W)
      W_lambda
    }
  )
)
