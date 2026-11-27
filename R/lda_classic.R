#' @description
#' Classe d'un modèle d'analyse discriminante linéaire prédictif basique
#'
#' @export
LDA_classic <- R6Class(
  "LDA_classic",
  inherit = LDA,
  public = list(
    #' @param omega individu à classer
    #' @param d nom de la fonction de score à utiliser ("d" ou "d2")
    predict = function(omega, d = "d") {
      return(private$classer(omega, d))
    },
    fit = function(X, y, seuil = 0.7, seed = NULL) {
      super$fit(X, y)
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

    #' @param estimer nom de la fonction d'estimation ("estimer_W" ou "estimer_W2")
    #' @return la matrice W (à appeler après fit)
    estimer = function(X, y, estimer = "estimer_W") {
      return(private[[estimer]](X, y))
    }
  ),
  private = list(
    estimer_W = function(X, y) {
      # V
      V <- lapply(split(as.data.frame(X), y), cov) # split pour couper

      # W
      W <- matrix(0, private$p, private$p)
      for (k in private$classes) {
        W <- W + (private$n_k[[k]] - 1) * V[[k]]
      }
      W / (private$n - private$K)
    },
    d = function(k, x) {
      m <- private$mu_k[k, , drop = FALSE] # 1 x p
      x <- matrix(x, nrow = 1) # 1 x p
      as.numeric(
        log(private$pi_k[[k]])
        - 0.5 * m %*% private$W_1 %*% t(m)
          + m %*% private$W_1 %*% t(x)
      )
    },

    #'  Classement d'un individu omega
    #'  @param d nom de la fonction de score à utiliser
    #'  @return la classe qui a le meilleur score
    classer = function(x, d = "d") {
      f_d <- private[[d]]
      scores <- sapply(private$classes, function(k) f_d(k, x))
      # softmax (on retire le max pour éviter un dépassement numérique)
      proba <- exp(scores - max(scores)) / sum(exp(scores - max(scores)))
      return(list(
        yk_ = private$classes[which.max(scores)], # argmax
        scores = scores,
        proba = proba
      ))
    },
    estimer_W2 = function(X, y) {
      X <- as.matrix(X)
      nk <- rowsum(rep(1, nrow(X)), y) # effectif de chaque classe
      G <- rowsum(X, y) / as.vector(nk) # moyenne de chaque classe (K x p)
      Xc <- X - G[match(y, rownames(G)), , drop = FALSE] # écart de chaque ligne à la moyenne de sa classe
      crossprod(Xc) / (private$n - private$K)
    },
    d2 = function(k, x) {
      m <- private$mu_k[k, ] # vecteur de longueur p
      a <- private$W_1 %*% m # W⁻¹ μ_k, calculé une seule fois
      as.numeric(
        log(private$pi_k[[k]])
        - 0.5 * sum(m * a)
          + sum(as.numeric(x) * a)
      )
    }
  )
)
