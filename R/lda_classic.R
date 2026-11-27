#' @description
#' Classe  d'un modèle d'analyse discirminante linéaire prédictif basique
#'
#' @export
LDA_classic <- R6Class(
  "LDA_classic",
  inherit = LDA,
  public = list(
    predict = function(omega) {
      return(private$classer(omega))
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

    #  Classement d'un individu omega
    #  @return la classe qui a le meilleur score
    classer = function(x) {
      scores <- sapply(private$classes, function(k) private$d(k, x))
      # softmax (on retire le max pour éviter un dépassement numérique)
      proba <- exp(scores - max(scores)) / sum(exp(scores - max(scores)))
      return(list(
        yk_ = private$classes[which.max(scores)], # argmax
        scores = scores,
        proba = proba
      ))
    }
  )
)
