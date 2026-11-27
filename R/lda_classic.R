#' @title LDA_classic: basic predictive LDA
#' @description
#' Class of a basic predictive linear discriminant analysis model.
#'
#' @importFrom stats cov
#' @export
LDA_classic <- R6Class(
  "LDA_classic",
  inherit = LDA,
  public = list(
    #' @description Assign each individual of X to one of the classes.
    #' @param X one or several individuals (a vector for one individual, or a matrix n x p).
    #' @param d NULL to use the coefficients, or name of the score function to compare ("d" or "d_test").
    #' @return `factor` of length n, with the same levels as y.
    predict = function(X, d = NULL) {
      if (is.null(d)) {
        return(super$predict(X))
      }
      X <- matrix(as.matrix(X), ncol = private$p)
      f_d <- private[[d]]
      scores <- vapply(private$classes, function(k) f_d(k, X), numeric(nrow(X)))
      scores <- matrix(scores, nrow = nrow(X))
      return(private$classify(scores))
    },

    #' @description Estimate W with one of the estimation functions.
    #' @param X matrix (n x p).
    #' @param y factor of length n.
    #' @param method name of the estimation function ("estimate_W" or "estimate_W_test").
    #' @return The matrix W (call it after fit).
    estimate = function(X, y, method = "estimate_W") {
      return(private[[method]](X, y))
    }
  ),
  private = list(
    estimate_W = function(X, y) {
      X <- as.matrix(X)
      nk <- rowsum(rep(1, nrow(X)), y)
      G <- rowsum(X, y) / as.vector(nk)
      Xc <- X - G[match(y, rownames(G)), , drop = FALSE]
      crossprod(Xc) / (private$n - private$K)
    },
    d = function(k, X) {
      m <- private$mu_k[k, ]
      a <- private$W_1 %*% m
      as.numeric(
        log(private$pi_k[[k]])
        - 0.5 * sum(m * a)
          + X %*% a
      )
    },

    estimate_W_test = function(X, y) {
      V <- lapply(split(as.data.frame(X), y), cov)

      W <- matrix(0, private$p, private$p)
      for (k in private$classes) {
        W <- W + (private$n_k[[k]] - 1) * V[[k]]
      }
      W / (private$n - private$K)
    },
    d_test = function(k, X) {
      m <- private$mu_k[k, , drop = FALSE]
      as.numeric(log(private$pi_k[[k]]) - 0.5 * m %*% private$W_1 %*% t(m)) +
        as.numeric(m %*% private$W_1 %*% t(X))
    }
  )
)
