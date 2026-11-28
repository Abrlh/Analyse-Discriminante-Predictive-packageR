#' @title LDA_high_dim: predictive LDA for high dimension
#' @description
#' Class of a linear discriminant analysis model for high dimension.
#' W is shrunk toward its diagonal (shrinkage of Schäfer and Strimmer, 2005).
#'
#' @export
LDA_high_dim <- R6Class(
  "LDA_high_dim",
  inherit = LDA,
  active = list(
    #' @field shrinkage_intensity Shrinkage intensity lambda (read-only).
    shrinkage_intensity = function() private$lambda
  ),
  private = list(
    # lambda: Shrinkage intensity, between 0 and 1.
    lambda = NULL,

    # Estimate W shrunk toward its diagonal.
    # Returns: W_lambda = (1 - lambda) W + lambda diag(W).
    estimate_W = function(X, y) {
      X <- as.matrix(X)
      n <- private$n
      K <- private$K

      nk <- rowsum(rep(1, nrow(X)), y)
      G <- rowsum(X, y) / as.vector(nk)
      Xc <- X - G[match(y, rownames(G)), , drop = FALSE]
      W <- crossprod(Xc) / (n - K)

      Z <- sweep(Xc, 2, sqrt(diag(W)), "/")
      ZtZ <- crossprod(Z)
      rho <- ZtZ / (n - K)

      v_bar <- ZtZ / n
      sum_squares <- crossprod(Z^2) - n * v_bar^2
      var_rho <- n / ((n - K)^2 * (n - 1)) * sum_squares

      off_diag <- row(W) != col(W)
      denom <- sum(rho[off_diag]^2)
      lambda <- if (denom == 0) 1 else sum(var_rho[off_diag]) / denom
      private$lambda <- max(0, min(1, lambda))

      W_lambda <- (1 - private$lambda) * W
      diag(W_lambda) <- diag(W)
      W_lambda
    }
  )
)
