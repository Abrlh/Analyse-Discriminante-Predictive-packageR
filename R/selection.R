#' Check a significance threshold
#'
#' @param value Value to check.
#' @param name Parameter name, for the error message.
#' @return `value` if it is a single number in \[0, 1\], an error otherwise.
#' @keywords internal
#' @noRd
verif_threshold <- function(value, name) {
  if (length(value) == 1 &&
    is.numeric(value) &&
    !is.na(value) && value >= 0 && value <= 1) {
    return(value)
  } else {
    stop(paste0(name, " must be a single number in [0,1]"))
  }
}

#' Check a number of iterations
#'
#' @param value Value to check.
#' @param name Parameter name, for the error message.
#' @return `value` if it is a single positive integer, an error otherwise.
#' @keywords internal
#' @noRd
verif_iteration_number <- function(value, name) {
  if (length(value) == 1 &&
    is.numeric(value) &&
    !is.na(value) && value > 0 && value == round(value)) {
    return(value)
  } else {
    stop(paste0(name, " must be a single integer greater than zero"))
  }
}

#' Within-class and total scatter matrices
#'
#' Sums of squares and cross-products, not divided by n.
#'
#' @param X Numeric matrix or data frame (n x p).
#' @param y Class labels, of length n.
#' @return A list with the p x p matrices `W` and `T`.
#' @keywords internal
#' @noRd
calc_W_T <- function(X, y) {
  X <- as.matrix(X)
  y <- as.factor(y)
  n <- nrow(X)
  p <- ncol(X)

  # T : centrage sur la moyenne globale
  Xc <- scale(X, center = TRUE, scale = FALSE)
  T_mat <- t(Xc) %*% Xc

  # W : centrage de chaque individu sur la moyenne de sa classe
  Wc <- matrix(
    data = NA,
    nrow = n,
    ncol = p,
    dimnames = list(NULL, colnames(X))
  )

  for (k in levels(y)) {
    filtre <- (y == k)
    X_k <- X[filtre, , drop = FALSE]
    Wc[filtre, ] <- sweep(X_k, MARGIN = 2, STATS = colMeans(X_k))
  }

  W_mat <- t(Wc) %*% Wc

  return(list(W = W_mat, T = T_mat))
}

#' Wilks' lambda of a set of variables
#'
#' Lambda(S) = det(W_SS) / det(T_SS), computed with log-determinants
#' (det() overflows for large sets).
#'
#' @param W,T Within-class and total scatter matrices.
#' @param vars Variable names, possibly empty.
#' @return A number in \[0, 1\], 1 for an empty set.
#' @keywords internal
#' @noRd
wilks <- function(W, T, vars) {
  if (length(vars) == 0) {
    return(1)
  } else {
    log_det_W <- determinant(W[vars, vars, drop = FALSE], logarithm = TRUE)$modulus
    log_det_T <- determinant(T[vars, vars, drop = FALSE], logarithm = TRUE)$modulus
    return(exp(as.numeric(log_det_W - log_det_T)))
  }
}

#' Partial F test from partial Wilks' lambdas
#'
#' F = ((n - K - q) / (K - 1)) * (1 - lambda_p) / lambda_p,
#' following F(K - 1, n - K - q). Vectorised over `lambda_p`.
#'
#' @param lambda_p Partial lambdas, Lambda(S + j) / Lambda(S).
#' @param q Size of S (without j).
#' @param n,K Number of observations and of classes.
#' @return A list of vectors `lambda_p`, `F` and `p_value`.
#' @keywords internal
#' @noRd
partial_stats <- function(lambda_p, q, n, K) {
  df1 <- K - 1
  df2 <- n - K - q
  F_stat <- (df2 / df1) * (1 - lambda_p) / lambda_p
  p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)

  return(list(
    lambda_p = lambda_p,
    F = F_stat,
    p_value = p_value
  ))
}


#' @title Variable selection for linear discriminant analysis (abstract class)
#'
#' @description
#' Parent class of the selection methods, used like a scikit-learn
#' transformer (`fit()`, then `transform()`). Use its subclasses:
#' [SelectionForward], [SelectionBackward], [SelectionStepwise]
#' or [SelectionANOVA].
#'
#' @details
#' Input data must be preprocessed: numeric columns with names,
#' no missing values.
#'
#' @importFrom R6 R6Class
#' @importFrom stats pf
#' @export
Selection <- R6Class(
  "Selection",
  private = list(
    .slentry = NULL,
    .slstay = NULL,
    .max_iter = NULL,
    .selected = NULL,
    .history = NULL,
    .is_fitted = FALSE,
    .W = NULL,
    .T = NULL,
    .n = NULL,
    .K = NULL,
    .tolerance = 1e-8,
    search = function(X) {
      stop("must be implemented by a sub class")
    },
    # All candidates at once:
    # Lambda_p(j|S) = (w_jj - W_jS W_SS^-1 W_Sj) / (same with T)
    step_forward = function(S, variables) {
      candidates <- setdiff(variables, S)
      if (length(candidates) == 0) {
        return(NULL)
      }

      W <- private$.W
      T <- private$.T
      w_res <- diag(W)[candidates]
      t_res <- diag(T)[candidates]

      if (length(S) > 0) {
        W_SC <- W[S, candidates, drop = FALSE]
        T_SC <- T[S, candidates, drop = FALSE]
        residuals <- tryCatch(list(
          w = colSums(W_SC * solve(W[S, S, drop = FALSE], W_SC)),
          t = colSums(T_SC * solve(T[S, S, drop = FALSE], T_SC))
        ), error = function(e) NULL) # singular matrix
        if (is.null(residuals)) {
          return(NULL)
        }
        w_res <- w_res - residuals$w
        t_res <- t_res - residuals$t
      }

      # Tolerance: skip constant or collinear candidates
      lambda_p <- w_res / t_res
      lambda_p[!(t_res > private$.tolerance * diag(T)[candidates])] <- NA

      stats <- partial_stats(lambda_p, length(S), private$.n, private$.K)
      return(private$pick(stats, candidates, which.max))
    },

    # All j in S at once: Lambda_p(j|S-j) = (T_SS^-1)_jj / (W_SS^-1)_jj
    step_backward = function(S) {
      if (length(S) == 0) {
        return(NULL)
      }

      inverses <- tryCatch(list(
        W = solve(private$.W[S, S, drop = FALSE]),
        T = solve(private$.T[S, S, drop = FALSE])
      ), error = function(e) NULL) # singular matrix
      if (is.null(inverses)) {
        return(NULL)
      }

      lambda_p <- diag(inverses$T) / diag(inverses$W)
      stats <- partial_stats(lambda_p, length(S) - 1, private$.n, private$.K)
      return(private$pick(stats, S, which.min))
    },

    # Variable with the max / min finite F (NULL if none)
    pick = function(stats, variables, choose) {
      F_finite <- ifelse(is.finite(stats$F), stats$F, NA)
      if (all(is.na(F_finite))) {
        return(NULL)
      }
      i <- choose(F_finite)
      return(list(
        lambda_p = unname(stats$lambda_p[i]),
        F = unname(stats$F[i]),
        p_value = unname(stats$p_value[i]),
        variable = variables[i]
      ))
    },

    # S = selected set after the action
    history_row = function(iter, action, stats, S) {
      data.frame(
        step = iter,
        action = action,
        variable = stats$variable,
        lambda = wilks(private$.W, private$.T, S),
        lambda_p = stats$lambda_p,
        R2_partial = 1 - stats$lambda_p,
        F = stats$F,
        p_value = stats$p_value
      )
    }
  ),
  public = list(
    #' @description
    #' Create a new selection object.
    #' @param slentry Significance level to enter, in \[0, 1\]. Default 0.15.
    #' @param slstay Significance level to stay, in \[0, 1\]. Default 0.15.
    #' @param max_iter Maximum number of iterations. Default 100.
    #' @return A new `Selection` object.
    initialize = function(slentry = 0.15,
                          slstay = 0.15,
                          max_iter = 100) {
      private$.slentry <- verif_threshold(value = slentry, name = "slentry")
      private$.slstay <- verif_threshold(value = slstay, name = "slstay")
      private$.max_iter <- verif_iteration_number(value = max_iter, name = "max_iter")
    },

    #' @description
    #' Select the variables of `X` that best separate the classes of `y`.
    #' @param X Numeric data frame or matrix (n x p).
    #' @param y Class labels, of length n, with at least 2 classes.
    #' @return The object itself, invisibly.
    fit = function(X, y) {
      private$.selected <- NULL
      private$.history <- NULL
      private$.is_fitted <- FALSE

      X <- as.matrix(X)
      y <- droplevels(as.factor(y))

      if (!is.numeric(X)) {
        stop("X must contain only numeric columns")
      }
      if (anyNA(X) || anyNA(y)) {
        stop("X and y must not contain missing values")
      }
      if (nrow(X) != length(y)) {
        stop("X and y must have the same number of rows")
      }
      if (nlevels(y) < 2) {
        stop("y must contain at least 2 classes")
      }
      if (is.null(colnames(X))) {
        stop("X must have column names")
      }

      res <- calc_W_T(X, y)
      private$.W <- res$W
      private$.T <- res$T
      private$.n <- nrow(X)
      private$.K <- nlevels(y)

      private$search(X)

      private$.is_fitted <- TRUE
      invisible(self)
    },

    #' @description
    #' Keep only the selected variables of a data set.
    #' @param X Data frame or matrix containing the selected columns.
    #' @return `X` restricted to the selected columns.
    transform = function(X) {
      if (!private$.is_fitted) {
        stop("the object is not fitted")
      } else {
        diff <- setdiff(private$.selected, colnames(X))
        if (length(diff) > 0) {
          stop("columns not matching : ", paste(diff, collapse = ", "))
        } else {
          return(X[, private$.selected, drop = FALSE])
        }
      }
    },

    #' @description
    #' Shortcut for `fit(X, y)` followed by `transform(X)`.
    #' @param X,y See `fit()`.
    #' @return `X` restricted to the selected columns.
    fit_transform = function(X, y) {
      self$fit(X, y)
      return(self$transform(X))
    },

    #' @description
    #' Print the parameters and, once fitted, the selected variables.
    #' @param ... Not used.
    #' @return The object itself, invisibly.
    print = function(...) {
      cat(class(self)[1], "\n")
      cat("slentry =", private$.slentry, "\n")
      cat("slstay =", private$.slstay, "\n")
      cat("max_iter =", private$.max_iter, "\n")

      if (!private$.is_fitted) {
        cat("not fitted yet \n")
      } else {
        cat(length(private$.selected), "selected variables :", "\n")
        cat(paste(private$.selected, collapse = ", "), "\n")
      }

      invisible(self)
    },

    #' @description
    #' Print the selection history (see `history`).
    #' @return The object itself, invisibly.
    summary = function() {
      cat(class(self)[1], "\n")

      if (is.null(private$.history)) {
        cat("no history yet \n")
      } else {
        print(private$.history)
      }

      invisible(self)
    }
  ),
  active = list(
    #' @field slentry Significance level to enter (read-only).
    slentry = function(value) {
      if (missing(value)) {
        return(private$.slentry)
      } else {
        stop("slentry is read-only")
      }
    },

    #' @field slstay Significance level to stay (read-only).
    slstay = function(value) {
      if (missing(value)) {
        return(private$.slstay)
      } else {
        stop("slstay is read-only")
      }
    },

    #' @field max_iter Maximum number of iterations (read-only).
    max_iter = function(value) {
      if (missing(value)) {
        return(private$.max_iter)
      } else {
        stop("max_iter is read-only")
      }
    },

    #' @field selected Selected variables, in order of entry (read-only).
    selected = function(value) {
      if (missing(value)) {
        return(private$.selected)
      } else {
        stop("selected is read-only")
      }
    },

    #' @field history Data frame, one row per action (read-only):
    #'   `step`, `action` (`"add"` or `"remove"`), `variable`, `lambda`
    #'   (Wilks' lambda after the action), `lambda_p` (partial lambda),
    #'   `R2_partial`, `F` and `p_value`.
    history = function(value) {
      if (missing(value)) {
        return(private$.history)
      } else {
        stop("history is read-only")
      }
    },

    #' @field is_fitted `TRUE` once `fit()` has been called (read-only).
    is_fitted = function(value) {
      if (missing(value)) {
        return(private$.is_fitted)
      } else {
        stop("is_fitted is read-only")
      }
    }
  )
)


#' @title Stepwise variable selection (STEPDISC)
#'
#' @description
#' Stepwise selection based on Wilks' lambda and the partial F test,
#' as in SAS PROC STEPDISC.
#'
#' @details
#' From an empty set, each iteration adds the variable with the largest
#' partial F if its p-value is <= `slentry` (otherwise it stops), then
#' removes the selected variable with the smallest F if its p-value
#' is > `slstay`. `slentry` must be <= `slstay`. Constant or collinear
#' variables are ignored.
#'
#' @references
#' Rakotomalala R., *Analyse discriminante linéaire*, course notes,
#' Université Lumière Lyon 2.
#'
#' SAS Institute, *The STEPDISC Procedure*, SAS/STAT User's Guide.
#'
#' @examples
#' sel <- SelectionStepwise$new(slentry = 0.15, slstay = 0.15)
#' sel$fit(iris[, 1:4], iris$Species)
#' sel$selected
#' sel$summary()
#' X_sel <- sel$transform(iris[, 1:4])
#'
#' @export
SelectionStepwise <- R6Class(
  "SelectionStepwise",
  inherit = Selection,
  private = list(
    search = function(X) {
      variables <- colnames(X)
      S <- character(0)
      history <- NULL
      iter <- 0

      while (iter < private$.max_iter) {
        iter <- iter + 1

        # Add variable
        best <- private$step_forward(S, variables)
        if (is.null(best) || best$p_value > private$.slentry) {
          break
        }
        S <- c(S, best$variable)
        history <- rbind(history, private$history_row(iter, "add", best, S))

        # Remove variable
        worst <- private$step_backward(S)
        if (!is.null(worst) && worst$p_value > private$.slstay) {
          S <- setdiff(S, worst$variable)
          history <- rbind(history, private$history_row(iter, "remove", worst, S))
        }
      }

      private$.selected <- S
      private$.history <- history
    }
  ),
  public = list(
    #' @description
    #' Create a new stepwise selection object.
    #' @param slentry Significance level to enter, in \[0, 1\]. Default 0.15.
    #' @param slstay Significance level to stay, >= `slentry`. Default 0.15.
    #' @param max_iter Maximum number of iterations. Default 100.
    #' @return A new `SelectionStepwise` object.
    initialize = function(slentry = 0.15,
                          slstay = 0.15,
                          max_iter = 100) {
      super$initialize(
        slentry = slentry,
        slstay = slstay,
        max_iter = max_iter
      )
      if (slentry > slstay) {
        stop("slentry must be lower than or equal to slstay")
      }
    }
  )
)


#' @title Forward variable selection (STEPDISC)
#'
#' @description
#' Forward selection based on Wilks' lambda and the partial F test,
#' as in SAS PROC STEPDISC (METHOD = FORWARD).
#'
#' @details
#' From an empty set, each iteration adds the variable with the largest
#' partial F if its p-value is <= `slentry`, otherwise it stops. No
#' variable is removed, so `slstay` is not used. Constant or collinear
#' variables are ignored.
#'
#' @references
#' Rakotomalala R., *Analyse discriminante linéaire*, course notes,
#' Université Lumière Lyon 2.
#'
#' SAS Institute, *The STEPDISC Procedure*, SAS/STAT User's Guide.
#'
#' @examples
#' sel <- SelectionForward$new(slentry = 0.15)
#' sel$fit(iris[, 1:4], iris$Species)
#' sel$selected
#' sel$summary()
#'
#' @export
SelectionForward <- R6Class("SelectionForward",
  inherit = Selection,
  private = list(
    search = function(X) {
      variables <- colnames(X)
      S <- character(0)
      history <- NULL
      iter <- 0

      while (iter < private$.max_iter) {
        iter <- iter + 1

        best <- private$step_forward(S, variables)
        if (is.null(best) || best$p_value > private$.slentry) {
          break
        }
        S <- c(S, best$variable)
        history <- rbind(history, private$history_row(iter, "add", best, S))
      }

      private$.selected <- S
      private$.history <- history
    }
  )
)


#' @title Backward variable selection (STEPDISC)
#'
#' @description
#' Backward elimination based on Wilks' lambda and the partial F test,
#' as in SAS PROC STEPDISC (METHOD = BACKWARD).
#'
#' @details
#' From all the variables, each iteration removes the variable with the
#' smallest partial F if its p-value is > `slstay`, otherwise it stops.
#' `slentry` is not used. Requires `n - K >= p`: for high-dimensional
#' data, use [SelectionForward] or [SelectionStepwise].
#'
#' @references
#' Rakotomalala R., *Analyse discriminante linéaire*, course notes,
#' Université Lumière Lyon 2.
#'
#' SAS Institute, *The STEPDISC Procedure*, SAS/STAT User's Guide.
#'
#' @examples
#' sel <- SelectionBackward$new(slstay = 0.15)
#' sel$fit(iris[, 1:4], iris$Species)
#' sel$selected
#' sel$summary()
#'
#' @export
SelectionBackward <- R6Class("SelectionBackward",
  inherit = Selection,
  private = list(
    search = function(X) {
      variables <- colnames(X)
      if (length(variables) > private$.n - private$.K) {
        stop("backward selection needs n - K >= p (more observations than variables)")
      }

      S <- variables
      history <- NULL
      iter <- 0

      while (iter < private$.max_iter) {
        iter <- iter + 1

        worst <- private$step_backward(S)
        if (is.null(worst) || worst$p_value <= private$.slstay) {
          break
        }
        S <- setdiff(S, worst$variable)
        history <- rbind(history, private$history_row(iter, "remove", worst, S))
      }

      private$.selected <- S
      private$.history <- history
    }
  )
)



#' @title Univariate ANOVA filter
#'
#' @description
#' Variable filter for high-dimensional data, based on a one-way ANOVA
#' F test for each variable taken alone.
#'
#' @details
#' Variables are ranked by decreasing F. Those with a p-value lower than
#' or equal to `slentry` are kept, up to `max_var` variables. The filter
#' ignores the redundancy between variables, and is typically used
#' before [SelectionStepwise] or [SelectionForward]. In `history`,
#' `action` is `"keep"` and `lambda` is `NA`. `slstay` and `max_iter`
#' are not used.
#'
#' @references
#' Rakotomalala R., *Analyse discriminante linéaire*, course notes,
#' Université Lumière Lyon 2.
#'
#' @examples
#' sel <- SelectionANOVA$new(slentry = 0.05, max_var = 2)
#' sel$fit(iris[, 1:4], iris$Species)
#' sel$selected
#' sel$summary()
#'
#' @export
SelectionANOVA <- R6Class("SelectionANOVA",
  inherit = Selection,
  private = list(
    .max_var = NULL,
    search = function(X) {
      variables <- colnames(X)
      w <- diag(private$.W)[variables]
      t <- diag(private$.T)[variables]

      # Partial lambda with an empty S, NA for constant variables
      lambda_p <- ifelse(t > 0, w / t, NA)
      stats <- partial_stats(lambda_p, 0, private$.n, private$.K)

      kept <- which(is.finite(stats$F) & stats$p_value <= private$.slentry)
      kept <- kept[order(stats$F[kept], decreasing = TRUE)]
      if (!is.null(private$.max_var)) {
        kept <- kept[seq_len(min(length(kept), private$.max_var))]
      }

      private$.selected <- variables[kept]
      if (length(kept) > 0) {
        private$.history <- data.frame(
          step = seq_along(kept),
          action = "keep",
          variable = variables[kept],
          lambda = NA_real_,
          lambda_p = unname(stats$lambda_p[kept]),
          R2_partial = unname(1 - stats$lambda_p[kept]),
          F = unname(stats$F[kept]),
          p_value = unname(stats$p_value[kept])
        )
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new ANOVA filter.
    #' @param slentry Significance level to keep a variable, in \[0, 1\].
    #'   Default 0.15.
    #' @param max_var Maximum number of variables to keep, or `NULL` for
    #'   no limit. Default `NULL`.
    #' @return A new `SelectionANOVA` object.
    initialize = function(slentry = 0.15, max_var = NULL) {
      # slstay and max_iter unused
      super$initialize(slentry = slentry, slstay = 1, max_iter = 1)
      if (!is.null(max_var)) {
        private$.max_var <- verif_iteration_number(value = max_var, name = "max_var")
      }
    }
  )
)
