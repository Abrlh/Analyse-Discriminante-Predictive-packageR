#' Check a significance threshold
#'
#' @param value Value to check.
#' @param name Name of the parameter, used in the error message.
#' @return `value`, unchanged, if it is a single number in \[0, 1\].
#'   Otherwise an error is raised.
#' @keywords internal
#' @noRd
verif_threshold = function(value, name) {
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
#' @param name Name of the parameter, used in the error message.
#' @return `value`, unchanged, if it is a single strictly positive integer.
#'   Otherwise an error is raised.
#' @keywords internal
#' @noRd
verif_iteration_number = function(value, name) {
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
#' Computes the within-class scatter matrix W and the total scatter
#' matrix T, as sums of squares and cross-products (not divided by n).
#' They satisfy T = W + B, where B is the between-class scatter matrix.
#'
#' @param X Numeric matrix or data frame (n rows, p columns).
#' @param y Factor or vector of class labels, of length n.
#' @return A list with two p x p symmetric matrices: `W` (within-class)
#'   and `T` (total), with the column names of `X` as dimnames.
#' @keywords internal
#' @noRd
calc_W_T = function(X, y) {
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
#' Lambda(S) = det(W_SS) / det(T_SS), where W_SS and T_SS are the
#' sub-matrices of W and T restricted to the variables of S.
#' The closer to 0, the better the variables separate the classes.
#'
#' @param W Within-class scatter matrix (see `calc_W_T()`).
#' @param T Total scatter matrix (see `calc_W_T()`).
#' @param vars Character vector of variable names. May be empty.
#' @return A single number in \[0, 1\]. By convention, 1 for an empty set.
#' @keywords internal
#' @noRd
wilks = function(W, T, vars) {
  if (length(vars) == 0) {
    return(1)
  } else {
    return(det(W[vars, vars, drop = FALSE]) / det(T[vars, vars, drop = FALSE]))
  }
}

#' Partial Wilks' lambda and F test of a variable
#'
#' Measures what variable `j` adds to the set `S`:
#' lambda_p = Lambda(S + j) / Lambda(S), and
#' F = ((n - K - q) / (K - 1)) * (1 - lambda_p) / lambda_p,
#' which follows an F(K - 1, n - K - q) distribution, with q = |S|.
#' Used for entry (j not in S) and for removal (S = selected set minus j).
#'
#' @param W Within-class scatter matrix.
#' @param T Total scatter matrix.
#' @param S Character vector of variables already in the model (without `j`).
#' @param j Name of the tested variable.
#' @param n Number of observations.
#' @param K Number of classes.
#' @return A list with `lambda_p` (partial lambda), `F` (F statistic)
#'   and `p_value`.
#' @keywords internal
#' @noRd
partial_stats = function(W, T, S, j, n, K) {
  q <- length(S)
  lambda_p <- wilks(W, T, c(S, j)) / wilks(W, T, S)
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
#' Abstract parent class of the variable selection methods based on
#' Wilks' lambda (STEPDISC family). It works like a scikit-learn
#' transformer: `fit()` chooses the variables, `transform()` keeps
#' only those variables in a data set.
#'
#' This class is not used directly: use one of its subclasses,
#' [SelectionForward], [SelectionBackward] or [SelectionStepwise].
#'
#' @details
#' Input data must already be preprocessed: numeric columns only,
#' no missing values, column names required.
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
    
    search = function(X) {
      stop("must be implemented by a sub class")
    },
    
    step_forward = function(S, variables) {
      candidates <- setdiff(variables, S)
      if (length(candidates) == 0) {
        return(NULL)
      }
      
      best <- NULL
      for (j in candidates) {
        stats <- partial_stats(private$.W, private$.T, S, j, private$.n, private$.K)
        
        if (!is.finite(stats$F)) {
          next
        }
        
        if (is.null(best) || stats$F > best$F) {
          stats$variable <- j
          best <- stats
        }
      }
      return(best)
    },
    
    step_backward = function(S) {
      if (length(S) == 0) {
        return(NULL)
      }
      
      worst <- NULL
      for (j in S) {
        stats <- partial_stats(private$.W,
                               private$.T,
                               setdiff(S, j),
                               j,
                               private$.n,
                               private$.K)
        
        if (!is.finite(stats$F)) {
          next
        }
        
        if (is.null(worst) || stats$F < worst$F) {
          stats$variable <- j
          worst <- stats
        }
      }
      return(worst)
    },
    
    # One row of the history: action on a variable, given the stats
    # returned by step_forward / step_backward and the set S after the action
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
    #' @param slentry Significance level to enter a variable, in \[0, 1\].
    #'   A variable enters if the p-value of its partial F test is lower
    #'   than or equal to `slentry`. Default 0.15 (as in SAS PROC STEPDISC).
    #' @param slstay Significance level to stay, in \[0, 1\].
    #'   A selected variable is removed if its p-value becomes greater
    #'   than `slstay`. Default 0.15.
    #' @param max_iter Maximum number of iterations, a strictly positive
    #'   integer. Default 100.
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
    #' Previous results are reset at each call.
    #' @param X Numeric data frame or matrix (n rows, p columns), with
    #'   column names and no missing values.
    #' @param y Factor or vector of class labels, of length n, with at
    #'   least 2 classes and no missing values.
    #' @return The object itself, invisibly (so calls can be chained).
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
    #' @param X Data frame or matrix containing at least the selected
    #'   columns (for example a test set with the same variables).
    #' @return `X` restricted to the selected columns, in selection order,
    #'   with the same type as `X`.
    transform = function(X) {
      if (!private$.is_fitted) {
        stop("the object is not fitted")
      } else {
        diff = setdiff(private$.selected, colnames(X))
        if (length(diff) > 0) {
          stop("columns not matching : ", paste(diff, collapse = ", "))
        } else {
          return(X[, private$.selected, drop = FALSE])
        }
      }
    },
    
    #' @description
    #' Shortcut for `fit(X, y)` followed by `transform(X)`.
    #' @param X Numeric data frame or matrix (see `fit()`).
    #' @param y Factor or vector of class labels (see `fit()`).
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
        cat(length(private$.selected), "selected variables :" , "\n")
        cat(paste(private$.selected, collapse = ", "), "\n")
      }
      
      invisible(self)
    },
    
    #' @description
    #' Print the step-by-step history of the selection (see the `history`
    #' field).
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
    
    #' @field selected Character vector of the selected variables, in
    #'   order of entry. `NULL` before `fit()` (read-only).
    selected = function(value) {
      if (missing(value)) {
        return(private$.selected)
      } else {
        stop("selected is read-only")
      }
    },
    
    #' @field history Data frame with one row per action (read-only),
    #'   `NULL` before `fit()` or if no variable entered. Columns:
    #'   * `step`: iteration number.
    #'   * `action`: `"add"` or `"remove"`.
    #'   * `variable`: variable added or removed.
    #'   * `lambda`: Wilks' lambda of the selected set *after* the action.
    #'   * `lambda_p`: partial Wilks' lambda of the variable.
    #'   * `R2_partial`: partial R-squared, `1 - lambda_p`.
    #'   * `F`: partial F statistic.
    #'   * `p_value`: p-value of the partial F test.
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
#' Stepwise selection of variables for linear discriminant analysis,
#' based on Wilks' lambda and the partial F test, as in SAS
#' PROC STEPDISC.
#'
#' @details
#' Starting from an empty set, each iteration:
#' 1. **Entry**: among the variables not yet selected, takes the one
#'    with the largest partial F. It enters if its p-value is lower
#'    than or equal to `slentry`. Otherwise the selection stops.
#' 2. **Removal**: among the selected variables, takes the one with
#'    the smallest partial F. It is removed if its p-value is greater
#'    than `slstay`.
#'
#' The selection also stops when every variable is selected or after
#' `max_iter` iterations. `slentry` must be lower than or equal to
#' `slstay`, otherwise a variable could enter and leave in a loop.
#'
#' Variables whose partial F cannot be computed (constant variable,
#' variable collinear with the selected ones) are ignored.
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
#'
#' # Keep only the selected variables
#' X_sel <- sel$transform(iris[, 1:4])
#' head(X_sel)
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
    #' @param slstay Significance level to stay, in \[0, 1\], greater than
    #'   or equal to `slentry`. Default 0.15.
    #' @param max_iter Maximum number of iterations. Default 100.
    #' @return A new `SelectionStepwise` object.
    initialize = function(slentry = 0.15,
                          slstay = 0.15,
                          max_iter = 100) {
      super$initialize(slentry = slentry,
                       slstay = slstay,
                       max_iter = max_iter)
      if (slentry > slstay) {
        stop('slentry must be lower than or equal to slstay')
      }
    }
  )
)



#' @title Forward variable selection (STEPDISC)
#'
#' @description
#' Forward selection of variables for linear discriminant analysis,
#' based on Wilks' lambda and the partial F test, as in SAS
#' PROC STEPDISC (METHOD = FORWARD).
#'
#' @details
#' Starting from an empty set, each iteration takes, among the variables
#' not yet selected, the one with the largest partial F. It enters if its
#' p-value is lower than or equal to `slentry`. Otherwise the selection
#' stops. A variable that has entered is never removed, so `slstay` is
#' not used.
#'
#' The selection also stops when every variable is selected or after
#' `max_iter` iterations. Variables whose partial F cannot be computed
#' (constant variable, variable collinear with the selected ones) are
#' ignored.
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
                            ))



#' @title Backward variable selection (STEPDISC)
#'
#' @description
#' Backward elimination of variables for linear discriminant analysis,
#' based on Wilks' lambda and the partial F test, as in SAS
#' PROC STEPDISC (METHOD = BACKWARD).
#'
#' @details
#' Starting from all the variables, each iteration takes, among the
#' selected variables, the one with the smallest partial F. It is removed
#' if its p-value is greater than `slstay`. Otherwise the selection
#' stops. A variable that has been removed never comes back, so
#' `slentry` is not used.
#'
#' The selection also stops when no variable is left or after `max_iter`
#' iterations. Backward elimination starts from the full model, so it
#' needs more observations than variables: `n - K >= p`, where `K` is
#' the number of classes. For high-dimensional data, use
#' [SelectionForward] or [SelectionStepwise].
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
                             ))