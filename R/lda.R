#' @title LDA: predictive linear discriminant analysis
#' @description Abstract class of an LDA.
#' Subclasses only define how W is estimated.
#' @export
LDA <- R6Class(
  "LDA",
  public = list(
    #' @description Abstract class: cannot be created directly.
    initialize = function() {
      if (class(self)[1] == "LDA") stop("Abstract class")
    },

    #' @description Fit the model on the observations X and y.
    #' @param X matrix (n x p).
    #' @param y factor of length n.
    #' @return The object itself, invisibly.
    fit = function(X, y) {
      private$X <- X
      private$y <- y

      private$n <- nrow(private$X)
      private$p <- ncol(private$X)
      private$classes <- levels(private$y)
      private$K <- length(private$classes)

      private$n_k <- table(private$y)
      private$pi_k <- private$n_k / private$n

      sum_xi <- rowsum(private$X, private$y)
      private$mu_k <- sum_xi / as.vector(private$n_k)

      private$W <- private$estimate_W(private$X, private$y)
      private$W_1 <- solve(private$W)

      A <- private$W_1 %*% t(private$mu_k)
      a0 <- log(as.vector(private$pi_k)) - 0.5 * colSums(t(private$mu_k) * A)
      private$coeff <- rbind(intercept = a0, A)
      invisible(self)
    },
    #' @description Assign each individual of X to one of the classes.
    #' @param X one or several individuals (a vector for one individual, or a matrix n x p).
    #' @return `factor` of length n, with the same levels as y.
    predict = function(X) {
      return(private$classify(self$scores(X)))
    },
    #' @description Membership probabilities of each individual of X for each class.
    #' @param X one or several individuals (a vector for one individual, or a matrix n x p).
    #' @return `matrix` n x K.
    predict_proba = function(X) {
      scores <- self$scores(X)
      e <- exp(scores - apply(scores, 1, max))
      return(e / rowSums(e))
    },
    #' @description Scores d_k(x) of the classification functions.
    #' @param X one or several individuals (a vector for one individual, or a matrix n x p).
    #' @return `matrix` n x K.
    scores = function(X) {
      X <- matrix(as.matrix(X), ncol = private$p)
      return(cbind(1, X) %*% private$coeff)
    }
  ),
  active = list(
    #' @field n_classes Number of classes (read-only).
    n_classes = function() private$K,
    #' @field class_names Names of the classes (read-only).
    class_names = function() private$classes,
    #' @field variable_names Names of the explanatory variables (read-only).
    variable_names = function() colnames(private$X),
    #' @field n_individuals Number of training individuals (read-only).
    n_individuals = function() private$n,
    #' @field n_variables Number of variables (read-only).
    n_variables = function() private$p,
    #' @field class_sizes Size of each class (read-only).
    class_sizes = function() private$n_k,
    #' @field class_proportions Proportion of each class (read-only).
    class_proportions = function() private$pi_k,
    #' @field class_means Mean of each class (read-only).
    class_means = function() private$mu_k,
    #' @field within_covariance Within-class covariance matrix W (read-only).
    within_covariance = function() private$W,
    #' @field data_X Training explanatory variables (read-only).
    data_X = function() private$X,
    #' @field data_y Training classes (read-only).
    data_y = function() private$y,
    #' @field coefficients Coefficients of the classification functions (read-only).
    #' A matrix (p + 1) x K: the intercept, then one coefficient per variable.
    coefficients = function() private$coeff
  ),
  private = list(
    # X: Training explanatory variables.
    X = NULL,
    # y: Training classes.
    y = NULL,
    # n: Number of training individuals.
    n = NULL,
    # p: Number of variables.
    p = NULL,
    # K: Number of classes.
    K = NULL,

    # classes: Names of the classes.
    classes = NULL,

    # n_k: Size of each class.
    n_k = NULL,
    # pi_k: Proportion of each class.
    pi_k = NULL,
    # mu_k: Mean of each class, matrix K x p.
    mu_k = NULL,

    # W: Within-class covariance matrix.
    W = NULL,
    # W_1: Inverse of W.
    W_1 = NULL,
    # coeff: Coefficients of the classification functions, (p + 1) x K.
    coeff = NULL,
    estimate_W = function(X, y) stop("Abstract method"),

    # Classify individuals from their scores.
    # scores: matrix n x K of the scores d_k(x).
    # Returns: For each individual, the class with the best score (factor).
    classify = function(scores) {
      factor(private$classes[max.col(scores, ties.method = "first")], levels = private$classes)
    }
  )
)
