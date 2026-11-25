#' @description
#' Classe abstraite d'un modèle d'analyse discirminante linéaire prédictif
#'
#' @export
LDA <- R6Class("LDA",
               public = list(
                 
                 initialize = function() {
                   if (class(self)[1] == "LDA") stop("Classe abstraite")
                 },
                 
                 #' @param X matrix
                 #' @param y factor
                 #' @param seuil proportion de train
                 fit = function(X,y,seuil = 0.7,seed = NULL) {
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
                   private$X_test  <- X[-idx, , drop = FALSE]
                   private$y_test  <- y[-idx]
                   
                   # n , p , les classes et card(classs)
                   private$n <- nrow(private$X_train)
                   private$p <- ncol(private$X_train)
                   private$classes <- levels(private$y_train)
                   private$K <- length(private$classes)
                   
                   # n_k
                   private$n_k <- table(private$y_train) 
                   
                   # pi_k
                   private$pi_k <- private$n_k/private$n
                   
                   sum_xi <- rowsum(private$X_train,private$y_train)
                   private$mu_k <-sum_xi/as.vector(private$n_k)
                   
                   private$W <- private$estimer_W(private$X_train, private$y_train)
                   private$W_1 <- solve(private$W)
                   invisible(self)
                 },
                 
                 
                 #' @return classe majoritaire pour omega
                 predict = function(omega){
                   stop("Méthode abstraite")
                 }
                 
                 
               ),
               
               private = list(
                 X_train = NULL,
                 y_train = NULL,
                 X_test  = NULL,
                 y_test  = NULL,
                 n = NULL,
                 p = NULL,
                 K = NULL,
                 classes = NULL,
                 n_k = NULL,
                 pi_k = NULL,
                 mu_k = NULL,
                 W = NULL,
                 d = function(k, x) stop("Méthode abstraite"),                 
                 W_1 = NULL,
                 estimer_W = function(X,y) stop("méthode abstraite")
               )
               
)
