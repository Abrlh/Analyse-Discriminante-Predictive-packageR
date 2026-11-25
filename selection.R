# install.packages("R6")
library(R6)

verif_threshold = function(value, name){
  if(length(value) == 1 && is.numeric(value) && !is.na(value) && value >= 0 && value <=1) {
    return(value)
  } else {
    stop(paste0(name, " must be a single number in [0,1]"))
  }
}

verif_iteration_number = function(value, name){
  if(length(value) == 1 && is.numeric(value) && !is.na(value) && value > 0 && value == round(value)) {
    return(value)
  } else {
    stop(paste0(name, " must be a single integer greater than zero"))
  }
}

Selection <- R6Class("Selection",
                     
  private = list(
   .slentry = NULL,
   .slstay = NULL,
   .max_iter = NULL,
   .selected = NULL,
   .history = NULL,
   .is_fitted = FALSE,
   search = function(X){
     stop("must be implemented by a sub class")
   }
  ),
                     
  public = list(
    initialize = function(slentry = 0.15, slstay = 0.15, max_iter = 100){
      
      private$.slentry <- verif_threshold(value = slentry, name = "slentry")
      private$.slstay <- verif_threshold(value = slstay, name = "slstay")
      private$.max_iter <- verif_iteration_number(value = max_iter, name = "max_iter")
      cat("initialization successful\n")
    },
    
    fit = function(X, y){
      private$.selected <- NULL
      private$.history <- NULL
      private$.is_fitted <- FALSE
      private$search(X)
      private$.is_fitted <- TRUE
      invisible(self)
    },
    
    transform = function(X){
      if(!private$.is_fitted){
        stop("the object is not fitted")
      } else {
        diff = setdiff(private$.selected, colnames(X))
        if(length(diff) > 0) {
          stop("columns not matching : ", paste(diff, collapse = ", "))
        } else {
          return(X[, private$.selected, drop = FALSE])
        }
      }
    },
    
    fit_transform = function(X, y){
      self$fit(X, y)
      return(self$transform(X))
    },
    
    print = function(...){
      cat(class(self)[1], "\n")
      cat("slentry =", private$.slentry, "\n")
      cat("slstay =",private$.slstay, "\n")
      cat("max_iter =", private$.max_iter, "\n")
      
      if(!private$.is_fitted){
        cat("not fitted yet \n")
      } else {
        cat(length(private$.selected),"selected variables :" ,"\n")
        cat(paste(private$.selected, collapse = ", "), "\n")
      }
      
      invisible(self)
    },
    
    summary = function(){
      cat(class(self)[1], "\n")
      
      if(is.null(private$.history)){
        cat("no history yet \n")
      } else {
        print(private$.history, "\n")
      }
      
      invisible(self)
    }
  ),
  
  active = list(
    slentry = function(value){
      if(missing(value)){
        return(private$.slentry)
      } else {
        stop("slentry is read-only")
      }
    },
    
    slstay = function(value){
      if(missing(value)){
        return(private$.slstay)
      } else {
        stop("slstay is read-only")
      }
    },
    
    max_iter = function(value){
      if(missing(value)){
        return(private$.max_iter)
      } else {
        stop("max_iter is read-only")
      }
    },
    
    selected = function(value){
      if(missing(value)){
        return(private$.selected)
      } else {
        stop("selected is read-only")
      }
    },
    
    history = function(value){
      if(missing(value)){
        return(private$.history)
      } else {
        stop("history is read-only")
      }
    },
    
    is_fitted = function(value){
      if(missing(value)){
        return(private$.is_fitted)
      } else {
        stop("is_fitted is read-only")
      }
    }
  )
)



SelectionStepwise <- R6Class("SelectionStepwise", inherit = Selection,
    
    private = list(
      search = function(X){
        private$.selected <- colnames(X)
      }
    ),
    
    public = list(
      initialize = function(slentry = 0.15, slstay = 0.15, max_iter = 100){
        super$initialize(slentry = slentry, slstay = slstay, max_iter = max_iter)
        if(slentry > slstay) {
          stop('slentry must be lower than or equal to slstay')
        }
      }
    )
)



#s <- Selection$new()
#s$fit(iris[, 1:4], iris$Species)
#SelectionStepwise$new(slentry = 0.2, slstay = 0.1)
#SelectionStepwise$new(slentry = 2)
#s <- SelectionStepwise$new()
#s$fit(iris[, 1:4], iris$Species)
#s$transform(iris)
#s$fit_transform(iris)
