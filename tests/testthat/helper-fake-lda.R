# Fausse LDA utilisée uniquement pour tester LDAClassifier (et l'appli), en
# attendant les vraies classes LDA de la partie LDA.
# Classement au centre de classe le plus proche.
FakeLDA <- R6::R6Class("FakeLDA",
  public = list(
    fit = function(Z, y) {
      private$.means <- rowsum(Z, y) / as.vector(table(y))
      private$.classes <- levels(y)
      invisible(self)
    },
    predict_proba = function(Z) {
      m <- private$.means
      d <- sapply(seq_len(nrow(m)), function(k) rowSums(sweep(Z, 2, m[k, ])^2))
      d <- matrix(d, nrow = nrow(Z))
      s <- exp(-(d - apply(d, 1, min)))
      P <- s / rowSums(s)
      colnames(P) <- private$.classes
      P
    },
    predict = function(Z) {
      P <- self$predict_proba(Z)
      factor(colnames(P)[max.col(P)], levels = colnames(P))
    }
  ),
  private = list(.means = NULL, .classes = NULL)
)

# Exécute du code avec LDAClassifier branché sur FakeLDA, puis restaure
# LDAClassifier à l'identique.
with_fake_lda <- function(code) {
  gen <- LDAClassifier
  old_private <- gen$private_methods
  on.exit(gen$private_methods <- old_private, add = TRUE)
  gen$set("private", "new_lda", function() FakeLDA$new(), overwrite = TRUE)
  force(code)
}

# Exécute du code avec LDAClassifier branché sur FakeLDA, puis restaure
# LDAClassifier à l'identique.
# FakeLDA est transmis par une option R : une méthode ajoutée avec $set()
# ne voit que le namespace du package, pas l'environnement des tests.
with_fake_lda <- function(code) {
  gen <- LDAClassifier
  old_private <- gen$private_methods
  old_opt <- options(ldapred.fake_lda = FakeLDA)
  on.exit({
    gen$private_methods <- old_private
    options(old_opt)
  }, add = TRUE)
  gen$set("private", "new_lda", function() getOption("ldapred.fake_lda")$new(), overwrite = TRUE)
  force(code)
}