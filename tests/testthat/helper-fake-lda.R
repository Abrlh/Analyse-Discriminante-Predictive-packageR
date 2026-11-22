# Fausse LDA utilisée uniquement dans les tests de LDAClassifier, en attendant
# l'implémentation de LDA_Classic par la partie LDA.
# Elle remplace temporairement les méthodes de LDA_Classic (classement au
# centre de classe le plus proche), exécute le code, puis restaure la classe.
with_fake_lda <- function(code) {
  gen <- LDA_Classic
  old_public <- gen$public_methods
  old_private <- gen$private_fields
  on.exit({
    gen$public_methods <- old_public
    gen$private_fields <- old_private
  }, add = TRUE)

  gen$set("private", ".fake", NULL, overwrite = TRUE)

  gen$set("public", "fit", function(Z, y) {
    means <- rowsum(Z, y) / as.vector(table(y))
    private$.fake <- list(classes = levels(y), means = means)
    invisible(self)
  }, overwrite = TRUE)

  gen$set("public", "predict_proba", function(Z) {
    m <- private$.fake$means
    d <- sapply(seq_len(nrow(m)), function(k) rowSums(sweep(Z, 2, m[k, ])^2))
    d <- matrix(d, nrow = nrow(Z))
    s <- exp(-(d - apply(d, 1, min)))
    P <- s / rowSums(s)
    colnames(P) <- private$.fake$classes
    P
  }, overwrite = TRUE)

  gen$set("public", "predict", function(Z) {
    P <- self$predict_proba(Z)
    factor(colnames(P)[max.col(P)], levels = colnames(P))
  }, overwrite = TRUE)

  force(code)
}
