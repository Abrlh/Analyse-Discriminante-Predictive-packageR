### Code pour utiliser la sélection de variables (R/selection.R) :

X <- iris[, 1:4]          # data.frame ou matrice numérique, avec noms de colonnes, sans NA

y <- iris$Species         # facteur (ou vecteur), au moins 2 classes

sel <- SelectionStepwise$new(slentry = 0.15, slstay = 0.15)
sel$fit(X, y)

| nom | description |
| - | - |
| sel | paramètres + variables sélectionnées |
| sel$selected | noms des variables retenues, dans l'ordre d'entrée |
| sel$summary() | historique étape par étape (lambda, lambda partiel, R2 partiel, F, p-value) |
| sel$history | le même historique en data.frame (pour Shiny / le manager) |
| X_sel <- sel$transform(X) | X réduit aux variables retenues, même type que X

### En une ligne
X_sel <- SelectionStepwise$new()$fit_transform(X, y)

### Les deux autres méthodes (c'est la même utilisation)

Ajouts uniquement :
SelectionForward$new(slentry = 0.15)

retraits uniquement :
SelectionBackward$new(slstay = 0.15)

(nécessite n - K >= p)

Attention : faire le fit de la sélection sur le train uniquement, puis transform sur le train ET le test,
sinon la sélection "voit" le test. Comme LDA$fit fait le découpage en interne, il faudra
sans doute brancher la sélection dans le manager après le split.

Aide complète : ?SelectionStepwise (après roxygen2::roxygenise())