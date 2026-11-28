### Code pour utiliser la sélection de variables (R/selection.R) :

```r
X <- iris[, 1:4]          # data.frame ou matrice numérique, avec noms de colonnes, sans NA
y <- iris$Species         # facteur (ou vecteur), au moins 2 classes

sel <- SelectionStepwise$new(slentry = 0.15, slstay = 0.15)
sel$fit(X, y)
```

| nom | description |
| - | - |
| sel | paramètres + variables sélectionnées |
| sel$selected | noms des variables retenues, dans l'ordre d'entrée |
| sel$summary() | historique étape par étape (lambda, lambda partiel, R2 partiel, F, p-value) |
| sel$history | le même historique en data.frame (pour Shiny / le manager) |
| X_sel <- sel$transform(X) | X réduit aux variables retenues, même type que X |

### En une ligne

```r
X_sel <- SelectionStepwise$new()$fit_transform(X, y)
```

### Les méthodes disponibles (même utilisation pour toutes)

| classe | principe | paramètres | quand l'utiliser |
| - | - | - | - |
| SelectionStepwise | ajout puis retrait à chaque tour (STEPDISC) | slentry <= slstay, max_iter | cas général, méthode de référence |
| SelectionForward | ajouts uniquement (STEPDISC) | slentry, max_iter | cas général, marche en grande dimension |
| SelectionBackward | retraits uniquement, part de toutes les variables (STEPDISC) | slstay, max_iter | petit p, nécessite n - K >= p |
| SelectionANOVA | un F d'ANOVA par variable, prise seule | slentry, max_var | grande dimension, seule ou en pré-filtre |

```r
SelectionForward$new(slentry = 0.15)
SelectionBackward$new(slstay = 0.15)
SelectionANOVA$new(slentry = 0.05, max_var = NULL)
```

Paramètres (tous fixés à la création, en lecture seule ensuite) :

- slentry : une variable entre si la p-value de son F partiel est <= slentry (défaut 0.15, comme SAS)
- slstay : une variable sélectionnée sort si sa p-value devient > slstay (défaut 0.15)
- max_iter : nombre maximal de tours (défaut 100)
- max_var (ANOVA uniquement) : nombre maximal de variables gardées (défaut NULL, pas de limite)

### Grande dimension

- Backward est refusé si p > n - K (le modèle complet n'est pas calculable).
- Avec beaucoup de variables, STEPDISC au seuil 0.15 garde des variables de bruit (sur Madelon, 500 variables : 88 retenues, pas mieux que 2 ou 3). Deux solutions :
  - un seuil plus strict : `SelectionStepwise$new(slentry = 0.001, slstay = 0.001)`
  - un pré-filtre ANOVA, puis STEPDISC sur les variables restantes :

```r
X_filtre <- SelectionANOVA$new(max_var = 50)$fit_transform(X, y)
sel <- SelectionStepwise$new()$fit(X_filtre, y)
```

- SelectionANOVA ignore la redondance entre variables (deux variables très corrélées peuvent être gardées toutes les deux) : c'est un filtre, pas un remplaçant de STEPDISC donc dans l'idéal il faut faire tourner l'un PUIS l'autre dans le cas de la grande dimension.
- Sur ce dernier point je dirai qu'il faut laisser le choix à l'utilisateur de s'il veut faire une double sélection ANOVA + stepwise dans le cas de la grande dim. ou juste une sélection unique avec ANOVA, au risque qu'elle soit moins précise.
