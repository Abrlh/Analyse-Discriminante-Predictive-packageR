# Création du jeu de données `palmer_penguins` et des fichiers de l'appli.
# Source : datasets::penguins (inclus dans R depuis la version 4.5.0),
# données de Gorman, Williams et Fraser (2014), licence CC0.
# À lancer une fois, depuis la racine du projet :
#   source("data-raw/palmer_penguins.R")

palmer_penguins <- data.frame(
  species     = datasets::penguins$species,      # cible : Adelie, Chinstrap, Gentoo
  island      = datasets::penguins$island,       # catégorielle : Biscoe, Dream, Torgersen
  bill_len    = datasets::penguins$bill_len,     # longueur du bec (mm)
  bill_dep    = datasets::penguins$bill_dep,     # profondeur du bec (mm)
  flipper_len = datasets::penguins$flipper_len,  # longueur des nageoires (mm)
  body_mass   = datasets::penguins$body_mass,    # masse (g)
  sex         = datasets::penguins$sex           # catégorielle : female, male (avec NA)
)

# 1. Version R du jeu (dossier data/) : utilisable avec data(palmer_penguins)
usethis::use_data(palmer_penguins, overwrite = TRUE)

# 2. Fichiers texte (séparateur tabulation) pour l'application Shiny
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)

#    a. Le jeu complet, pour la démo « charger un fichier »
write.table(palmer_penguins, "inst/extdata/palmer_penguins.txt",
            sep = "\t", row.names = FALSE, quote = FALSE, na = "")

#    b. 12 manchots complets SANS l'espèce, pour la démo « prédire un fichier »
set.seed(2026)
complete <- which(stats::complete.cases(palmer_penguins))
new_rows <- sort(sample(complete, 12))
write.table(palmer_penguins[new_rows, -1], "inst/extdata/palmer_penguins_new.txt",
            sep = "\t", row.names = FALSE, quote = FALSE)
