#' Palmer penguins: size measurements of three penguin species
#'
#' @description
#' Size measurements and sex of 344 adult penguins of three species, observed
#' on three islands of the Palmer Archipelago (Antarctica) between 2007 and
#' 2009. It is the example data set of the package: the target `species` has
#' three classes, and the predictors mix numeric measurements and categorical
#' variables, with some missing values.
#'
#' @details
#' **The three classes (species).**
#' \itemize{
#'   \item `Adelie` (152 penguins): the smallest bills; found on the three
#'     islands.
#'   \item `Chinstrap` (68 penguins): long and deep bills; found only on
#'     Dream island.
#'   \item `Gentoo` (124 penguins): the heaviest, with the longest flippers
#'     and shallow bills; found only on Biscoe island.
#' }
#' The species are well separated by the bill and flipper measurements, so a
#' linear discriminant analysis classifies them very accurately. The island
#' is also very informative, since two species live on a single island.
#'
#' **Missing values.** Two penguins have no measurement at all, and the sex
#' of eleven penguins is unknown. [LDAClassifier] removes these rows before
#' fitting, with a warning.
#'
#' **Files for the application.** Two tab-separated files are installed with
#' the package, to try [run_app()]:
#' \itemize{
#'   \item `palmer_penguins.txt`: the whole data set, to fit a model;
#'   \item `palmer_penguins_new.txt`: 12 penguins without their species, to
#'     predict a file of new observations.
#' }
#' Their location is given by
#' `system.file("extdata", "palmer_penguins.txt", package = "ldapred")`.
#'
#' @format A data frame with 344 rows and 7 variables:
#' \describe{
#'   \item{species}{Penguin species (factor: `Adelie`, `Chinstrap`,
#'     `Gentoo`). The target variable.}
#'   \item{island}{Island where the penguin was observed (factor: `Biscoe`,
#'     `Dream`, `Torgersen`).}
#'   \item{bill_len}{Bill length, in millimetres.}
#'   \item{bill_dep}{Bill depth, in millimetres.}
#'   \item{flipper_len}{Flipper length, in millimetres.}
#'   \item{body_mass}{Body mass, in grams.}
#'   \item{sex}{Sex of the penguin (factor: `female`, `male`).}
#' }
#'
#' @source Gorman K. B., Williams T. D., Fraser W. R. (2014), Ecological
#'   sexual dimorphism and environmental variability within a community of
#'   Antarctic penguins (genus *Pygoscelis*), *PLoS ONE*, 9(3), e90081.
#'   Data published under the CC0 licence; copied from the `penguins` data
#'   set of the R package `datasets` (R >= 4.5.0), see also Horst A. M.,
#'   Hill A. P., Gorman K. B. (2020), *palmerpenguins: Palmer Archipelago
#'   (Antarctica) penguin data*, R package.
#'
#' @examples
#' # Classes and predictors
#' table(palmer_penguins$species)
#' summary(palmer_penguins)
#'
#' # LDA with numeric and categorical predictors (incomplete rows removed)
#' model <- LDAClassifier$new()
#' model$fit(palmer_penguins[, -1], palmer_penguins$species)
#' model$summary()
"palmer_penguins"
