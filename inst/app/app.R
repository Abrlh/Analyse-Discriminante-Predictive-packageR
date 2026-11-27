library(shiny)
library(bslib)

# Chargement du package (sauf s'il est déjà chargé par devtools::load_all())
if (!"ldapred" %in% loadedNamespaces()) library(ldapred)

# Thème de l'application : les couleurs de la maquette
app_theme <- bs_theme(
  version = 5,
  primary = "#4f46e5",    # indigo : couleur principale
  success = "#0f766e",    # sarcelle
  warning = "#b45309",    # ambre
  info    = "#6d28d9",    # violet
  base_font = font_collection("Inter", "Segoe UI", "system-ui", "sans-serif"),
  code_font = font_collection("JetBrains Mono", "Consolas", "monospace"),
  "border-radius" = "0.75rem",
  "card-border-radius" = "1rem"
)

# Palettes proposées dans le menu « Thème » (couleur principale)
palettes <- c(
  "Indigo"       = "#4f46e5",
  "Sarcelle"     = "#0f766e",
  "Violet"       = "#7c3aed",
  "Framboise"    = "#be123c",
  "Orange brûlé" = "#c2410c"
)

# Style sur mesure, pour se rapprocher de la maquette
app_theme <- bs_add_rules(app_theme, "
  /* Cadres dont le contenu (menus déroulants) peut dépasser sans être coupé */
  .card.overflow-visible, .card.overflow-visible > .card-body {
    overflow: visible !important;
  }

  /* Barre de chargement du fichier : plus haute, texte lisible */
  .shiny-file-input-progress {
    height: 1.75rem !important; margin-top: .5rem; border-radius: .6rem;
  }
  .shiny-file-input-progress .progress-bar {
    font-size: .9rem; font-weight: 600; line-height: 1.75rem;
  }

  /* Fond de page gris-bleu clair (mode clair), cartes blanches qui ressortent */
  [data-bs-theme='light'] body { background-color: #f4f5fb; }

  /* Cartes : bordure légère, ombre douce, titres en gras */
  .card {
    border: 1px solid var(--bs-border-color-translucent);
    box-shadow: 0 1px 2px rgba(15,23,42,.04), 0 8px 24px rgba(15,23,42,.06);
  }
  .card-header {
    background: transparent; border-bottom: 0;
    font-weight: 700; padding-top: 1.1rem;
  }

  /* Barre d'onglets colorée (couleur principale du thème) */
  .navbar {
    background-color: var(--bs-primary) !important;
    border-bottom: 0;
    box-shadow: 0 6px 20px rgba(var(--bs-primary-rgb), .30);
    padding-top: 1rem; padding-bottom: 1rem;
  }
  .navbar .navbar-brand { color: #ffffff !important; }
  .navbar .nav-link {
    color: rgba(255,255,255,.85) !important;
    border-radius: .75rem; padding: .75rem 2.1rem !important;  /* cadre plus large */
    font-weight: 600; margin: 0 .3rem;
    display: inline-flex; align-items: center; gap: .55rem;
    transition: background-color .18s ease, color .18s ease,
                transform .18s ease, box-shadow .18s ease;
  }
  .navbar .nav-link svg { transition: transform .18s ease; }
  /* Plus d'espace : entre le logo et les onglets, et entre les onglets */
  .navbar .navbar-brand { margin-right: 2.5rem; }
  .navbar .navbar-nav { gap: .35rem; }
  /* Icônes des indicateurs (cartes blanches) : couleur du thème, sans dégradé.
     bslib leur met sinon un dégradé de couleurs fixes, qui ignore le thème. */
  .bslib-value-box:not(.bg-primary) .value-box-showcase,
  .bslib-value-box:not(.bg-primary) .value-box-showcase svg {
    background: none !important;
    -webkit-text-fill-color: currentColor !important;
    color: var(--bs-primary) !important;
  }
  .bslib-value-box:not(.bg-primary) .value-box-showcase svg,
  .bslib-value-box:not(.bg-primary) .value-box-showcase svg * {
    fill: var(--bs-primary) !important;
  }
  /* Survol : l'onglet s'éclaircit, se soulève et son icône grossit un peu */
  .navbar .nav-link:hover {
    background: rgba(255,255,255,.18); color: #ffffff !important;
    transform: translateY(-2px);
    box-shadow: 0 6px 14px rgba(0,0,0,.18);
  }
  .navbar .nav-link:hover svg { transform: scale(1.15); }
  /* Onglet actif : carré arrondi blanc */
  .navbar .nav-link.active {
    background: #ffffff !important; color: var(--bs-primary) !important;
    box-shadow: 0 4px 12px rgba(0,0,0,.15);
  }
  .navbar .navbar-toggler { border-color: rgba(255,255,255,.6); }
  .navbar .navbar-toggler-icon { filter: invert(1) brightness(2); }
  .navbar bslib-input-dark-mode { color: #ffffff; }

  /* Logo de la barre */
  .brand-logo {
    width: 2.6rem; height: 2.6rem; border-radius: .8rem;
    background: #ffffff; color: var(--bs-primary);
    display: inline-flex; align-items: center; justify-content: center;
  }

  /* Menu des palettes */
  .palette-dot {
    display: inline-block; width: 14px; height: 14px; border-radius: 50%;
    margin-right: .6rem; vertical-align: -2px;
  }

  /* Onglet Aide : cartes d'étapes et questions en gras */
  .step-card { transition: transform .15s ease, box-shadow .15s ease; }
  .step-card:hover { transform: translateY(-4px);
    box-shadow: 0 12px 28px rgba(15,23,42,.12); }
  .step-icon {
    width: 3.2rem; height: 3.2rem; border-radius: 1rem;
    display: inline-flex; align-items: center; justify-content: center;
  }
  .accordion-button { font-weight: 700; }

  /* Indicateurs clés : contour visible (aussi en mode sombre) et animation */
  .bslib-value-box {
    border: 1px solid var(--bs-border-color) !important;
    transition: transform .15s ease, box-shadow .15s ease;
  }
  .bslib-value-box:hover {
    transform: translateY(-4px);
    box-shadow: 0 12px 28px rgba(0,0,0,.18);
  }

  /* Mode sombre : contours des cartes plus visibles */
  [data-bs-theme='dark'] .card,
  [data-bs-theme='dark'] .bslib-value-box {
    border-color: rgba(255,255,255,.16) !important;
    box-shadow: 0 1px 2px rgba(0,0,0,.3), 0 8px 24px rgba(0,0,0,.35);
  }

  /* Boutons et champs plus arrondis */
  .btn { font-weight: 600; border-radius: .65rem; }
  .selectize-input, .form-control, .form-select { border-radius: .65rem; }

  /* Variables choisies affichées comme des puces colorées */
  .selectize-control.multi .selectize-input > div {
    background: rgba(var(--bs-primary-rgb), .12); color: var(--bs-primary);
    border-radius: 999px; padding: 2px 10px; font-weight: 600;
  }

  /* Bloc de description du modèle */
  pre {
    background: var(--bs-tertiary-bg); border: 0;
    border-radius: .75rem; padding: 1rem;
  }
")

# ---------------------------------------------------------------------------
# Fonctions utilitaires
# ---------------------------------------------------------------------------

# Traduction française des tableaux DT
dt_french <- list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json")

# Lecture d'un fichier de données (texte ou xlsx).
# Renvoie un data.frame, ou NULL (avec une notification) en cas d'échec.
read_data <- function(path, name, sep, dec) {
  ext <- tolower(tools::file_ext(name))
  df <- tryCatch({
    if (ext == "xlsx") {
      as.data.frame(readxl::read_excel(path))
    } else {
      read.table(path, header = TRUE, sep = sep, dec = dec,
                 na.strings = c("NA", ""))
    }
  }, error = function(e) {
    showNotification(paste("Erreur lors de la lecture du fichier :",
                           conditionMessage(e)), type = "error")
    NULL
  })
  if (is.null(df)) return(NULL)

  # Détection des colonnes texte qui ressemblent à des nombres avec une
  # virgule décimale (séparateur décimal probablement mal choisi)
  # Une colonne est concernée si presque toutes ses valeurs sont des nombres
  # (avec ou sans virgule : "5,1", "3", "-2,75") ET si au moins une valeur
  # contient une virgule
  looks_numeric <- vapply(df, function(col) {
    if (!is.character(col)) return(FALSE)
    values <- trimws(col[!is.na(col)])
    if (length(values) == 0) return(FALSE)
    mean(grepl("^-?[0-9]+(,[0-9]+)?$", values)) > 0.9 &&
      any(grepl(",", values, fixed = TRUE))
  }, logical(1))
  if (any(looks_numeric)) {
    # Conversion automatique : virgule remplacée par un point, puis en nombre
    df[looks_numeric] <- lapply(df[looks_numeric], function(col) {
      as.numeric(sub(",", ".", col, fixed = TRUE))
    })
    showNotification(
      paste0("Les colonnes ", paste(names(df)[looks_numeric], collapse = ", "),
             " contenaient des nombres avec une virgule décimale : elles ont ",
             "été converties en numériques. Vérifiez le séparateur décimal."),
      type = "warning", duration = NULL)
  }

  # Les colonnes texte deviennent des facteurs (nécessaire pour la cible)
  df[] <- lapply(df, function(col) if (is.character(col)) factor(col) else col)
  df
}

# Exécute un calcul du package sans jamais faire planter l'appli.
# Les avertissements deviennent des notifications orange, les erreurs des
# notifications rouges ; en cas d'erreur, la fonction renvoie NULL.
safe_run <- function(expr, error_prefix) {
  tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      showNotification(conditionMessage(w), type = "warning")
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      showNotification(paste(error_prefix, conditionMessage(e)),
                       type = "error", duration = NULL)
      NULL
    }
  )
}

# Une couleur par classe, dans l'ordre des classes du modèle
# (sarcelle, ambre, violet, framboise, bleu, vert... recyclées si besoin)
class_colors <- function(classes) {
  pal <- c("#0f766e", "#b45309", "#6d28d9", "#be123c", "#0369a1", "#4d7c0f")
  stats::setNames(rep(pal, length.out = length(classes)), classes)
}

# Une couleur par modalité d'une variable catégorielle illustrative
level_colors <- function(levels) {
  pal <- c("#4f46e5", "#0ea5e9", "#f59e0b", "#10b981", "#ec4899", "#64748b")
  stats::setNames(rep(pal, length.out = length(levels)), levels)
}

# Une ligne de graphique en barre horizontale :
# étiquette à gauche, barre au milieu (share entre 0 et 1), texte à droite
bar_row <- function(label, share, color, text) {
  div(class = "d-flex align-items-center gap-3 mb-2",
    span(class = "fw-semibold text-truncate", style = "width: 110px;", label),
    div(class = "flex-grow-1 rounded-pill",
        style = "height: 14px; background: var(--bs-tertiary-bg); overflow: hidden;",
        div(class = "rounded-pill",
            style = sprintf("height: 14px; width: %.1f%%; background: %s;",
                            100 * share, color))),
    span(class = "fw-bold text-end",
         style = "width: 64px; font-variant-numeric: tabular-nums;", text)
  )
}

# Libellé suivi d'une icône (i) qui affiche une explication au survol
label_info <- function(label, info) {
  tagList(label, " ",
          tooltip(bsicons::bs_icon("info-circle", class = "text-body-secondary"),
                  info))
}

# Libellés français des paramètres (récapitulatif)
method_labels <- c(classic = "Classique", highdim = "Grande dimension")
selection_labels <- c(none = "Aucune", forward = "Ascendante (forward)",
                      backward = "Descendante (backward)",
                      stepwise = "Pas à pas (stepwise)")

# Carte d'aide affichée à la place d'un contenu vide
empty_state <- function(icon, title, text, button = NULL) {
  card(card_body(
    class = "text-center py-5",
    div(class = "mb-3 text-primary", bsicons::bs_icon(icon, size = "3rem")),
    h4(class = "fw-bold", title),
    p(class = "text-body-secondary mx-auto", style = "max-width: 560px;", text),
    button
  ))
}

# Carte d'une étape de l'onglet Aide, avec un bouton vers l'onglet concerné
step_card <- function(n, icon, color, title, text, button_id, button_label) {
  card(
    class = "step-card h-100", fill = FALSE,
    card_body(
      fillable = FALSE,
      div(class = "d-flex align-items-center justify-content-between mb-3",
          span(class = sprintf("step-icon bg-%s-subtle text-%s", color, color),
               bsicons::bs_icon(icon, size = "1.6rem")),
          span(class = sprintf("fs-1 fw-bold text-%s opacity-25", color), n)),
      h5(class = "fw-bold", title),
      p(class = "text-body-secondary", text)
    ),
    card_footer(
      actionButton(button_id, button_label, icon = icon("arrow-right"),
                   class = sprintf("btn-outline-%s w-100", color))
    )
  )
}

# Liste de valeurs affichées en puces (au-delà de max_n : « +N »)
pills <- function(x, cls = "bg-primary-subtle text-primary-emphasis",
                  max_n = 8, center = FALSE) {
  shown <- utils::head(x, max_n)
  div(class = paste("d-flex flex-wrap gap-1 mt-1",
                    if (center) "justify-content-center"),
      lapply(shown, function(v) {
        span(class = paste("badge rounded-pill fw-semibold", cls), v)
      }),
      if (length(x) > max_n) {
        span(class = "badge rounded-pill text-bg-secondary",
             paste0("+", length(x) - max_n))
      })
}

# Une ligne du récapitulatif : icône, intitulé, valeur
recap_row <- function(icon, label, value) {
  div(class = "d-flex gap-3 py-2 border-bottom",
      div(class = "text-primary flex-shrink-0 pt-1",
          bsicons::bs_icon(icon, size = "1.2rem")),
      div(class = "flex-grow-1",
          div(class = "small text-body-secondary", label),
          div(class = "fw-semibold", value)))
}

# Bandeau : les réglages ont changé depuis le dernier entraînement
stale_alert <- function(button_id) {
  div(class = "alert alert-warning d-flex flex-wrap align-items-center gap-3",
      bsicons::bs_icon("exclamation-triangle", size = "1.3rem"),
      div(class = "flex-grow-1",
          tags$strong("Les résultats correspondent au modèle entraîné précédemment."),
          " Les données ou les réglages ont changé depuis : réentraînez le modèle",
          " pour mettre les résultats à jour."),
      actionButton(button_id, "Aller à l'onglet Variables",
                   class = "btn-warning btn-sm"))
}

# Matrice de confusion affichée en carte de chaleur (tableau HTML)
heatmap_table <- function(cm) {
  cls <- colnames(cm)
  row_tot <- rowSums(cm)

  # En-tête : les classes prédites
  header <- tags$tr(
    tags$th(""),
    lapply(cls, function(k) tags$th(class = "text-center small text-body-secondary", k))
  )

  # Une ligne par classe observée, une case colorée par classe prédite
  body <- lapply(rownames(cm), function(r) {
    tags$tr(
      tags$th(class = "small text-body-secondary", r),
      lapply(cls, function(k) {
        v <- cm[r, k]
        share <- if (row_tot[r] > 0) v / row_tot[r] else 0
        # Couleur principale du thème (indigo #4f46e5), plus ou moins opaque
        bg <- if (v == 0) "var(--bs-tertiary-bg)"
              else sprintf("rgba(var(--bs-primary-rgb), %.2f)", 0.15 + 0.85 * share)
        fg <- if (share > 0.5) "#ffffff" else "var(--bs-body-color)"
        tags$td(v, class = "text-center fw-bold fs-5",
                style = sprintf("background:%s; color:%s; border-radius:.6rem; height:64px; vertical-align:middle;",
                                bg, fg))
      })
    )
  })

  tags$table(class = "w-100", style = "border-collapse: separate; border-spacing: 6px;",
             tags$thead(header), tags$tbody(body))
}

# ---------------------------------------------------------------------------
# Interface
# ---------------------------------------------------------------------------

ui <- page_navbar(
  id = "tabs",
  title = tags$span(class = "d-flex align-items-center gap-3",
                    span(class = "brand-logo",
                         bsicons::bs_icon("graph-up-arrow", size = "1.4rem")),
                    span(class = "fw-bold fs-3", style = "letter-spacing: -0.02em;",
                         "ldapred")),
  window_title = "ldapred - Analyse discriminante prédictive",
  theme = app_theme,
  fillable = FALSE,
  # Traduit le texte de fin de chargement de fileInput (« Upload complete »)
  header = tags$head(tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function () {
      new MutationObserver(function () {
        document.querySelectorAll('.shiny-file-input-progress .progress-bar')
          .forEach(function (bar) {
            if (bar.textContent === 'Upload complete') {
              bar.textContent = 'Chargement terminé';
            }
          });
      }).observe(document.body, { childList: true, subtree: true, characterData: true });
    });
  "))),

  # Espaceur à gauche : avec celui de droite, il place les onglets au milieu
  nav_spacer(),

  # ---- Onglet 1 : Données --------------------------------------------------
  nav_panel(
    title = "Données", value = "Données", icon = bsicons::bs_icon("database"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Charger les données", width = 340,
        fileInput("file", "Fichier (CSV, TXT ou XLSX)",
                  accept = c(".csv", ".txt", ".tsv", ".xlsx"),
                  buttonLabel = "Parcourir...",
                  placeholder = "Aucun fichier sélectionné"),
                tags$p(class = "fw-semibold mt-2 mb-2", "Ou un jeu d'exemple"),
        actionButton("example_iris", "Iris (quantitatif)",
                     icon = icon("seedling"),
                     class = "btn-outline-primary btn-sm w-100 mb-3"),
        radioButtons("sep", "Séparateur (fichiers texte uniquement)",
                     choices = c("Tabulation" = "\t", "Point-virgule" = ";",
                                 "Virgule" = ","),
                     selected = "\t"),
        radioButtons("dec", "Séparateur décimal",
                     choices = c("Point" = ".", "Virgule" = ","),
                     selected = ".", inline = TRUE)
      ),
      card(
        # Au moins la hauteur du panneau de gauche, et elle grandit avec le
        # tableau : pas de barre de défilement interne
        fill = FALSE, style = "min-height: 100%;",
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("table"), "Aperçu des données"),
        card_body(
          fillable = FALSE, class = "overflow-visible",
          # Avant chargement : message centré, à la place d'un tableau vide
          conditionalPanel(
            "!output.has_data",
            div(class = "d-flex flex-column justify-content-center align-items-center text-center text-body-secondary",
                style = "min-height: 340px;",   # moins haut qu'avant : pas de défilement
                div(class = "mb-3 text-primary", bsicons::bs_icon("cloud-upload", size = "5rem")),
                h3(class = "fw-bold text-body", "Aucune donnée chargée"),
                p(class = "fs-5 mx-auto mb-0", style = "max-width: 560px;",
                  "Choisissez un fichier dans le panneau de gauche, ou cliquez",
                  "sur « Iris (quantitatif) » pour essayer avec un jeu d'exemple."))
          ),
          # Après chargement : pastilles et aperçu du tableau
          conditionalPanel(
            "output.has_data",
            uiOutput("data_badges"),
            DT::DTOutput("data_preview")
          )
        ),
        # Bouton vers l'étape suivante, dans le pied de carte (sous le tableau)
        # Pied de carte affiché seulement avec des données (sinon hauteur vide)
        conditionalPanel(
          "output.has_data",
          card_footer(
            div(class = "d-flex flex-wrap justify-content-end align-items-center gap-3",
                span(class = "text-body-secondary",
                     "Données prêtes. Étape suivante :"),
                actionButton("next_vars", "Choisir les variables",
                             class = "btn-primary"))
          )
        )
      )
    )
  ),

  # ---- Onglet 2 : Variables ------------------------------------------------
  nav_panel(
    title = "Variables", value = "Variables", icon = bsicons::bs_icon("sliders"),

    # Sans données : uniquement le message, ni cadres ni récapitulatif
    conditionalPanel(
      "!output.has_data",
      empty_state("database", "Aucune donnée chargée",
        "Commencez par charger un fichier, ou un jeu d'exemple, dans l'onglet Données.",
        actionButton("go_data", "Aller à l'onglet Données",
                     icon = icon("arrow-left"), class = "btn-primary"))
    ),

    # Avec données : les deux cadres et le récapitulatif
    conditionalPanel(
      "output.has_data",
      layout_columns(
        col_widths = breakpoints(md = c(12, 12, 12), lg = c(4, 4, 4)),

        # ---- Cadre 1 : les variables ----------------------------------------
        card(
          fill = FALSE, class = "overflow-visible h-100",
          card_header(class = "d-flex align-items-center gap-2",
                      bsicons::bs_icon("list-check"), "Variables"),
          card_body(
            fillable = FALSE, class = "overflow-visible",
            style = "min-height: 520px;",

            tags$h6(class = "fw-bold d-flex align-items-center gap-2",
                    bsicons::bs_icon("bullseye", class = "text-primary"),
                    "1. Variable cible (catégorielle)"),
            uiOutput("target_ui"),
            hr(),

            tags$h6(class = "fw-bold d-flex align-items-center gap-2",
                    bsicons::bs_icon("check2-square", class = "text-success"),
                    "2. Variables explicatives"),
            uiOutput("explanatory_ui"),
            hr(),

            tags$h6(class = "fw-bold d-flex align-items-center gap-2",
                    bsicons::bs_icon("lightbulb", class = "text-warning"),
                    "3. Variables illustratives (facultatif)"),
            helpText("Elles ne servent pas au modèle : elles aident à interpréter les classes prédites."),
            uiOutput("illustrative_ui")
          )
        ),

        # ---- Cadre 2 : les paramètres du modèle ------------------------------
        card(
          fill = FALSE, class = "overflow-visible h-100",
          card_header(class = "d-flex align-items-center gap-2",
                      bsicons::bs_icon("gear"), "Paramètres du modèle"),
          card_body(
            fillable = FALSE, class = "overflow-visible",
            style = "min-height: 520px;",

            selectInput("method", label_info("Méthode de LDA",
                          "Classique : covariance intra-classes commune, pour le cas usuel (beaucoup plus d'individus que de variables). Grande dimension : covariance régularisée, pour de nombreuses variables ou des variables corrélées."),
                        choices = c("Classique" = "classic",
                                    "Grande dimension" = "highdim"),
                        width = "100%"),

            # Shrinkage : seulement pour la LDA grande dimension
            conditionalPanel(
              "input.method == 'highdim'",
              checkboxInput("shrink_auto",
                label_info("Estimer automatiquement le shrinkage",
                  "La matrice de covariance est rapprochée de sa diagonale pour la stabiliser. Par défaut, l'intensité λ est estimée à partir des données (Schäfer et Strimmer, 2005)."),
                value = TRUE),
              conditionalPanel(
                "!input.shrink_auto",
                sliderInput("shrinkage", "Intensité du shrinkage (λ)",
                            min =0.01, max = 1, value = 0.1, step = 0.01, width = "100%")
              )
            ),
            selectInput("selection", label_info("Sélection de variables",
                          "Ascendante : ajoute une à une les variables les plus significatives. Descendante : part de toutes les variables et retire les moins significatives. Pas à pas : ascendante, avec possibilité de retirer une variable devenue non significative."),
                        choices = c("Aucune" = "none",
                                    "Ascendante (forward)" = "forward",
                                    "Descendante (backward)" = "backward",
                                    "Pas à pas (stepwise)" = "stepwise"),
                        width = "100%"),

            # Seuil d'entrée : sélections forward et stepwise
            conditionalPanel(
              "input.selection == 'forward' || input.selection == 'stepwise'",
              sliderInput("alpha_in", label_info("Seuil d'entrée",
                            "Une variable entre dans le modèle si la p-value de son test F partiel est inférieure à ce seuil."),
                          min = 0.01, max = 1, value = 0.15, step = 0.01,
                          width = "100%")
            ),
            # Seuil de maintien : sélections backward et stepwise
            conditionalPanel(
              "input.selection == 'backward' || input.selection == 'stepwise'",
              sliderInput("alpha_out", label_info("Seuil de maintien",
                            "Une variable est retirée du modèle si la p-value de son test F partiel dépasse ce seuil."),
                          min = 0.01, max = 1, value = 0.15, step = 0.01,
                          width = "100%")
            ),
            # Nombre maximal d'itérations : toutes les méthodes de sélection
            conditionalPanel(
              "input.selection != 'none'",
              numericInput("max_iter", label_info("Nombre maximal d'itérations",
                             "Sécurité : la sélection s'arrête si elle atteint ce nombre d'étapes."),
                           value = 100, min = 1, step = 1, width = "100%")
            ),
            uiOutput("threshold_warning"),
            hr(),

            checkboxInput("use_test",
              label_info("Réserver un échantillon test",
                "Une partie des individus n'est pas utilisée pour l'apprentissage : elle sert à mesurer le taux de bonne classification sur des individus que le modèle n'a jamais vus. C'est une évaluation plus honnête que sur les données d'apprentissage."),
              value = FALSE),
            conditionalPanel(
              "input.use_test",
              sliderInput("test_prop", "Part de l'échantillon test",
                          min = 10, max = 50, value = 30, step = 5, post = " %",
                          width = "100%"),
              numericInput("seed",
                label_info("Graine aléatoire",
                  "Fixe le tirage de l'échantillon test : avec la même graine, on obtient le même découpage, donc les mêmes résultats."),
                value = 123, width = "100%")
            )
          )
        ),

        # ---- Cadre 3 : le récapitulatif --------------------------------------
        card(
          fill = FALSE, class = "h-100 border-primary",
          card_header(class = "d-flex align-items-center gap-2 text-primary",
                      bsicons::bs_icon("clipboard-check"), "Récapitulatif"),
          card_body(fillable = FALSE, style = "min-height: 520px;",
                    uiOutput("recap_ui")),
          card_footer(
            actionButton("run", "Entraîner le modèle", icon = icon("play"),
                         class = "btn-primary btn-lg w-100")
          )
        )
      )
    )
  ),

  # ---- Onglet 3 : Résultats ------------------------------------------------
  nav_panel(
    title = "Résultats", value = "Résultats", icon = bsicons::bs_icon("bar-chart"),

    uiOutput("stale_res"),
    conditionalPanel(
      "!output.has_analysis",
      empty_state("bar-chart", "Aucun modèle entraîné",
        "Choisissez la variable cible et les variables explicatives, puis cliquez sur « Entraîner le modèle » : les résultats s'afficheront ici.",
        actionButton("go_vars_res", "Aller à l'onglet Variables",
                     icon = icon("arrow-left"), class = "btn-primary"))
    ),
    conditionalPanel(
      "output.has_analysis",

    # Les 4 indicateurs clés
    layout_columns(
      fill = FALSE,
      col_widths = breakpoints(md = c(6, 6, 6, 6), lg = c(3, 3, 3, 3)),
      value_box(
        title = "Taux de bonne classification",
        value = textOutput("vb_accuracy"),
        showcase = bsicons::bs_icon("check-circle"),
        theme = "primary",
        p(textOutput("vb_accuracy_sub", inline = TRUE))
      ),
      value_box(
        title = "Individus utilisés",
        value = textOutput("vb_n"),
        showcase = bsicons::bs_icon("people", class = "text-primary"),
        p(textOutput("vb_n_sub", inline = TRUE))
      ),
      value_box(
        title = "Variables du modèle",
        value = textOutput("vb_vars"),
        showcase = bsicons::bs_icon("layers", class = "text-primary"),
        p(textOutput("vb_vars_sub", inline = TRUE))
      ),
      value_box(
        title = "Classes",
        value = textOutput("vb_classes"),
        showcase = bsicons::bs_icon("tags", class = "text-primary"),
        p(textOutput("vb_classes_sub", inline = TRUE))
      )
    ),

    # Matrice de confusion (carte de chaleur) et description du modèle
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("grid-3x3"), "Matrice de confusion"),
        card_body(
          helpText("Lignes : classes observées. Colonnes : classes prédites. ",
                   "Plus la case est foncée, plus elle contient d'individus."),
          uiOutput("confusion_ui"),
          textOutput("confusion_note")
        )
      ),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("info-circle"), "Description du modèle"),
        card_body(fillable = FALSE, uiOutput("model_desc_ui"))
      )
    ),

    # Évaluation sur l'échantillon test
    card(
      card_header(class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("clipboard-check"),
                  "Évaluation sur l'échantillon test"),
      card_body(uiOutput("test_ui"))
    ),

    # Évaluation globale et pertinence des variables
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("clipboard-data"), "Évaluation globale du modèle"),
        card_body(uiOutput("wilks_ui"))
      ),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("bar-chart-steps"), "Pertinence des variables"),
        card_body(uiOutput("var_tests_ui"))
      )
    ),

    # Étapes de la sélection de variables
    card(
      card_header(class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("list-ol"), "Étapes de la sélection"),
      card_body(uiOutput("selection_steps"))
    ),

    # Coefficients et variables illustratives
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("calculator"),
                    "Coefficients des fonctions de classement"),
        card_body(tableOutput("coefficients"))
      ),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("lightbulb"), "Variables illustratives"),
        card_body(uiOutput("illus_ui"), uiOutput("illus_plot"))
      )
    )
    )
  ),

  # ---- Onglet 4 : Prédiction -----------------------------------------------
  nav_panel(
    title = "Prédiction", value = "Prédiction", icon = bsicons::bs_icon("crosshair"),

    uiOutput("stale_pred"),
    conditionalPanel(
      "!output.has_analysis",
      empty_state("crosshair", "Aucun modèle à utiliser",
        "Pour prédire de nouveaux individus, entraînez d'abord un modèle dans l'onglet Variables.",
        actionButton("go_vars_pred", "Aller à l'onglet Variables",
                     icon = icon("arrow-left"), class = "btn-primary"))
    ),
    conditionalPanel(
      "output.has_analysis",
    # Deux cadres côte à côte : le formulaire et la prédiction de l'individu
    layout_columns(
      col_widths = breakpoints(md = c(12, 12), lg = c(7, 5)),
      card(
        fill = FALSE, class = "h-100",
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("person"), "Un individu"),
        card_body(fillable = FALSE, uiOutput("new_form")),
        card_footer(
          actionButton("predict_one", "Prédire cet individu",
                       icon = icon("bullseye"), class = "btn-primary btn-lg w-100"))
      ),
      card(
        fill = FALSE, class = "h-100",
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("stars"), "Prédiction de l'individu"),
        card_body(fillable = FALSE, uiOutput("pred_one_ui"))
      )
    ),
    card(
      card_header(class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("file-earmark-spreadsheet"),
                  "Un fichier d'individus"),
      card_body(
        fillable = FALSE,
        fileInput("new_file",
                  "Fichier de nouveaux individus (mêmes options de lecture que l'onglet Données)",
                  accept = c(".csv", ".txt", ".tsv", ".xlsx"),
                  buttonLabel = "Parcourir...",
                  placeholder = "Aucun fichier sélectionné",
                  width = "100%"),
        # Avant le chargement : explication et colonnes attendues
        conditionalPanel("!output.has_pred_file", uiOutput("pred_file_hint")),
        # Après : bouton de téléchargement et tableau des prédictions
        conditionalPanel(
          "output.has_pred_file",
          div(class = "d-flex justify-content-end mb-3",
              downloadButton("download_pred", "Télécharger les prédictions",
                             class = "btn-primary")),
          DT::DTOutput("pred_file")
        )
      )
    )
    )
  ),

  # ---- Onglet 5 : Aide -----------------------------------------------------
  nav_panel(
    title = "Aide", value = "Aide", icon = bsicons::bs_icon("question-circle"),

    div(class = "mb-2",
        h3(class = "fw-bold mb-1", "Comment utiliser l'application"),
        p(class = "text-body-secondary",
          "Quatre étapes, de vos données à la prédiction de nouveaux individus.")),

    layout_columns(
      col_widths = breakpoints(md = c(6, 6, 6, 6), lg = c(3, 3, 3, 3)),
      step_card(1, "database", "primary", "Charger les données",
                "Un fichier texte (tabulation par défaut), CSV ou Excel, ou le jeu d'exemple iris.",
                "help_go_data", "Aller aux données"),
      step_card(2, "sliders", "success", "Choisir les variables",
                "La variable cible, les variables explicatives et, si besoin, des variables illustratives.",
                "help_go_vars", "Choisir les variables"),
      step_card(3, "bar-chart", "warning", "Entraîner le modèle",
                "Puis lire les résultats : taux de bonne classification, matrice de confusion, échantillon test.",
                "help_go_res", "Voir les résultats"),
      step_card(4, "crosshair", "info", "Prédire",
                "Un nouvel individu au formulaire, ou tout un fichier d'individus à télécharger.",
                "help_go_pred", "Prédire")
    ),

    card(
      card_header(class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("chat-left-text"), "Questions fréquentes"),
      accordion(
        accordion_panel(
          "Qu'est-ce que l'analyse discriminante linéaire prédictive ?",
          "Une méthode d'apprentissage supervisé : à partir d'individus dont on connaît la classe, elle construit une fonction de classement par classe, puis affecte chaque nouvel individu à la classe la plus probable."
        ),
        accordion_panel(
          "À quoi servent les variables illustratives ?",
          "Elles ne participent pas au calcul du modèle. Elles aident à interpréter les résultats, en montrant comment elles se répartissent selon les classes prédites."
        ),
        accordion_panel(
          "Comment les variables qualitatives sont-elles traitées ?",
          "Chaque variable qualitative est codée en indicatrices 0/1, une par modalité sauf la première, qui sert de référence. Le même codage est réappliqué aux nouveaux individus."
        ),
        accordion_panel(
          "Que deviennent les valeurs manquantes ?",
          "Les lignes incomplètes sont retirées pour l'apprentissage, avec un avertissement. Lors de la prédiction, elles reçoivent une prédiction vide, et le fichier garde le même nombre de lignes."
        ),
        accordion_panel(
          "Pourquoi le taux de bonne classification est-il optimiste ?",
          "Il est calculé sur les données qui ont servi à construire le modèle. Pour une évaluation honnête, il faut un échantillon test que le modèle n'a pas vu."
        )
      )
    )
  ),

  # ---- À droite de la barre : palette de couleurs, mode clair / sombre ------
  nav_spacer(),
  do.call(nav_menu, c(
    list(title = "Thème", icon = bsicons::bs_icon("palette"), align = "right"),
    # Une entrée par palette : pastille de couleur + nom
    lapply(seq_along(palettes), function(i) {
      nav_item(actionLink(
        paste0("palette_", i),
        tagList(span(class = "palette-dot",
                     style = sprintf("background: %s;", palettes[[i]])),
                names(palettes)[i]),
        class = "dropdown-item"))
    })
  )),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

# ---------------------------------------------------------------------------
# Serveur
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Source des données : "none" au départ, puis "file" ou "iris"
  source_type <- reactiveVal("none")
  observeEvent(input$file, source_type("file"))
  observeEvent(input$example_iris, source_type("iris"))

  # Données d'apprentissage : selon la dernière source choisie
  data <- reactive({
    switch(source_type(),
      file = {
        req(input$file)
        df <- read_data(input$file$datapath, input$file$name, input$sep, input$dec)
        req(df)
        df
      },
      iris = datasets::iris,
      req(FALSE)   # "none" : rien n'est encore chargé
    )
  })
  # Pastilles : lignes, colonnes, numériques, catégorielles
  output$data_badges <- renderUI({
    df <- data()
    n_num <- sum(vapply(df, is.numeric, logical(1)))
    n_cat <- ncol(df) - n_num
    badge <- function(text, cls) {
      span(class = paste("badge rounded-pill fs-6 fw-semibold px-3 py-2", cls), text)
    }

    src <- if (source_type() == "iris") "Exemple : iris" else input$file$name
    div(class = "d-flex flex-wrap gap-2 mb-3",
      badge(src, "text-bg-dark"),
      badge(paste(nrow(df), "lignes"), "bg-body-tertiary text-body border"),
      badge(paste(ncol(df), "colonnes"), "bg-body-tertiary text-body border"),
      badge(paste(n_num, "numérique(s)"), "text-bg-primary"),
      badge(paste(n_cat, "catégorielle(s)"), "text-bg-info")
    )
    
  })

  # Aperçu interactif du tableau
  output$data_preview <- DT::renderDT({
    DT::datatable(data(), options = list(pageLength = 10, scrollX = TRUE,
                                         language = dt_french))
  })

  # Menu de la cible : uniquement les colonnes catégorielles (facteurs)
  output$target_ui <- renderUI({
    df <- data()
    cat_cols <- names(df)[vapply(df, is.factor, logical(1))]
    selectInput("target", NULL, choices = cat_cols, width = "100%")
  })

  # Menu des explicatives : toutes les colonnes sauf la cible
  output$explanatory_ui <- renderUI({
    req(input$target)
    others <- setdiff(names(data()), input$target)
    selectInput("explanatory", NULL, width = "100%",
                choices = others, selected = others, multiple = TRUE)
  })

  # Menu des illustratives : toutes les colonnes sauf la cible (facultatif)
  output$illustrative_ui <- renderUI({
    req(input$target)
    others <- setdiff(names(data()), input$target)
    selectInput("illustrative", NULL, width = "100%",
                choices = others, multiple = TRUE)
  })

  # Récapitulatif des choix, avec contrôle de cohérence
  output$recap_ui <- renderUI({
    req(input$target)
    expl <- input$explanatory
    illus <- input$illustrative

    # Problèmes qui empêchent l'entraînement
    problems <- c(
      if (length(expl) == 0) "Choisissez au moins une variable explicative.",
      if (length(intersect(expl, illus)) > 0)
        "Une variable ne peut pas être à la fois explicative et illustrative.",
      if (!thresholds_ok())
        "Le seuil d'entrée doit être inférieur ou égal au seuil de maintien."
    )

    thresholds <- switch(input$selection,
      forward  = paste0("Seuil d'entrée : ", input$alpha_in),
      backward = paste0("Seuil de maintien : ", input$alpha_out),
      stepwise = paste0("Entrée : ", input$alpha_in, " · maintien : ", input$alpha_out),
      NULL)

    tagList(
      recap_row("bullseye", "Variable cible",
        tagList(input$target,
                span(class = "badge rounded-pill text-bg-primary ms-2",
                     paste(nlevels(data()[[input$target]]), "classes")))),
      recap_row("check2-square",
        sprintf("Variables explicatives (%d)", length(expl)),
        if (length(expl) > 0) pills(expl)
        else span(class = "text-danger", "aucune")),
      recap_row("lightbulb",
        sprintf("Variables illustratives (%d)", length(illus)),
        if (length(illus) > 0) pills(illus, "bg-warning-subtle text-warning-emphasis")
        else span(class = "fw-normal text-body-secondary", "aucune")),
      recap_row("cpu", "Méthode", method_labels[[input$method]]),
      recap_row("funnel", "Sélection de variables",
        tagList(selection_labels[[input$selection]],
                if (!is.null(thresholds))
                  div(class = "small fw-normal text-body-secondary", thresholds))),
      recap_row("clipboard-data", "Échantillon test",
        if (isTRUE(input$use_test))
          sprintf("%s %% · graine %s", input$test_prop, input$seed) else "non"),

      # État final : prêt, ou liste des problèmes
      if (length(problems) > 0) {
        div(class = "alert alert-danger small mt-3 mb-0",
            lapply(problems, function(p) div(bsicons::bs_icon("x-circle"), " ", p)))
      } else {
        div(class = "alert alert-success small mt-3 mb-0",
            bsicons::bs_icon("check-circle"),
            " Tout est prêt : vous pouvez entraîner le modèle.")
      }
    )
  })

  # Entraînement au clic ; renvoie le modèle et tout ce qu'il faut pour les
  # onglets Résultats et Prédiction (figé au moment du clic)
  analysis <- eventReactive(input$run, {
    req(input$target, input$explanatory)
    if (length(intersect(input$explanatory, input$illustrative)) > 0) {
      showNotification("Une variable ne peut pas être à la fois explicative et illustrative.",
                       type = "error")
      req(FALSE)
    }

    if (!thresholds_ok()) {
      showNotification("Le seuil d'entrée doit être inférieur ou égal au seuil de maintien.",
                       type = "error")
      req(FALSE)
    }

    df <- data()
    X <- df[input$explanatory]
    y <- df[[input$target]]

    # Échantillon test stratifié, tiré par le package (train_test_split)
    test_idx <- integer(0)
    if (isTRUE(input$use_test)) {
      sp <- safe_run(train_test_split(X, y, test_size = input$test_prop / 100,
                                      seed = input$seed),
                     "Erreur lors du découpage apprentissage / test :")
      req(sp)
      test_idx <- sp$test_index
    }
    train_rows <- setdiff(seq_len(nrow(X)), test_idx)
    X_train <- X[train_rows, , drop = FALSE]
    y_train <- y[train_rows]

    m <- LDAClassifier$new(method = input$method, selection = input$selection,
                           slentry = input$alpha_in, slstay = input$alpha_out,
                           max_iter = input$max_iter,
                           shrinkage = if (isTRUE(input$shrink_auto)) NULL else input$shrinkage)
    res <- withProgress(message = "Entraînement du modèle en cours...", value = 0.5, {
      safe_run(m$fit(X_train, y_train), "Erreur lors de l'entraînement du modèle :")
    })
    req(res)

    # Évaluation sur l'échantillon test, si demandé (calculée par le package)
    test_eval <- NULL
    if (length(test_idx) > 0) {
      test_eval <- safe_run(m$evaluate(X[test_idx, , drop = FALSE], y[test_idx]),
                            "Erreur lors de l'évaluation sur l'échantillon test :")
    }

    # Prédictions sur les données d'apprentissage (pour les illustratives ;
    # les avertissements sur les NA ont déjà été affichés pendant le fit)
    pred_train <- suppressWarnings(m$predict(X_train))

    showNotification("Modèle entraîné.", type = "message")
    list(model = m, train = X_train, vars = input$explanatory, y = y_train,
         pred_train = pred_train,
         illus = df[train_rows, input$illustrative, drop = FALSE],
         test = test_eval, settings = current_settings())
  })

  # Après un entraînement réussi, on bascule sur l'onglet Résultats
  has_run <- reactiveVal(FALSE)
  observeEvent(analysis(), {
    has_run(TRUE)
    nav_select("tabs", selected = "Résultats", session = session)
  })

  # ---- États de l'appli, utilisés par les conditionalPanel de l'interface --
  # Des données sont-elles chargées ?
  output$has_data <- reactive({
    isTRUE(tryCatch(nrow(data()) > 0, error = function(e) FALSE))
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # Un entraînement a-t-il déjà réussi ?
  output$has_analysis <- reactive(has_run())
  outputOptions(output, "has_analysis", suspendWhenHidden = FALSE)

  # Boutons qui mènent vers l'onglet utile
  observeEvent(input$go_data, nav_select("tabs", "Données", session = session))
  observeEvent(input$next_vars, nav_select("tabs", "Variables", session = session))

  # Boutons des cartes d'étapes de l'onglet Aide
  observeEvent(input$help_go_data, nav_select("tabs", "Données", session = session))
  observeEvent(input$help_go_vars, nav_select("tabs", "Variables", session = session))
  observeEvent(input$help_go_res, nav_select("tabs", "Résultats", session = session))
  observeEvent(input$help_go_pred, nav_select("tabs", "Prédiction", session = session))

  # Menu « Thème » : changement de la couleur principale de toute l'appli
  lapply(seq_along(palettes), function(i) {
    observeEvent(input[[paste0("palette_", i)]], {
      session$setCurrentTheme(bs_theme_update(app_theme, primary = palettes[[i]]))
    })
  })
  observeEvent(input$go_vars_res, nav_select("tabs", "Variables", session = session))
  observeEvent(input$go_vars_pred, nav_select("tabs", "Variables", session = session))
  observeEvent(input$go_vars_stale_res, nav_select("tabs", "Variables", session = session))
  observeEvent(input$go_vars_stale_pred, nav_select("tabs", "Variables", session = session))

  # ---- Résultats périmés -----------------------------------------------------
  # Réglages actuels (données + variables + paramètres)
  current_settings <- reactive({
    list(
      source = source_type(),
      file = if (source_type() == "file") list(input$file$datapath, input$sep, input$dec),
      target = input$target, explanatory = input$explanatory,
      illustrative = input$illustrative, method = input$method,
      selection = input$selection, alpha_in = input$alpha_in,
      alpha_out = input$alpha_out, max_iter = input$max_iter,
      shrink_auto = input$shrink_auto, shrinkage = input$shrinkage,
      use_test = input$use_test,
      test_prop = input$test_prop, seed = input$seed
    )
  })

  # Les réglages ont-ils changé depuis le dernier entraînement réussi ?
  stale <- reactive({
    if (!has_run()) return(FALSE)
    a <- tryCatch(analysis(), error = function(e) NULL)
    !is.null(a) && !identical(current_settings(), a$settings)
  })

  output$stale_res <- renderUI(if (stale()) stale_alert("go_vars_stale_res"))
  output$stale_pred <- renderUI(if (stale()) stale_alert("go_vars_stale_pred"))

  # Description du modèle, mise en forme (toutes les informations viennent
  # du modèle entraîné et des réglages mémorisés au moment de l'entraînement)
  output$model_desc_ui <- renderUI({
    a <- analysis()
    m <- a$model
    st <- a$settings
    counts <- rowSums(m$confusion)          # effectifs d'apprentissage par classe
    cols <- class_colors(m$classes)
    n_used <- sum(counts)
    n_removed <- nrow(a$train) - n_used

    thresholds <- switch(st$selection,
      forward  = paste0("Seuil d'entrée : ", st$alpha_in),
      backward = paste0("Seuil de maintien : ", st$alpha_out),
      stepwise = paste0("Entrée : ", st$alpha_in, " · maintien : ", st$alpha_out),
      NULL)

    tagList(
      recap_row("check-circle", "Statut",
        span(class = "badge rounded-pill text-bg-success", "Modèle entraîné")),
      recap_row("people", "Individus utilisés pour l'apprentissage",
        tagList(n_used,
                if (n_removed > 0)
                  div(class = "small fw-normal text-body-secondary",
                      paste(n_removed, "ligne(s) retirée(s) (valeurs manquantes)")))),
      recap_row("tags", sprintf("Classes (%d)", length(m$classes)),
        div(class = "d-flex flex-wrap gap-1 mt-1",
            lapply(m$classes, function(k) {
              span(class = "badge rounded-pill fw-semibold",
                   style = sprintf("background: %s; color: #ffffff;", cols[[k]]),
                   sprintf("%s (%d)", k, counts[[k]]))
            }))),
      recap_row("layers",
        sprintf("Variables : %d d'origine, %d colonne(s) après codage",
                length(a$vars), length(m$selected_vars)),
        pills(m$selected_vars, max_n = 12)),
      recap_row("cpu", "Méthode",
        tagList(method_labels[[st$method]],
                if (!is.null(m$shrinkage))
                  div(class = "small fw-normal text-body-secondary",
                      sprintf("Intensité du shrinkage λ = %.3f%s", m$shrinkage,
                              if (isTRUE(st$shrink_auto)) " (estimée)" else " (choisie)")))),
      recap_row("funnel", "Sélection de variables",
        tagList(selection_labels[[st$selection]],
                if (!is.null(thresholds))
                  div(class = "small fw-normal text-body-secondary", thresholds))),
      recap_row("clipboard-data", "Échantillon test",
        if (isTRUE(st$use_test))
          sprintf("%s %% · graine %s", st$test_prop, st$seed) else "non"),

      # Sortie brute de print(), repliable
      tags$details(class = "mt-3",
        tags$summary(class = "small text-body-secondary",
                     "Afficher la sortie R de print(model)"),
        verbatimTextOutput("model_print"))
    )
  })

  # Sortie brute du modèle (dans la section repliable)
  output$model_print <- renderPrint({
    print(analysis()$model)
  })
  # Calculée même quand la section repliable est fermée
  outputOptions(output, "model_print", suspendWhenHidden = FALSE)

  # ---- Indicateurs clés (tous lus dans le package) --------------------------
  output$vb_accuracy <- renderText({
    sprintf("%.1f %%", 100 * analysis()$model$accuracy)
  })
  output$vb_accuracy_sub <- renderText({
    sprintf("Taux d'erreur %.1f %% · données d'apprentissage",
            100 * (1 - analysis()$model$accuracy))
  })

  output$vb_n <- renderText({
    sum(analysis()$model$confusion)
  })
  output$vb_n_sub <- renderText({
    a <- analysis()
    removed <- nrow(a$train) - sum(a$model$confusion)
    if (removed == 0) "Aucune ligne retirée (NA)"
    else paste(removed, "ligne(s) retirée(s) (NA)")
  })

  output$vb_vars <- renderText({
    length(analysis()$vars)
  })
  output$vb_vars_sub <- renderText({
    paste(length(analysis()$model$selected_vars), "colonne(s) après codage")
  })

  output$vb_classes <- renderText({
    length(analysis()$model$classes)
  })
  output$vb_classes_sub <- renderText({
    paste(analysis()$model$classes, collapse = " · ")
  })

  # ---- Matrice de confusion en carte de chaleur -----------------------------
  output$confusion_ui <- renderUI({
    heatmap_table(analysis()$model$confusion)
  })

  # Évaluation sur l'échantillon test : comparaison avec l'apprentissage
  output$test_ui <- renderUI({
    a <- analysis()
    if (is.null(a$test)) {
      return(div(class = "text-body-secondary",
        bsicons::bs_icon("info-circle"),
        " Aucun échantillon test. Cochez « Réserver un échantillon test » dans",
        " l'onglet Variables pour évaluer le modèle sur des individus qu'il n'a",
        " jamais vus."))
    }
    tagList(
      layout_columns(
        col_widths = c(6, 6),
        value_box(
          title = "Taux sur l'échantillon test",
          value = sprintf("%.1f %%", 100 * a$test$accuracy),
          showcase = bsicons::bs_icon("clipboard-check"),
          theme = "primary",   # suit la palette choisie
          p(sprintf("%d individus que le modèle n'a pas vus", sum(a$test$confusion)))
        ),
        value_box(
          title = "Taux sur l'apprentissage",
          value = sprintf("%.1f %%", 100 * a$model$accuracy),
          showcase = bsicons::bs_icon("mortarboard", class = "text-primary"),
          p("Taux de resubstitution (optimiste)")
        )
      ),
      helpText("Matrice de confusion sur l'échantillon test."),
      heatmap_table(a$test$confusion)
    )
  })

  output$confusion_note <- renderText({
    cm <- analysis()$model$confusion
    sprintf("%d individus sur %d bien classés.", sum(diag(cm)), sum(cm))
  })

  # p-values lisibles : "< 0,0001" pour les très petites, sinon 4 décimales
  fmt_p <- function(p) ifelse(p < 1e-4, "< 0,0001", formatC(p, format = "f", digits = 4))

  # Évaluation globale : lambda de Wilks + F de Rao
  output$wilks_ui <- renderUI({
    w <- analysis()$model$wilks
    req(w)
    separe <- w$p_value < 0.05
    lignes <- list(
      c("Lambda de Wilks", formatC(w$lambda, format = "f", digits = 4)),
      c("F de Rao", sprintf("%.2f (ddl = %g ; %g)", w$F, w$df1, w$df2)),
      c("p-value", fmt_p(w$p_value))
    )
    tagList(
      tags$table(class = "table table-sm mb-3",
        tags$tbody(lapply(lignes, function(l)
          tags$tr(tags$th(class = "fw-semibold", l[1]),
                  tags$td(class = "text-end", l[2]))))),
      div(class = paste("alert mb-0", if (separe) "alert-success" else "alert-warning"),
          if (separe) {
            "Les classes sont significativement séparées : les centres des classes sont différents (p < 0,05). Plus le lambda est proche de 0, meilleure est la séparation."
          } else {
            "Les classes ne sont pas significativement séparées (p ≥ 0,05) : le modèle discrimine mal."
          })
    )
  })

  # Pertinence de chaque variable, en plus de toutes les autres
  output$var_tests_ui <- renderUI({
    vt <- analysis()$model$variable_tests
    req(vt)
    vt <- vt[order(vt$F, decreasing = TRUE), ]   # de la plus utile à la moins utile
    tab <- data.frame(
      "Variable"       = vt$variable,
      "Lambda partiel" = formatC(vt$lambda_partial, format = "f", digits = 4),
      "R² partiel"     = formatC(vt$R2_partial, format = "f", digits = 3),
      "F"              = formatC(vt$F, format = "f", digits = 2),
      "p-value"        = fmt_p(vt$p_value),
      check.names = FALSE
    )
    tagList(
      renderTable(tab),
      helpText("Apport de chaque variable en plus de toutes les autres, de la plus utile à la moins utile. Une p-value élevée (> 0,05) signale une variable redondante.")
    )
  })

  # Historique de la sélection : une ligne par étape
  output$selection_steps <- renderUI({
    a <- analysis()
    # Cas 1 : aucune sélection demandée
    if (a$model$selection == "none") {
      return(p(class = "text-body-secondary mb-0", "Aucune sélection de variables demandée : toutes les variables explicatives sont utilisées."))
    }
    # Cas 2 : sélection faite, mais aucune étape (rien retiré en backward)
    h <- a$model$selection_history
    if (is.null(h) || nrow(h) == 0) {
      return(p(class = "text-body-secondary mb-0",
      sprintf("La sélection n'a retiré aucune variable : toutes restent significatives au seuil de maintien (%s).", a$settings$alpha_out)))
    }

    tab <- data.frame(
      "Étape"           = as.integer(h$step),
      "Action"          = ifelse(h$action == "add", "Entrée", "Sortie"),
      "Variable"        = h$variable,
      "Lambda de Wilks" = formatC(h$lambda, format = "f", digits = 4),
      "R² partiel"      = formatC(h$R2_partial, format = "f", digits = 3),
      "F"               = formatC(h$F, format = "f", digits = 2),
      "p-value"         = fmt_p(h$p_value),
      check.names = FALSE
    )
    tagList(
      renderTable(tab),
      helpText("Lambda de Wilks du modèle après chaque étape : plus il est proche de 0, mieux les classes sont séparées.")
    )
  })

  # Coefficients : s'affichent automatiquement dès que la LDA les fournit
  output$coefficients <- renderTable({
    coef <- analysis()$model$coefficients
    validate(need(!is.null(coef),
      "Les coefficients seront affichés dès que les classes LDA les fourniront."))
    coef
  }, rownames = TRUE, digits = 4)

  # Choix de la variable illustrative à étudier
  output$illus_ui <- renderUI({
    a <- analysis()
    validate(need(ncol(a$illus) > 0,
      "Aucune variable illustrative choisie dans l'onglet Variables."))
    selectInput("illus_var", "Variable illustrative", choices = names(a$illus))
  })

  # Variables illustratives : barres selon le type de la variable
  output$illus_plot <- renderUI({
    a <- analysis()
    req(input$illus_var, input$illus_var %in% names(a$illus))
    col <- a$illus[[input$illus_var]]
    classes <- a$model$classes
    cols <- class_colors(classes)

    if (is.numeric(col)) {
      # Numérique : moyenne par classe prédite, barre proportionnelle à la plus grande
      means <- tapply(col, a$pred_train, mean, na.rm = TRUE)
      mx <- max(abs(means), na.rm = TRUE)
      tagList(
        helpText("Moyenne de la variable dans chaque classe prédite."),
        lapply(classes, function(k) {
          bar_row(k, abs(means[[k]]) / mx, cols[[k]], sprintf("%.2f", means[[k]]))
        })
      )
    } else {
      # Catégorielle : répartition des modalités dans chaque classe prédite
      tab <- table(a$pred_train, col)
      props <- prop.table(tab, margin = 1)
      lev_cols <- level_colors(colnames(tab))

      rows <- lapply(classes, function(k) {
        segments <- lapply(colnames(tab), function(m) {
          p <- props[k, m]
          if (is.na(p) || p == 0) return(NULL)
          div(class = "d-flex align-items-center justify-content-center text-white small fw-semibold",
              style = sprintf("width: %.1f%%; background: %s;", 100 * p, lev_cols[[m]]),
              if (p >= 0.12) sprintf("%.0f %%", 100 * p))
        })
        div(class = "d-flex align-items-center gap-3 mb-2",
          span(class = "fw-semibold", style = "width: 110px;", k),
          div(class = "flex-grow-1 d-flex rounded-3",
              style = "height: 28px; overflow: hidden; background: var(--bs-tertiary-bg);",
              segments)
        )
      })

      legend <- div(class = "d-flex flex-wrap gap-3 mt-3 small",
        lapply(colnames(tab), function(m) {
          span(class = "d-flex align-items-center gap-2",
               span(style = sprintf("width: 12px; height: 12px; border-radius: 3px; background: %s;",
                                    lev_cols[[m]])),
               m)
        })
      )

      tagList(helpText("Répartition des modalités dans chaque classe prédite."),
              rows, legend)
    }
  })

  # Formulaire construit à partir des variables explicatives
  output$new_form <- renderUI({
    a <- analysis()
    fields <- lapply(seq_along(a$vars), function(i) {
      v <- a$vars[i]
      col <- a$train[[v]]
      id <- paste0("new_", i)
      if (is.numeric(col)) {
        numericInput(id, v, value = round(stats::median(col, na.rm = TRUE), 2),
                     width = "100%")
      } else {
        selectInput(id, v, choices = levels(as.factor(col)), width = "100%")
      }
    })

    tagList(
      helpText("Les valeurs sont pré-remplies avec la médiane (variables numériques)",
               "ou la première modalité (variables qualitatives)."),
      # Avec beaucoup de variables, l'import d'un fichier est plus pratique
      if (length(fields) > 12) {
        div(class = "alert alert-info small",
            bsicons::bs_icon("info-circle"),
            sprintf(" Le modèle utilise %d variables. Pour prédire plusieurs individus,", length(fields)),
            " ou si la saisie est trop longue, utilisez plutôt l'import d'un fichier ci-dessous.")
      },
      # Grille responsive : autant de colonnes que la largeur le permet (220 px min.)
      do.call(layout_column_wrap,
              c(list(width = "220px", fill = FALSE, heights_equal = "row"), fields))
    )
  })


  # Prédiction de l'individu saisi, au clic sur le bouton
  pred_one <- eventReactive(input$predict_one, {
    a <- analysis()

    # Construction d'un data.frame d'une ligne avec les valeurs saisies
    values <- lapply(seq_along(a$vars), function(i) {
      col <- a$train[[a$vars[i]]]
      val <- input[[paste0("new_", i)]]
      if (is.numeric(col)) as.numeric(val)
      else factor(val, levels = levels(as.factor(col)))
    })
    new <- as.data.frame(stats::setNames(values, a$vars), check.names = FALSE)

    res <- safe_run({
      pred <- a$model$predict(new)
      P <- a$model$predict_proba(new)
      data.frame("Classe prédite" = as.character(pred), round(P, 3),
                 check.names = FALSE)
    }, "Erreur lors de la prédiction :")
    req(res)
    res
  })

  # Prédiction de l'individu : carte de la classe prédite + barres de probabilités
  output$pred_one_ui <- renderUI({
    # Avant le premier clic : invitation à prédire (pas de cadre vide)
    res <- tryCatch(pred_one(), error = function(e) NULL)
    if (is.null(res)) {
      return(div(class = "d-flex flex-column align-items-center justify-content-center text-center text-body-secondary h-100 py-4",
                 div(class = "mb-3 text-primary",
                     bsicons::bs_icon("stars", size = "3.5rem")),
                 h5(class = "fw-bold text-body", "Pas encore de prédiction"),
                 p(class = "mb-0",
                   "Réglez les caractéristiques de l'individu, puis cliquez sur",
                   "« Prédire cet individu ».")))
    }
    classes <- analysis()$model$classes
    cols <- class_colors(classes)
    cl <- res[["Classe prédite"]][1]

    # Une valeur manquante dans le formulaire : pas de prédiction possible
    if (is.na(cl)) {
      return(div(class = "alert alert-warning mb-0",
                 bsicons::bs_icon("exclamation-triangle"),
                 " Une valeur est manquante : impossible de prédire cet individu."))
    }

    probs <- unlist(res[1, classes])
    # La classe prédite en haut, les probabilités d'appartenance en dessous
    tagList(
      div(class = "rounded-4 p-4 mb-4 text-white",
          style = sprintf("background: %s;", cols[[cl]]),
          div(class = "small fw-semibold", "Classe prédite"),
          div(class = "display-6 fw-bold", cl),
          div(class = "small", sprintf("Probabilité : %.1f %%", 100 * probs[[cl]]))),
      h6(class = "fw-bold mb-3", "Probabilités d'appartenance"),
      lapply(classes, function(k) {
        bar_row(k, probs[[k]], cols[[k]], sprintf("%.1f %%", 100 * probs[[k]]))
      })
    )
  })

  # Fichier de nouveaux individus
  new_data <- reactive({
    req(input$new_file)
    df <- read_data(input$new_file$datapath, input$new_file$name,
                    input$sep, input$dec)
    req(df)
    df
  })

  # Prédictions du fichier (données + classe prédite + probabilités)
  pred_file <- reactive({
    a <- analysis()
    nd <- new_data()
    res <- safe_run({
      pred <- a$model$predict(nd)
      # suppressWarnings : les avertissements ont déjà été affichés par predict()
      P <- suppressWarnings(a$model$predict_proba(nd))
      colnames(P) <- paste0("P(", colnames(P), ")")
      data.frame(nd, "Classe prédite" = pred, round(P, 3), check.names = FALSE)
    }, "Erreur lors de la prédiction :")
    req(res)
    res
  })

  # Un fichier de prédiction a-t-il été traité avec succès ?
  output$has_pred_file <- reactive({
    isTRUE(tryCatch(nrow(pred_file()) > 0, error = function(e) FALSE))
  })
  outputOptions(output, "has_pred_file", suspendWhenHidden = FALSE)

  # Avant le chargement : explication et colonnes attendues
  output$pred_file_hint <- renderUI({
    a <- analysis()
    div(class = "d-flex flex-column align-items-center text-center text-body-secondary py-4",
        div(class = "mb-3 text-primary",
            bsicons::bs_icon("file-earmark-arrow-up", size = "4rem")),
        h5(class = "fw-bold text-body", "Prédire tout un fichier d'individus"),
        p(class = "mx-auto", style = "max-width: 620px;",
          "Choisissez ci-dessus un fichier (texte, CSV ou Excel) contenant les",
          "nouveaux individus. Il doit contenir au moins les colonnes suivantes ;",
          "les colonnes en plus sont conservées dans le résultat."),
        pills(a$vars, max_n = 40, center = TRUE))
  })

  output$pred_file <- DT::renderDT({
    classes <- analysis()$model$classes
    cols <- class_colors(classes)
    DT::datatable(pred_file(), options = list(pageLength = 10, scrollX = TRUE,
                                              language = dt_french)) |>
      DT::formatStyle("Classe prédite",
                      backgroundColor = DT::styleEqual(classes, unname(cols)),
                      color = "white", fontWeight = "bold")
  })

  # Téléchargement des prédictions (CSV compatible Excel français)
  output$download_pred <- downloadHandler(
    filename = function() "predictions.csv",
    content = function(file) {
      utils::write.csv2(pred_file(), file, row.names = FALSE)
    }
  )

    # Alerte stepwise : le seuil d'entrée doit être <= au seuil de maintien
  thresholds_ok <- reactive({
    input$selection != "stepwise" || input$alpha_in <= input$alpha_out
  })

  output$threshold_warning <- renderUI({
    if (!thresholds_ok()) {
      div(class = "alert alert-warning small mb-0",
          bsicons::bs_icon("exclamation-triangle"),
          " Le seuil d'entrée doit être inférieur ou égal au seuil de maintien,",
          " sinon une variable peut entrer puis sortir en boucle.")
    }
  })
}

shinyApp(ui, server)