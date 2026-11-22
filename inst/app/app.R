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

# Style sur mesure, pour se rapprocher de la maquette
app_theme <- bs_add_rules(app_theme, "
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

  /* Barre d'onglets : fond uni, onglets en pastilles, onglet actif coloré */
  .navbar {
    background-color: var(--bs-body-bg) !important;
    border-bottom: 1px solid var(--bs-border-color-translucent);
  }
  .navbar .nav-link {
    border-radius: .6rem; padding: .55rem .9rem !important;
    font-weight: 600; margin: 0 .1rem;
  }
  .navbar .nav-link.active {
    background: rgba(79,70,229,.12); color: var(--bs-primary) !important;
  }

  /* Boutons et champs plus arrondis */
  .btn { font-weight: 600; border-radius: .65rem; }
  .selectize-input, .form-control, .form-select { border-radius: .65rem; }

  /* Variables choisies affichées comme des puces colorées */
  .selectize-control.multi .selectize-input > div {
    background: rgba(79,70,229,.12); color: var(--bs-primary);
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

# ---------------------------------------------------------------------------
# Interface
# ---------------------------------------------------------------------------

ui <- page_navbar(
  id = "tabs",
  title = tags$span(class = "fw-bold",
                    bsicons::bs_icon("graph-up-arrow", class = "text-primary me-2"),
                    "ldapred"),
  window_title = "ldapred - Analyse discriminante prédictive",
  theme = app_theme,
  fillable = FALSE,

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
        radioButtons("sep", "Séparateur (fichiers texte uniquement)",
                     choices = c("Tabulation" = "\t", "Point-virgule" = ";",
                                 "Virgule" = ","),
                     selected = "\t"),
        radioButtons("dec", "Séparateur décimal",
                     choices = c("Point" = ".", "Virgule" = ","),
                     selected = ".", inline = TRUE)
      ),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("table"), "Aperçu des données"),
        card_body(
          uiOutput("data_badges"),
          DT::DTOutput("data_preview")
        )
      )
    )
  ),

  # ---- Onglet 2 : Variables ------------------------------------------------
  nav_panel(
    title = "Variables", value = "Variables", icon = bsicons::bs_icon("sliders"),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Récapitulatif", position = "right", width = 360,
        verbatimTextOutput("variables_info"),
        actionButton("run", "Lancer l'analyse", icon = icon("play"),
                     class = "btn-primary btn-lg w-100")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(class = "d-flex align-items-center gap-2",
                      bsicons::bs_icon("bullseye"), "1. Variable cible"),
          card_body(uiOutput("target_ui"))
        ),
        card(
          card_header(class = "d-flex align-items-center gap-2",
                      bsicons::bs_icon("gear"), "4. Paramètres du modèle"),
          card_body(
            selectInput("method", "Méthode de LDA",
                        choices = c("Classique" = "classic",
                                    "Grande dimension" = "highdim")),
            selectInput("selection", "Sélection de variables",
                        choices = c("Aucune" = "none",
                                    "Ascendante (forward)" = "forward",
                                    "Descendante (backward)" = "backward",
                                    "Pas à pas (stepwise)" = "stepwise"))
          ),
             # Seuil d'entrée : sélections forward et stepwise
            conditionalPanel(
              "input.selection == 'forward' || input.selection == 'stepwise'",
              sliderInput("alpha_in", "Seuil d'entrée",
                          min = 0.01, max = 0.20, value = 0.05, step = 0.01)
            ),
            # Seuil de maintien : sélections backward et stepwise
            conditionalPanel(
              "input.selection == 'backward' || input.selection == 'stepwise'",
              sliderInput("alpha_out", "Seuil de maintien",
                          min = 0.01, max = 0.20, value = 0.05, step = 0.01)
            ),
            uiOutput("threshold_warning")
        )
      ),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("list-check"), "2. Variables explicatives"),
        card_body(uiOutput("explanatory_ui"))
      ),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("lightbulb"),
                    "3. Variables illustratives (facultatif)"),
        card_body(
          helpText("Elles ne servent pas au modèle : elles aident à interpréter les classes prédites."),
          uiOutput("illustrative_ui")
        )
      )
    )
  ),

  # ---- Onglet 3 : Résultats ------------------------------------------------
  nav_panel(
    title = "Résultats", value = "Résultats", icon = bsicons::bs_icon("bar-chart"),

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
        showcase = bsicons::bs_icon("people", class = "text-success"),
        p(textOutput("vb_n_sub", inline = TRUE))
      ),
      value_box(
        title = "Variables du modèle",
        value = textOutput("vb_vars"),
        showcase = bsicons::bs_icon("layers", class = "text-warning"),
        p(textOutput("vb_vars_sub", inline = TRUE))
      ),
      value_box(
        title = "Classes",
        value = textOutput("vb_classes"),
        showcase = bsicons::bs_icon("tags", class = "text-info"),
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
        card_body(verbatimTextOutput("model_print"))
      )
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
  ),

  # ---- Onglet 4 : Prédiction -----------------------------------------------
  nav_panel(
    title = "Prédiction", value = "Prédiction", icon = bsicons::bs_icon("crosshair"),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("person"), "Un individu"),
        card_body(
          helpText("Lancez d'abord une analyse dans l'onglet Variables."),
          uiOutput("new_form"),
          actionButton("predict_one", "Prédire cet individu",
                       icon = icon("bullseye"), class = "btn-primary w-100")
        )
      ),
      card(
        card_header(class = "d-flex align-items-center gap-2",
                    bsicons::bs_icon("stars"), "Prédiction de l'individu"),
                card_body(uiOutput("pred_one_ui"))
      )
    ),
    card(
      card_header(class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("file-earmark-spreadsheet"),
                  "Un fichier d'individus"),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          fileInput("new_file",
                    "Fichier de nouveaux individus (mêmes options de lecture que l'onglet Données)",
                    accept = c(".csv", ".txt", ".tsv", ".xlsx"),
                    buttonLabel = "Parcourir...",
                    placeholder = "Aucun fichier sélectionné"),
          downloadButton("download_pred", "Télécharger les prédictions",
                         class = "btn-outline-primary w-100 mt-4")
        ),
        DT::DTOutput("pred_file")
      )
    )
  ),

  # ---- Onglet 5 : Aide -----------------------------------------------------
  nav_panel(
    title = "Aide", value = "Aide", icon = bsicons::bs_icon("question-circle"),
    layout_columns(
      col_widths = breakpoints(md = c(6, 6, 6, 6), lg = c(3, 3, 3, 3)),
      card(card_body(
        h5(bsicons::bs_icon("1-circle-fill", class = "text-primary"), " Charger"),
        p("Un fichier texte (tabulation par défaut), CSV ou Excel.")
      )),
      card(card_body(
        h5(bsicons::bs_icon("2-circle-fill", class = "text-success"), " Choisir"),
        p("La variable cible, les explicatives et, si besoin, des illustratives.")
      )),
      card(card_body(
        h5(bsicons::bs_icon("3-circle-fill", class = "text-warning"), " Analyser"),
        p("Taux de bonne classification, matrice de confusion, coefficients.")
      )),
      card(card_body(
        h5(bsicons::bs_icon("4-circle-fill", class = "text-info"), " Prédire"),
        p("Un nouvel individu au formulaire, ou tout un fichier à télécharger.")
      ))
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

  # ---- À droite de la barre : mode clair / sombre ---------------------------
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

# ---------------------------------------------------------------------------
# Serveur
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Données d'apprentissage, lues avec read_data()
  data <- reactive({
    req(input$file)
    df <- read_data(input$file$datapath, input$file$name, input$sep, input$dec)
    req(df)
    df
  })

  # Pastilles : lignes, colonnes, numériques, catégorielles
  output$data_badges <- renderUI({
    df <- data()
    n_num <- sum(vapply(df, is.numeric, logical(1)))
    n_cat <- ncol(df) - n_num
    badge <- function(text, cls) {
      span(class = paste("badge rounded-pill fs-6 fw-semibold px-3 py-2", cls), text)
    }
    div(class = "d-flex flex-wrap gap-2 mb-3",
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
    selectInput("target", "Variable cible (catégorielle)", choices = cat_cols)
  })

  # Menu des explicatives : toutes les colonnes sauf la cible
  output$explanatory_ui <- renderUI({
    req(input$target)
    others <- setdiff(names(data()), input$target)
    selectInput("explanatory", "Variables explicatives",
                choices = others, selected = others, multiple = TRUE)
  })

  # Menu des illustratives : toutes les colonnes sauf la cible (facultatif)
  output$illustrative_ui <- renderUI({
    req(input$target)
    others <- setdiff(names(data()), input$target)
    selectInput("illustrative", "Variables illustratives (facultatif)",
                choices = others, multiple = TRUE)
  })

  # Récapitulatif des choix, avec contrôle de cohérence
  output$variables_info <- renderText({
    req(input$target)
    validate(
      need(length(input$explanatory) > 0,
           "Choisissez au moins une variable explicative."),
      need(length(intersect(input$explanatory, input$illustrative)) == 0,
           "Une variable ne peut pas être à la fois explicative et illustrative.")
    )
    paste0(
      "Cible         : ", input$target, " (",
      nlevels(data()[[input$target]]), " classes)\n",
      "Explicatives  : ", paste(input$explanatory, collapse = ", "), "\n",
      "Illustratives : ", if (length(input$illustrative) > 0)
                            paste(input$illustrative, collapse = ", ") else "aucune", "\n",
      "Méthode       : ", input$method, "\n",
      "Sélection     : ", input$selection,
      switch(input$selection,
        forward  = paste0(" (entrée ", input$alpha_in, ")"),
        backward = paste0(" (maintien ", input$alpha_out, ")"),
        stepwise = paste0(" (entrée ", input$alpha_in, ", maintien ", input$alpha_out, ")"),
        "")
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

    # A FAIRE : transmettre input$alpha_in et input$alpha_out quand les classes
    # de sélection auront fixé leurs paramètres
    m <- LDAClassifier$new(method = input$method, selection = input$selection)


    res <- withProgress(message = "Analyse en cours...", value = 0.5, {
      safe_run(m$fit(X, y), "Erreur lors de l'analyse :")
    })

    # Prédictions sur les données d'apprentissage (les avertissements sur les
    # NA ont déjà été affichés pendant le fit)
    pred_train <- suppressWarnings(m$predict(X))

    showNotification("Analyse terminée.", type = "message")
    list(model = m, train = X, vars = input$explanatory, y = y,
         pred_train = pred_train, illus = df[input$illustrative])
  })

  # Après une analyse réussie, on bascule sur l'onglet Résultats
  observeEvent(analysis(), {
    nav_select("tabs", selected = "Résultats", session = session)
  })

  # Affichage du modèle
  output$model_print <- renderPrint({
    print(analysis()$model)
  })

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
    cm <- analysis()$model$confusion
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
                else sprintf("rgba(79, 70, 229, %.2f)", 0.15 + 0.85 * share)
          fg <- if (share > 0.5) "#ffffff" else "var(--bs-body-color)"
          tags$td(v, class = "text-center fw-bold fs-5",
                  style = sprintf("background:%s; color:%s; border-radius:.6rem; height:64px; vertical-align:middle;",
                                  bg, fg))
        })
      )
    })

    tags$table(class = "w-100", style = "border-collapse: separate; border-spacing: 6px;",
               tags$thead(header), tags$tbody(body))
  })

  output$confusion_note <- renderText({
    cm <- analysis()$model$confusion
    sprintf("%d individus sur %d bien classés.", sum(diag(cm)), sum(cm))
  })

  # Taux de bonne classification : fourni par le package
  output$accuracy <- renderText({
    acc <- analysis()$model$accuracy
    sprintf("Taux de bonne classification : %.1f %% (taux d'erreur : %.1f %%)",
            100 * acc, 100 * (1 - acc))
  })


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
    lapply(seq_along(a$vars), function(i) {
      v <- a$vars[i]
      col <- a$train[[v]]
      id <- paste0("new_", i)
      if (is.numeric(col)) {
        numericInput(id, v, value = round(stats::median(col, na.rm = TRUE), 2))
      } else {
        selectInput(id, v, choices = levels(as.factor(col)))
      }
    })
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
    res <- pred_one()
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