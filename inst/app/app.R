library(shiny)

# Chargement du package (sauf s'il est déjà chargé par devtools::load_all())
if (!"ldapred" %in% loadedNamespaces()) library(ldapred)

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

# ---------------------------------------------------------------------------
# Interface
# ---------------------------------------------------------------------------
ui <- navbarPage(
  title = "ldapred - Analyse discriminante prédictive",
  id = "tabs",

  # Onglet 1 : import des données
  tabPanel("Données",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choisir un fichier de données (CSV, TXT ou XLSX)",
                  accept = c(".csv", ".txt", ".tsv", ".xlsx"),
                  buttonLabel = "Parcourir...",
                  placeholder = "Aucun fichier sélectionné"),
        radioButtons("sep", "Séparateur (fichiers texte uniquement)",
                     choices = c("Tabulation" = "\t", "Point-virgule" = ";",
                                 "Virgule" = ","),
                     selected = "\t"),
        radioButtons("dec", "Séparateur décimal",
                     choices = c("Point" = ".", "Virgule" = ","),
                     selected = ".")
      ),
      mainPanel(
        textOutput("data_info"),
        DT::DTOutput("data_preview")
      )
    )
  ),

  # Onglet 2 : choix des variables et des paramètres
  tabPanel("Variables",
    sidebarLayout(
      sidebarPanel(
        uiOutput("target_ui"),
        uiOutput("explanatory_ui"),
        uiOutput("illustrative_ui"),
        hr(),
        selectInput("method", "Méthode de LDA",
                    choices = c("Classique" = "classic",
                                "Grande dimension" = "highdim")),
        selectInput("selection", "Sélection de variables",
                    choices = c("Aucune" = "none",
                                "Ascendante (forward)" = "forward",
                                "Descendante (backward)" = "backward",
                                "Pas à pas (stepwise)" = "stepwise")),
        hr(),
        actionButton("run", "Lancer l'analyse", class = "btn-primary")
      ),
      mainPanel(
        verbatimTextOutput("variables_info")
      )
    )
  ),

  # Onglet 3 : résultats du modèle
  tabPanel("Résultats",
    fluidRow(
      column(6,
        h4("Description du modèle"),
        verbatimTextOutput("model_print")
      ),
      column(6,
        h4("Qualité du classement (données d'apprentissage)"),
        textOutput("accuracy"),
        helpText("Lignes : classes observées. Colonnes : classes prédites."),
        tableOutput("confusion")
      )
    ),
    hr(),
    h4("Coefficients des fonctions de classement"),
    tableOutput("coefficients"),
    hr(),
    h4("Variables illustratives"),
    uiOutput("illus_ui"),
    tableOutput("illus_table")
  ),

  # Onglet 4 : prédiction de nouveaux individus
  tabPanel("Prédiction",
    sidebarLayout(
      sidebarPanel(
        helpText("Lancez d'abord une analyse dans l'onglet Variables."),
        h4("Un individu"),
        uiOutput("new_form"),
        actionButton("predict_one", "Prédire cet individu", class = "btn-primary"),
        hr(),
        h4("Un fichier d'individus"),
        fileInput("new_file",
                  "Choisir un fichier (mêmes options de lecture que l'onglet Données)",
                  accept = c(".csv", ".txt", ".tsv", ".xlsx"),
                  buttonLabel = "Parcourir...",
                  placeholder = "Aucun fichier sélectionné"),
        downloadButton("download_pred", "Télécharger les prédictions")
      ),
      mainPanel(
        h4("Prédiction de l'individu"),
        tableOutput("pred_one"),
        hr(),
        h4("Prédictions du fichier"),
        DT::DTOutput("pred_file")
      )
    )
  )
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

  # Nombre de lignes et de colonnes
  output$data_info <- renderText({
    df <- data()
    n_num <- sum(vapply(df, is.numeric, logical(1)))
    paste0(nrow(df), " lignes et ", ncol(df), " colonnes chargées (",
           n_num, " numériques, ", ncol(df) - n_num, " catégorielles).")
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
      "Sélection     : ", input$selection
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

    df <- data()
    X <- df[input$explanatory]
    y <- df[[input$target]]
    m <- LDAClassifier$new(method = input$method, selection = input$selection)

    res <- safe_run(m$fit(X, y), "Erreur lors de l'analyse :")
    req(res)

    # Prédictions sur les données d'apprentissage (les avertissements sur les
    # NA ont déjà été affichés pendant le fit)
    pred_train <- suppressWarnings(m$predict(X))

    showNotification("Analyse terminée.", type = "message")
    list(model = m, train = X, vars = input$explanatory, y = y,
         pred_train = pred_train, illus = df[input$illustrative])
  })

  # Après une analyse réussie, on bascule sur l'onglet Résultats
  observeEvent(analysis(), {
    updateNavbarPage(session, "tabs", selected = "Résultats")
  })

  # Affichage du modèle
  output$model_print <- renderPrint({
    print(analysis()$model)
  })

  # Taux de bonne classification : fourni par le package
  output$accuracy <- renderText({
    acc <- analysis()$model$accuracy
    sprintf("Taux de bonne classification : %.1f %% (taux d'erreur : %.1f %%)",
            100 * acc, 100 * (1 - acc))
  })

  # Matrice de confusion : fournie par le package
  output$confusion <- renderTable({
    as.data.frame.matrix(analysis()$model$confusion)
  }, rownames = TRUE)

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

  # Croisement de la variable illustrative avec les classes prédites
  output$illus_table <- renderTable({
    a <- analysis()
    req(input$illus_var, input$illus_var %in% names(a$illus))
    col <- a$illus[[input$illus_var]]
    if (is.numeric(col)) {
      # Numérique : moyenne de la variable dans chaque classe prédite
      means <- tapply(col, a$pred_train, mean, na.rm = TRUE)
    
        data.frame("Moyenne" = as.numeric(means), row.names = names(means), check.names = FALSE)
    } else {
      # Catégorielle : effectifs croisés classe prédite x modalité
      as.data.frame.matrix(table(a$pred_train, col))
    }
  }, rownames = TRUE, digits = 2)

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

  output$pred_one <- renderTable({
    pred_one()
  }, digits = 3)

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
    DT::datatable(pred_file(), options = list(pageLength = 10, scrollX = TRUE,
                                              language = dt_french))
  })

  # Téléchargement des prédictions (CSV compatible Excel français)
  output$download_pred <- downloadHandler(
    filename = function() "predictions.csv",
    content = function(file) {
      utils::write.csv2(pred_file(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)