# mod_esquisse.R


mod_esquisse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("launch"), "Lancer Esquisse"),
    conditionalPanel(
      condition = sprintf("input['%s'] > 0", ns("launch")),
      tagList(
        conditionalPanel(
          condition = sprintf("!input['%s'] || input['%s'].length == 0", ns("convert_to_date"), ns("convert_to_date")),
          selectInput(
            ns("convert_to_date"),
            label = "Variables à convertir en Date (facultatif)",
            choices = NULL,  # <-- THIS WAS MISSING
            multiple = TRUE
          )
        ),
        esquisse_ui(ns("esquisse"), header = FALSE)
      )
    )
  )
}

mod_esquisse_server <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_r <- reactiveValues(data = data.frame(), name = "placeholder")
    
    observeEvent(input$launch, {
      req(data_in())
      data_r$data <- data_in()
      data_r$name <- "Données"
      
      # After launch, list factor variables as candidates
      updateSelectInput(session, "convert_to_date",
                        choices = names(data_in())[sapply(data_in(), is.factor)])
    })
    
    observeEvent(input$convert_to_date, {
      req(input$convert_to_date)
      
      for (var in input$convert_to_date) {
        vec <- as.character(data_r$data[[var]])
        
        # Check if the strings contain a 4-digit number (likely a year)
        has_year <- grepl("\\b\\d{4}\\b", vec)
        
        if (!any(has_year)) {
          # No year detected at all: skip conversion
          showNotification(
            paste("Conversion ignorée pour la variable", var, ": absence d'année (ex : 2025) détectée."),
            type = "warning",
            duration = 7
          )
          next  # move to next variable
        }
        
        success <- FALSE
        
        tmp <- suppressWarnings(lubridate::dmy(vec, locale = "fr_FR.UTF-8"))
        if (all(!is.na(tmp))) {
          data_r$data[[var]] <- tmp
          success <- TRUE
        } else {
          tmp_en <- suppressWarnings(lubridate::dmy(vec))
          if (all(!is.na(tmp_en))) {
            data_r$data[[var]] <- tmp_en
            success <- TRUE
          } else {
            tmp2 <- suppressWarnings(lubridate::ymd(vec, truncated = 2))
            if (all(!is.na(tmp2))) {
              data_r$data[[var]] <- tmp2
              success <- TRUE
            } else {
              tmp3 <- suppressWarnings(lubridate::my(vec, locale = "fr_FR.UTF-8"))
              if (all(!is.na(tmp3))) {
                data_r$data[[var]] <- as.Date(tmp3)
                success <- TRUE
              } else {
                tmp3_en <- suppressWarnings(lubridate::my(vec))
                if (all(!is.na(tmp3_en))) {
                  data_r$data[[var]] <- as.Date(tmp3_en)
                  success <- TRUE
                }
              }
            }
          }
        }
        
        if (!success) {
          showNotification(
            paste("Impossible de convertir la variable", var, "en Date."),
            type = "warning",
            duration = 5
          )
        }
      }
    })
    
    esquisse_server(
      id = "esquisse",
      data_rv = data_r
    )
  })
}
