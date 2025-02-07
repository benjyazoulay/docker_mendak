
load.lib <- c("shiny", 
              "DT",
              "sortable", 
              "tidyverse", 
              "ggplot2", 
              "quanteda", 
              "rainette",
              "wordcloud",
              "dplyr",
              "readxl",
              "writexl",
              "quanteda.textplots",
              "quanteda.textstats",
              "FactoMineR",
              "factoextra",
              "ggpubr",
              "ggrepel",
              "paletteer",
              "udpipe",
              "openxlsx"
) # Ce sont les paquets dont on va avoir besoin

install.lib <- load.lib[!load.lib %in% installed.packages()] # On regarde les paquets qui ne sont pas installés

for (lib in install.lib) install.packages(lib,dependencies=TRUE) # On installe ceux-ci

sapply(load.lib,require,character=TRUE) # Et on charge tous les paquets nécessaires

french_stopwords<-read.csv2("french_stopwords.csv")

options(shiny.maxRequestSize=100*1024^2)


# Server
server <- function(input, output, session) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  dataset <- reactiveVal(NULL)
  var_info <- reactiveVal(NULL)
  
  # Function to read various file formats
  read_file <- function(file) {
    ext <- tools::file_ext(file$datapath)
    data <- switch(ext,
                   csv = read.csv(file$datapath),
                   xls = read_excel(file$datapath,na=""),
                   xlsx = read_excel(file$datapath,na=""),
                   rdata = get(load(file$datapath)),
                   rds = readRDS(file$datapath),
                   stop("Unsupported file format")
    )
    data[is.na(data)] <- ""  # Remplace les NA par des chaînes vides
    return(data)
  }
  
  # Function to detect variable types
  detect_var_types <- function(data) {
    sapply(data, function(x) {
      if (is.numeric(x)) {
        return("numeric")
      } else if (is.character(x) || is.factor(x)) {
        if (mean(nchar(as.character(x))) < 50) {
          return("factor")
        } else {
          return("character")
        }
      } else {
        return("other")
      }
    })
  }
  
  # Load data
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      data <- read_file(input$file1)
      var_types <- detect_var_types(data)
      for (col in names(data)) {
        if (var_types[col] == "factor") {
          data[[col]] <- as.factor(data[[col]])
        } else if (var_types[col] == "character") {
          data[[col]] <- as.character(data[[col]])
        }
      }
      # Add word count columns
      char_cols <- names(data)[sapply(data, is.character)]
      for (col in char_cols) {
        new_col_name <- paste0("nwords_", col)
        data[[new_col_name]] <- str_count(data[[col]], "\\S+")
      }
      
      dataset(data)
      var_info(lapply(names(data), function(var) {
        list(
          name = var,
          type = class(data[[var]])[1],
          levels = if(is.factor(data[[var]])) levels(data[[var]]) else NULL
        )
      }))
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  observeEvent(input$load_sample_eng, {
    sample_data <- read.csv("https://mathieuferry.github.io/datasets/sampleb.csv")
    var_types <- detect_var_types(sample_data)
    for (col in names(sample_data)) {
      if (var_types[col] == "factor") {
        sample_data[[col]] <- as.factor(sample_data[[col]])
      } else if (var_types[col] == "character") {
        sample_data[[col]] <- as.character(sample_data[[col]])
      }
    }
    # Add word count columns
    char_cols <- names(sample_data)[sapply(sample_data, is.character)]
    for (col in char_cols) {
      new_col_name <- paste0("nwords_", col)
      sample_data[[new_col_name]] <- str_count(sample_data[[col]], "\\S+")
    }
    
    dataset(sample_data)
    var_info(lapply(names(sample_data), function(var) {
      list(
        name = var,
        type = class(sample_data[[var]])[1],
        levels = if(is.factor(sample_data[[var]])) levels(sample_data[[var]]) else NULL
      )
    }))
  })
  
  observeEvent(input$load_sample_fr, {
    sample_data <- read.csv("https://mathieuferry.github.io/datasets/admin_harmoral_dec24.csv")
    var_types <- detect_var_types(sample_data)
    for (col in names(sample_data)) {
      if (var_types[col] == "factor") {
        sample_data[[col]] <- as.factor(sample_data[[col]])
      } else if (var_types[col] == "character") {
        sample_data[[col]] <- as.character(sample_data[[col]])
      }
    }
    # Add word count columns
    char_cols <- names(sample_data)[sapply(sample_data, is.character)]
    for (col in char_cols) {
      new_col_name <- paste0("nwords_", col)
      sample_data[[new_col_name]] <- str_count(sample_data[[col]], "\\S+")
    }
    
    dataset(sample_data)
    var_info(lapply(names(sample_data), function(var) {
      list(
        name = var,
        type = class(sample_data[[var]])[1],
        levels = if(is.factor(sample_data[[var]])) levels(sample_data[[var]]) else NULL
      )
    }))
  })
  
  observeEvent(input$load_sample_fr2, {
    
    sample_data <- openxlsx::read.xlsx("https://mathieuferry.github.io/datasets/wokisme_pressenationalefr.xlsx")
    sample_data[is.na(sample_data)] <- ""
    
    var_types <- detect_var_types(sample_data)
    for (col in names(sample_data)) {
      if (var_types[col] == "factor") {
        sample_data[[col]] <- as.factor(sample_data[[col]])
      } else if (var_types[col] == "character") {
        sample_data[[col]] <- as.character(sample_data[[col]])
      }
    }
    # Add word count columns
    char_cols <- names(sample_data)[sapply(sample_data, is.character)]
    for (col in char_cols) {
      new_col_name <- paste0("nwords_", col)
      sample_data[[new_col_name]] <- str_count(sample_data[[col]], "\\S+")
    }
    
    dataset(sample_data)
    var_info(lapply(names(sample_data), function(var) {
      list(
        name = var,
        type = class(sample_data[[var]])[1],
        levels = if(is.factor(sample_data[[var]])) levels(sample_data[[var]]) else NULL
      )
    }))
  })
  
  
  
  # Data summary
  output$data_summary <- renderText({
    req(dataset())
    data <- dataset()
    paste("Rows:", nrow(data), "\nColumns:", ncol(data))
  })
  
  # Variable management UI ---------------------
  # Function to generate variable management UI
  # Function to generate variable management UI
  generate_var_management_ui <- function() {
    req(var_info())
    lapply(var_info(), function(var) {
      fluidRow(
        column(2, textInput(paste0(var$name, "_name"), "Name", value = var$name)),
        column(2, selectInput(paste0(var$name, "_type"), "Type",
                              choices = c("numeric", "factor", "character"),
                              selected = var$type)),
        column(4, if(var$type == "factor") {
          tagList(
            sortable::sortable_js(paste0(var$name, "_levels")),
            tags$div(id = paste0(var$name, "_levels"),
                     lapply(seq_along(var$levels), function(i) {
                       tags$div(
                         style = "display: flex; align-items: center; margin-bottom: 5px;",
                         tags$span(class = "glyphicon glyphicon-menu-hamburger",
                                   style = "margin-right: 10px; cursor: move;"),
                         textInput(paste0(var$name, "_level_", i),
                                   label = NULL,
                                   value = var$levels[i])
                       )
                     })
            ),
            tags$script(sprintf("
    var sortable_%s = new Sortable(document.getElementById('%s_levels'), {
      onSort: function (evt) {
        var itemElems = evt.from.children;
        var newOrder = Array.prototype.map.call(itemElems, function(itemElem) {
          return itemElem.querySelector('input').value;
        });
        Shiny.setInputValue('%s_level_order', newOrder);
      }
    });
  ", var$name, var$name, var$name)),
            verbatimTextOutput(paste0(var$name, "_debug"))
          )
        } else {
          NULL
        }),
        column(2, checkboxInput(paste0(var$name, "_create_new"), "Create New Column")),
        column(2, actionButton(paste0(var$name, "_apply"), "Apply Changes"))
      )
    })
  }
  
  # Initial render of variable management UI
  output$variable_management <- renderUI({
    generate_var_management_ui()
  })
  
  # Initialize var_info reactiveVal
  var_info <- reactiveVal(list())
  
  # Initialize dataset reactiveVal
  dataset <- reactiveVal(data.frame())
  
  # Function to update var_info
  update_var_info <- function(data) {
    var_info(lapply(names(data), function(v) {
      list(
        name = v,
        type = class(data[[v]])[1],
        levels = if(is.factor(data[[v]])) levels(data[[v]]) else NULL
      )
    }))
  }
  
  # Initialize observers for each variable
  observe({
    data <- dataset()
    lapply(names(data), function(var_name) {
      observeEvent(input[[paste0(var_name, "_apply")]], {
        new_name <- input[[paste0(var_name, "_name")]]
        new_type <- input[[paste0(var_name, "_type")]]
        create_new <- input[[paste0(var_name, "_create_new")]]
        
        current_var <- data[[var_name]]
        
        if (new_type == "numeric") {
          if (is.numeric(current_var)) {
            new_var <- current_var
          } else {
            if (all(!is.na(suppressWarnings(as.numeric(as.character(current_var)))))) {
              new_var <- as.numeric(as.character(current_var))
            } else {
              showNotification("Cannot convert to numeric. Some values are non-numeric.", type = "error")
              return()
            }
          }
        } else if (new_type == "factor") {
          if (is.factor(current_var)) {
            old_levels <- levels(current_var)
            
            # Get the new order of levels from the JavaScript-updated input
            new_levels_order <- input[[paste0(var_name, "_level_order")]] %||%
              sapply(seq_along(old_levels), function(i) {
                input[[paste0(var_name, "_level_", i)]]
              })
            
            # Remove any NA or empty values
            new_levels_order <- new_levels_order[!is.na(new_levels_order) & new_levels_order != ""]
            
            print("Debug: UI Inputs (New Levels Order)")
            print(new_levels_order)
            
            # Reorder the levels without changing the underlying data
            new_var <- factor(current_var, levels = new_levels_order)
            
            print("Debug: Updated Factor Levels")
            print(levels(new_var))
            
            # Verify the order of unique values in the new factor
            print("Debug: Unique values in new factor")
            print(unique(new_var))
          } else {
            unique_values <- unique(current_var)
            if (length(unique_values) <= 10) {
              new_var <- factor(current_var)
            } else {
              showNotification("Cannot convert to factor. More than 10 unique values.", type = "error")
              return()
            }
          }
        }  else {
          new_var <- as.character(current_var)
        }
        
        if (create_new && new_name != var_name) {
          data[[new_name]] <- new_var
        } else {
          data[[var_name]] <- new_var
          if (new_name != var_name) {
            names(data)[names(data) == var_name] <- new_name
          }
        }
        
        # Update the dataset
        dataset(data)
        
        # Update var_info
        update_var_info(data)
        
        # Force re-render of variable management UI
        output$variable_management <- renderUI({
          generate_var_management_ui()
        })
        
        # Trigger an event to notify other parts of the app that the data has changed
        session$sendCustomMessage(type = 'datasetUpdated', message = list())
        
        showNotification("Variable updated successfully", type = "message")
      })
    })
  })
  
  # React to changes in dataset structure
  observe({
    data <- dataset()
    update_var_info(data)
    
    # Force re-render of variable management UI
    output$variable_management <- renderUI({
      generate_var_management_ui()
    })
  })
  
  # Data view
  output$data_view <- renderDT({
    req(dataset())
    datatable(dataset(), 
              editable = TRUE,
              options = list(scrollX = TRUE,
                             lengthMenu = c(5,10),
                             columnDefs = list(list(
                               targets = "_all",
                               render = JS(
                                 "function(data, type, row, meta) {",
                                 "return type === 'display' && data != null && data.length > 100 ?",
                                 "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                                 "}")
                             ))),
              class = "display")
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset", switch(input$export_format,
                              csv = ".csv",
                              xls = ".xls",
                              xlsx = ".xlsx",
                              rdata = ".RData",
                              rds = ".rds"), sep = "")
    },
    content = function(file) {
      switch(input$export_format,
             csv = write.csv(dataset(), file, row.names = FALSE),
             xls = write_xlsx(dataset(), file),
             xlsx = write_xlsx(dataset(), file),
             rdata = save(dataset, file = file),
             rds = saveRDS(dataset(), file)
      )
    }
  )
  
  # Univariate analysis ---------------
  output$var_select_uni <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]
    numeric_vars <- names(dataset())[sapply(dataset(), is.numeric)]
    
    selectInput("var_uni", "Select Variable:", choices = c(factor_vars,numeric_vars))
  })
  
  output$var_calc_uni<-renderUI({
    req(input$var_uni,dataset())
    var_uni<-input$var_uni
    data<-dataset()
    
    if(is.factor(data[[var_uni]])){
      radioButtons("calc_uni","Choice of calculation on plot:",choices=c("Frequency (N)","Proportion (%)"))
    }
  })
  
  output$univariate_table <- renderDT({
    req(input$var_uni)
    var <- input$var_uni
    data<-dataset()
    if(is.numeric(data[[var]])) {
      tab<-as.data.frame(t(summary(data[[var]])))
      tab<- tab %>% mutate(Descriptive=Var2) %>% select(c(Descriptive,Freq))
      datatable(tab)
    } else {
      
      result<- data %>% count(!!sym(var)) %>% mutate(proportion=round(n/sum(n)*100,2))
      # freq_table <- table(var)
      # result <- data.frame(
      #   Category = names(freq_table),
      #   Frequency = as.vector(freq_table),
      #   Percentage = round(100 * prop.table(freq_table), 2)
      # )
      datatable(result)
    }
  })
  
  output$univariate_plot <- renderPlot({
    req(input$var_uni,dataset())
    data<-dataset()
    var <- input$var_uni
    if(is.numeric(data[[var]])) {
      ggplot(dataset(), aes(x=!!sym(var))) + 
        geom_histogram(binwidth=1)+
        labs(title=paste("Histogram of", var),x=var)+
        theme_minimal()+
        theme(axis.text=element_text(size=18),axis.title=element_text(size=18))
      
    } else {
      result<-data %>% count(!!sym(var)) %>% mutate(proportion=round(n/sum(n)*100,2))
      if(input$calc_uni=="Frequency (N)"){
        ggplot(data=result, aes(x=!!sym(var), y=n)) +
          geom_bar(stat="identity")+
          geom_text(aes(label=n), vjust=1.6, color="white", size=6)+
          labs(title=paste("Barplot of", var),x=var)+
          theme_minimal()+
          theme(axis.text=element_text(size=18),axis.title=element_text(size=18))
        
      }
      else{
        ggplot(data=result, aes(x=!!sym(var), y=proportion)) +
          geom_bar(stat="identity")+
          geom_text(aes(label=paste0(round(proportion,1),"%")), vjust=1.6, color="white", size=6)+
          labs(title=paste("Barplot of", var),x=var)+
          theme_minimal()+
          theme(axis.text=element_text(size=18),axis.title=element_text(size=18))
        
      }
    }
  })
  
  
  # Bivariate analysis ---------------
  
  # Bivariate analysis
  output$var_select_bi <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]
    numeric_vars <- names(dataset())[sapply(dataset(), is.numeric)]
    
    
    selectInput("var_bi", "Select Dependent Variable:", choices = c(factor_vars,numeric_vars))
  })
  
  output$var_select_bi_exp <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]
    numeric_vars <- names(dataset())[sapply(dataset(), is.numeric)]
    vars<-c(factor_vars,numeric_vars)
    selectInput("var_bi_exp", "Select Explanatory Variable:", choices = c(vars),selected=vars[2])
  })
  
  output$display_option <- renderUI({
    req(input$var_bi, input$var_bi_exp, dataset())
    data <- dataset()
    
    if(is.factor(data[[input$var_bi]])) {
      radioButtons("display_type", "Display:", choices = c("Frequency (N)", "Proportion (%)"))
    }
  })
  
  output$bivariate_table <- renderDT({
    req(input$var_bi, input$var_bi_exp, dataset())
    data <- dataset()
    
    if(!input$var_bi %in% names(data) || !input$var_bi_exp %in% names(data)) {
      return(datatable(data.frame(Error = "One or both selected variables not found in dataset")))
    }
    
    var_bi <- data[[input$var_bi]]
    var_bi_exp <- data[[input$var_bi_exp]]
    
    if(is.numeric(var_bi) && is.factor(var_bi_exp)) {
      result <- data %>%
        group_by(!!sym(input$var_bi_exp)) %>%
        summarise(Mean = round(mean(!!sym(input$var_bi), na.rm = TRUE), 1))
      # Calculate overall mean
      overall_mean <- data %>%
        summarise(Mean = round(mean(!!sym(input$var_bi), na.rm = TRUE), 1)) %>%
        mutate(!!sym(input$var_bi_exp) := "All")
      # Combine group means with overall mean
      result <- bind_rows(result, overall_mean)        
      
    } else if(is.numeric(var_bi) && is.numeric(var_bi_exp)) {
      var_bi_exp_cat <- cut(var_bi_exp, breaks = quantile(var_bi_exp, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
      result <- data %>%
        mutate(Quartile = var_bi_exp_cat) %>%
        group_by(Quartile) %>%
        summarise(Mean = round(mean(!!sym(input$var_bi), na.rm = TRUE),1)) 
      overall_mean <- data %>%
        summarise(Mean = round(mean(!!sym(input$var_bi), na.rm = TRUE), 1)) %>%
        mutate(Quartile = "All")
      result <- bind_rows(result, overall_mean)
      
    } else if(is.factor(var_bi) && is.numeric(var_bi_exp)) {
      var_bi_exp_cat <- cut(var_bi_exp, breaks = quantile(var_bi_exp, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
      data<-data %>%
        mutate(Quartile = var_bi_exp_cat)
      result <- data %>%
        count(Quartile, !!sym(input$var_bi))
      total_row <- result %>%
        group_by(!!sym(input$var_bi)) %>%
        summarise(n = sum(n)) %>%
        mutate(Quartile := "All")
      
      result <- bind_rows(result, total_row) %>%
        group_by(Quartile)
      
      
      
      if(input$display_type == "Proportion (%)") {
        result <- result %>%
          mutate(Value = round(n / sum(n) * 100, 1)) %>%
          select(-n)
      } else {
        result <- result %>%
          rename(Value = n)
      }
      
      result <- result %>%
        pivot_wider(names_from = !!sym(input$var_bi), values_from = Value, values_fill = 0)
      
      # Add total column
      if(input$display_type == "Proportion (%)") {
        result <- result %>%
          mutate(Total = 100)
      } else {
        result <- result %>% ungroup() %>%
          mutate(Total = rowSums(select(., -1)))
      }
      
    }else if(is.factor(var_bi) && is.factor(var_bi_exp)) {
      result <- data %>%
        count(!!sym(input$var_bi_exp), !!sym(input$var_bi))
      total_row <- result %>%
        group_by(!!sym(input$var_bi)) %>%
        summarise(n = sum(n)) %>%
        mutate(!!sym(input$var_bi_exp) := "All")
      
      result <- bind_rows(result, total_row) %>%
        group_by(!!sym(input$var_bi_exp))
      
      
      
      if(input$display_type == "Proportion (%)") {
        result <- result %>%
          mutate(Value = round(n / sum(n) * 100, 1)) %>%
          select(-n)
      } else {
        result <- result %>%
          rename(Value = n)
      }
      
      result <- result %>%
        pivot_wider(names_from = !!sym(input$var_bi), values_from = Value, values_fill = 0)
      
      # Add total column
      if(input$display_type == "Proportion (%)") {
        result <- result %>%
          mutate(Total = 100)
      } else {
        result <- result %>% ungroup() %>%
          mutate(Total = rowSums(select(., -1)))
      }
    } else {
      return(datatable(data.frame(Error = "Invalid combination of variable types")))
    }
    
    # Transpose the result
    # result_t <- data.frame(t(result[-1]))
    # colnames(result_t) <- result[[1]]
    # result_t$Variable <- rownames(result_t)
    # result_t <- result_t %>% select(Variable, everything())
    
    datatable(result)
  })
  
  output$bivariate_plot <- renderPlot({
    req(input$var_bi, input$var_bi_exp, dataset())
    data <- dataset()
    
    if(!input$var_bi %in% names(data) || !input$var_bi_exp %in% names(data)) {
      return(ggplot() + 
               geom_text(aes(x = 0, y = 0, label = "One or both selected variables not found in dataset")) + 
               theme_void())
    }
    
    var_bi <- data[[input$var_bi]]
    var_bi_exp <- data[[input$var_bi_exp]]
    
    if(is.numeric(var_bi) && is.factor(var_bi_exp)) {
      ggplot(data, aes(x = !!sym(input$var_bi_exp), y = !!sym(input$var_bi))) +
        geom_boxplot() +
        labs(x = input$var_bi_exp, y = input$var_bi) +
        theme_minimal()+
        theme(axis.title = element_text(size=18),axis.text = element_text(size=18))
    } else if(is.numeric(var_bi) && is.numeric(var_bi_exp)) {
      ggplot(data, aes(x = !!sym(input$var_bi_exp), y = !!sym(input$var_bi))) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = input$var_bi_exp, y = input$var_bi,caption=paste("Pearson's correlation: ",sprintf('%#.2f',cor(var_bi,var_bi_exp,use="complete.obs")))) +
        theme_minimal()+
        theme(axis.title = element_text(size=18),axis.text = element_text(size=18),plot.caption = element_text(size=18))
      
    } else if(is.factor(var_bi) && is.numeric(var_bi_exp)) {
      
      var_bi_exp_cat <- cut(var_bi_exp, breaks = quantile(var_bi_exp, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
      data<-data %>%
        mutate(Quartile = var_bi_exp_cat)
      plot_data <- data %>%
        count(Quartile, !!sym(input$var_bi)) %>%
        group_by(Quartile) %>%
        mutate(prop = n / sum(n),
               label = sprintf("%.1f%%", prop * 100))
      plot <- ggplot(plot_data, aes(x = Quartile, y = prop, fill = !!sym(input$var_bi)))
      
      if(input$display_type == "Proportion (%)") {
        plot <- plot +
          geom_bar(stat = "identity", position = "fill") +
          geom_text(aes(label = label), position = position_fill(vjust = 0.5), size = 6) +
          scale_y_continuous(labels = scales::percent) +
          labs(x = paste(input$var_bi_exp," (quartile categorization)"), y = "Proportion", fill = input$var_bi)
        
      } else {
        plot <- plot +
          geom_bar(stat = "identity", position = "stack") +
          geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 6) +
          labs(x = paste(input$var_bi_exp," (quartile categorization)"), y = "Frequency", fill = input$var_bi)
      }
      
      plot + theme_minimal() + scale_fill_brewer(palette="Spectral")+ theme(axis.title = element_text(size=18),axis.text = element_text(size=18))
      
    } else if(is.factor(var_bi) && is.factor(var_bi_exp)) {
      plot_data <- data %>%
        count(!!sym(input$var_bi_exp), !!sym(input$var_bi)) %>%
        group_by(!!sym(input$var_bi_exp)) %>%
        mutate(prop = n / sum(n),
               label = sprintf("%.1f%%", prop * 100))
      
      if(input$display_type == "Proportion (%)") {
        plot <- ggplot(plot_data, aes(x = !!sym(input$var_bi_exp), y = prop, fill = fct_rev(!!sym(input$var_bi))))
        
        plot <- plot +
          geom_bar(stat = "identity", position = "fill") +
          geom_text(aes(label = label), position = position_fill(vjust = 0.5), size = 6) +
          scale_y_continuous(labels = scales::percent) +
          labs(x = input$var_bi_exp, y = "Proportion", fill = input$var_bi)
      } else {
        plot <- ggplot(plot_data, aes(x = !!sym(input$var_bi_exp), y = n, fill = fct_rev(!!sym(input$var_bi))))
        
        plot <- plot +
          geom_bar(stat = "identity", position = "stack") +
          geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 6) +
          labs(x = input$var_bi_exp, y = "Frequency", fill = input$var_bi)
      }
      
      plot + theme_minimal()+ scale_fill_brewer(palette="Spectral")+ theme(axis.title = element_text(size=18),axis.text = element_text(size=18))
    } else {
      ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "Invalid combination of variable types")) + 
        theme_void()
    }
  })  
  # Textual analysis -----------------
  output$text_var_select <- renderUI({
    req(dataset())
    char_vars <- names(dataset())[sapply(dataset(), is.character)]
    selectInput("text_var", "Select Text Variable:", choices = char_vars)
  })
  
  output$text_cleaning_options <- renderUI({
    req(dataset())
    tagList(
      checkboxInput("remove_punct", "Remove Punctuation", value = TRUE),
      checkboxInput("remove_numbers", "Remove Numbers", value = TRUE),
      checkboxInput("remove_symbols", "Remove Symbols", value = TRUE),
      checkboxInput("to_lower", "Convert to Lowercase", value = TRUE),
      checkboxInput("remove_stopwords", "Remove Stopwords", value = TRUE),
      conditionalPanel(
        condition = "input.remove_stopwords == true",
        selectInput("stopwords_lang", "Stopwords Language:",
                    choices = c("english", "french","french-bastin", "german", "spanish", "italian"),
                    selected = "english")
      ),
      checkboxInput("remove_manualpwords", "Remove words manually from corpus", value = F),
      conditionalPanel(
        condition = "input.remove_manualpwords == true",
        textInput("words_to_rm", "Words to remove (separated by commas):")
      ),
      checkboxInput("detailedlangcleaning","Detailed language cleaning (takes more time to run!)",value=F),
      conditionalPanel(
        condition="input.detailedlangcleaning==true",
        selectInput("langcleaning",HTML("<a href='https://universaldependencies.org/'>Language</a> of text to analyse"),
                    choices=c("afrikaans-afribooms", "ancient_greek-perseus", "ancient_greek-proiel",
                              "arabic-padt", "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb",
                              "buryat-bdt", "catalan-ancora", "chinese-gsd", "chinese-gsdsimp",
                              "classical_chinese-kyoto", "coptic-scriptorium", "croatian-set", "czech-cac",
                              "czech-cltt", "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino",
                              "dutch-lassysmall", "english-ewt", "english-gum", "english-lines", "english-partut",
                              "estonian-edt", "estonian-ewt", "finnish-ftb",      "finnish-tdt", "french-gsd",
                              "french-partut", "french-sequoia", "french-spoken", "galician-ctg",
                              "galician-treegal", "german-gsd", "german-hdt", "gothic-proiel", "greek-gdt",
                              "hebrew-htb", "hindi-hdtb", "hungarian-szeged", "indonesian-gsd", "irish-idt",
                              "italian-isdt", "italian-partut", "italian-postwita", "italian-twittiro",
                              "italian-vit", "japanese-gsd", "kazakh-ktb", "korean-gsd", "korean-kaist",
                              "kurmanji-mg", "latin-ittb", "latin-perseus", "latin-proiel", "latvian-lvtb",
                              "lithuanian-alksnis",      "lithuanian-hse", "maltese-mudt", "marathi-ufal",
                              "north_sami-giella", "norwegian-bokmaal", "norwegian-nynorsk",
                              "norwegian-nynorsklia", "old_church_slavonic-proiel", "old_french-srcmf",
                              "old_russian-torot", "persian-seraji", "polish-lfg", "polish-pdb", "polish-sz",
                              "portuguese-bosque", "portuguese-br", "portuguese-gsd", "romanian-nonstandard",
                              "romanian-rrt", "russian-gsd", "russian-syntagrus", "russian-taiga", "sanskrit-ufal",
                              "scottish_gaelic-arcosg", "serbian-set", "slovak-snk", "slovenian-ssj",     
                              "slovenian-sst", "spanish-ancora", "spanish-gsd", "swedish-lines",
                              "swedish-talbanken", "tamil-ttb", "telugu-mtg", "turkish-imst", "ukrainian-iu",
                              "upper_sorbian-ufal", "urdu-udtb", "uyghur-udt", "vietnamese-vtb", "wolof-wtb"),selected="english-ewt"),
        checkboxInput("select_wordforms",HTML("Select <a href='https://universaldependencies.org/u/pos/index.html'>word forms</a> to analyze"),value=F),
        conditionalPanel(
          condition="input.select_wordforms==true",
          selectInput("forms_keep","Choose from available options",
                      c("ADJ",
                        "ADP",
                        "ADV",
                        "AUX",
                        "CCONJ",
                        "DET",
                        "INTJ",
                        "NOUN",
                        "NUM",
                        "PART",
                        "PRON",
                        "PROPN",
                        "PUNCT",
                        "SCONJ",
                        "SYM",
                        "VERB",
                        "X"),multiple=TRUE)
        ),
        checkboxInput("lemmatize","Lemmatize corpus",value=F)
      ),
      
      sliderInput("minwordsize", "Minimum number of character for a word", min = 1, max = 10, value = 3),
      sliderInput("minwordsmention", "Minimum number of occurrences of words in whole corpus", min = 1, max = 100, value = 10),
      sliderInput("minwordsegment", "Number of words per segment (~ paragraphs, if possible determined by dots and commas)", min = 10, max = 500, value = 40)
      
    )
  })
  
  # observeEvent(input$clean_text, {
  #   req(input$langcleaning)
  #   udpipe_download_model(language = input$langcleaning)
  #   udmodel<-udpipe_load_model(file = list.files(pattern='english'))
  # })
  
  
  ##We also need corpus without transformation
  corpnotransf<-  eventReactive(input$clean_text,{
    text_var <- dataset()[[input$text_var]]
    corp <- corpus(text_var)
    corp
  })
  
  corpnotransfsplit<-  eventReactive(input$clean_text,{
    req(corpnotransf())
    corpsplit <- split_segments(corpnotransf(), segment_size = input$minwordsegment)
    corpsplit
  })
  
  ##Here, corpus with transformation... 
  
  corp <- eventReactive(input$clean_text, {
    withProgress(message = 'Processing text...', value = 0, {
      text_var <- dataset()[[input$text_var]]
      
      if(input$detailedlangcleaning) {
        incProgress(0.1, detail = "Downloading language model...")
        udmodel <- udpipe_download_model(language = input$langcleaning)
        
        if(input$select_wordforms & !input$lemmatize) {
          req(input$forms_keep)
          forms <- input$forms_keep
          getforms <- function(x) {
            ttg <- udpipe(x, object = udmodel)
            ttg <- ttg[ttg$upos %in% forms,]
            return(paste(ttg$token, collapse=" "))
          }
          incProgress(0.3, detail = "Processing word forms...")
          text_var <- unlist(lapply(seq_along(text_var), function(i) {
            incProgress(0.5 / length(text_var), detail = sprintf("Processing text %d of %d", i, length(text_var)))
            getforms(text_var[i])
          }))
        } else if(input$select_wordforms & input$lemmatize) {
          req(input$forms_keep)
          forms <- input$forms_keep
          getforms <- function(x) {
            ttg <- udpipe(x, object = udmodel)
            ttg <- ttg[ttg$upos %in% forms,]
            return(paste(ttg$lemma, collapse=" "))
          }
          incProgress(0.3, detail = "Processing word forms and lemmatizing...")
          text_var <- unlist(lapply(seq_along(text_var), function(i) {
            incProgress(0.5 / length(text_var), detail = sprintf("Processing text %d of %d", i, length(text_var)))
            getforms(text_var[i])
          }))
        } else if(!input$select_wordforms & input$lemmatize) {
          getforms <- function(x) {
            ttg <- udpipe(x, object = udmodel)
            return(paste(ttg$lemma, collapse=" "))
          }
          incProgress(0.3, detail = "Lemmatizing...")
          text_var <- unlist(lapply(seq_along(text_var), function(i) {
            incProgress(0.5 / length(text_var), detail = sprintf("Processing text %d of %d", i, length(text_var)))
            getforms(text_var[i])
          }))
        }
      }
      
      incProgress(0.9, detail = "Creating corpus...")
      # Transform to corpus
      corp <- corpus(text_var)
      
      incProgress(1, detail = "Done!")
      corp
    })
  })
  
  dtm<- eventReactive(input$clean_text, {
    req(corp())
    
    
    # Tokenize the text
    tokenc <- tokens(corp())
    
    
    # Apply selected cleaning options
    if(input$remove_punct) tokenc <- tokens(tokenc, remove_punct = TRUE) %>% as.tokens()
    if(input$remove_numbers) {
      tokenc <- tokens(tokenc, remove_numbers = TRUE) %>% as.tokens()
      typesnum <- grep("[[:digit:]]", types(tokenc), value = TRUE)
      tokenc <- tokens_remove(tokenc, typesnum)
    }
    if(input$remove_symbols) tokenc <- tokens(tokenc, remove_symbols = TRUE) %>% as.tokens()
    if(input$to_lower) tokenc <- tokens_tolower(tokenc)
    if(input$remove_stopwords) {
      if(input$stopwords_lang %in% c("english", "french", "german", "spanish", "italian")){
        tokenc <- tokens_remove(tokenc, stopwords(input$stopwords_lang))
      }else if(input$stopwords_lang %in% c("french-bastin")){
        tokenc <- tokens_remove(tokenc, french_stopwords$token)
      }
    }
    if(input$remove_manualpwords & input$words_to_rm!="") {
      man_torm<-trimws(unlist(strsplit(input$words_to_rm,",")))
      tokenc <- tokens_remove(tokenc, man_torm)
    }
    
    
    tokenc<-tokens_select(tokenc,min_nchar=input$minwordsize)
    dtm <- dfm(tokenc, tolower = T)
    dtm <- dfm_trim(dtm, min_docfreq = input$minwordsmention)
    dtm
  })
  
  
  corpsplit<-  eventReactive(input$clean_text,{
    req(corp())
    corpsplit <- split_segments(corp(), segment_size = input$minwordsegment)
    corpsplit
  })
  
  
  
  dtmsplit<- eventReactive(input$clean_text, {
    req(corpsplit())
    
    
    
    
    # Tokenize the text
    tokenc <- tokens(corpsplit())
    
    # Apply selected cleaning options
    if(input$remove_punct) tokenc <- tokens(tokenc, remove_punct = TRUE) %>% as.tokens()
    if(input$remove_numbers) {
      tokenc <- tokens(tokenc, remove_numbers = TRUE) %>% as.tokens()
      typesnum <- grep("[[:digit:]]", types(tokenc), value = TRUE)
      tokenc <- tokens_remove(tokenc, typesnum)
    }
    if(input$remove_symbols) tokenc <- tokens(tokenc, remove_symbols = TRUE) %>% as.tokens()
    if(input$to_lower) tokenc <- tokens_tolower(tokenc)
    if(input$remove_stopwords) {
      tokenc <- tokens_remove(tokenc, stopwords(input$stopwords_lang))
    }
    if(input$remove_manualpwords & input$words_to_rm!="") {
      man_torm<-trimws(unlist(strsplit(input$words_to_rm,",")))
      tokenc <- tokens_remove(tokenc, man_torm)
    }
    
    tokenc<-tokens_select(tokenc,min_nchar=input$minwordsize)
    dtm <- dfm(tokenc, tolower = T)
    dtm <- dfm_trim(dtm, min_docfreq = input$minwordsmention)
    dtm
    #showNotification("Text cleaned successfully and transformed to a document-feature matrix.", type = "message")
  })
  
  output$corpus_summary <- renderText({
    req(corp(),corpsplit())
    corpsum<-quanteda.textstats::textstat_summary(corp())
    corpsumnonempty<-corpsum %>% filter(tokens>0)
    nseg<-nrow(docvars(corpsplit()))
    #head(docvars(corpsplit()))
    paste("Number of non-empty documents:", nrow(corpsumnonempty),
          "\nNumber of segments:", nseg,
          "\nAverage number of segments per non-empty document:", nseg/nrow(corpsumnonempty),
          "\nAverage number of words (tokens) per non-empty document:", round(mean(corpsumnonempty$tokens,na.rm=T),0))
  })
  
  
  output$freq_terms_table <- renderDT({
    req(dtm())
    freq <- textstat_frequency(dtm()) %>% select(-group)
    datatable(freq,caption="Frequency is the count of the feature (word) in the whole corpus.\n
             Rank is the rank of the frequency (1 is the greatest frequency).\n
             Docfreq is the document frequency (the number of documents in which this feature occurred at least once).")
  })
  
  
  output$wordcloud <- renderPlot({
    req(dtm())
    textplot_wordcloud(dtm(), random_order = F, rotation = 0.25,min_size =1,max_words = input$maxwords,
                       color = RColorBrewer::brewer.pal(8, "Dark2"))
    
  },width=500,height=500)
  
  # madfm <- dfm_group(dtm, groups=sample_data$Sex)
  
  # Stratified freq server -------------------
  
  observe({
    req(dataset())
    print("dataset() output:")
    print(dataset())
    
    # Ensure dataset has columns and handle the case when it's empty
    if (ncol(dataset()) > 0) {
      updateSelectInput(session, "strat_var_forfreq", 
                        choices = names(dataset())[sapply(dataset(), function(x) is.factor(x) | is.numeric(x))])
    } else {
      updateSelectInput(session, "strat_var_forfreq", choices = NULL)
    }
  })
  
  
  observe({
    req(dtm())
    print("dtm() output:")
    print(dtm())
    
    word_choices <- sort(colnames(dtm()))
    updateSelectInput(session, "word_choice_forfreq", 
                      choices = word_choices)
  })
  
  
  stratified_frequency <- eventReactive(input$compute_strat_freq, {
    req(dataset(), dtm(), input$strat_var_forfreq, input$word_choice_forfreq)
    
    # Get the stratification variable
    strat_var <- dataset()[[input$strat_var_forfreq]]
    
    # If numeric, categorize into quartiles
    if(is.numeric(strat_var)) {
      strat_var <- cut(strat_var, breaks = quantile(strat_var, probs = 0:4/4, na.rm = TRUE), 
                       include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))
    }
    
    dtm_df <- convert(dtm(),to="data.frame")
    dtm_df$strat_var <- strat_var
    
    # Compute stratified frequency
    result <- dtm_df %>%
      group_by(strat_var) %>%
      summarise(
        occurrences = sum(!!sym(input$word_choice_forfreq), na.rm = TRUE),
        total_words = sum(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))),
        documents_with_word = sum(!!sym(input$word_choice_forfreq) > 0, na.rm = TRUE),
        total_documents = n()
      ) %>%
      mutate(
        proportion_occurrences = occurrences / total_words * 100,
        proportion_documents = documents_with_word / total_documents * 100
      ) %>% select(strat_var,occurrences,proportion_occurrences,documents_with_word,proportion_documents)
    
    return(result)
  })
  
  output$strat_freq_table <- renderDT({
    req(stratified_frequency())
    datatable(stratified_frequency(), 
              caption = paste("Stratified occurrence for word:", input$word_choice_forfreq,"by :",input$strat_var_forfreq)) %>%
      formatRound(columns = c("proportion_occurrences", "proportion_documents"), digits = 1)
    
  })
  
  # Geometric Data Analysis server -----------------
  output$supplementary_vars <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]
    checkboxGroupInput("supp_vars", "Select Supplementary Variables", choices = factor_vars, inline = TRUE)
  })
  
  output$id_vars <-renderUI({
    req(dataset())
    df<-dataset()
    df_factors <- df[sapply(df, function(x) is.factor(x) && length(levels(x)) == nrow(df))]
    choices <- if(length(df_factors) > 0) {
      c("No ID variable", names(df_factors))
    } else {
      c("No ID variable")
    }
    selectInput("id_varplot", "Select ID Variable:", choices = choices,selected="No ID variable")
  })
  
  output$whichtexts_agd <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]
    tagList(
      radioButtons("where_agd","Which section of the corpus?",choices=c("The entire corpus","Filtered corpus")),
      conditionalPanel(
        condition="input.where_agd == 'Filtered corpus'",
        selectInput("agd_varfilter", "Select factor variable to filter corpus:",
                    choices = factor_vars,
                    selected = factor_vars[1])
      )
    )
    
    
  })
  
  output$levels_toagd <- renderUI({
    req(dataset(),input$where_agd == "Filtered corpus",input$agd_varfilter)
    levels <- levels(dataset()[[input$agd_varfilter]])
    
    tagList(
      conditionalPanel(
        condition="input.where_agd == 'Filtered corpus'",
        selectInput("agd_levelfilter", "Select level(s) of variable:",
                    choices = levels,
                    multiple=T,
                    selected = levels[1])
      )
      
    )
    
  })
  
  analysis_gda <- eventReactive(input$run_gda, {
    req(dtm())
    req(dataset())
    dtm_matrix <- as.matrix(dtm())
    current_data <- dataset()
    
    if(input$where_agd=="Filtered corpus"){
      # Get indices of filtered rows
      filtered_indices <- which(current_data[[input$agd_varfilter]] %in% input$agd_levelfilter)
      print("Selected rows:")
      print(filtered_indices)
      current_data <- current_data[filtered_indices,]
      dtm_matrix <- dtm_matrix[filtered_indices,]
      
      
    }
    
    quali_sup <- NULL
    if (length(input$supp_vars) > 0) {
      # Get the original data columns for supplementary variables
      supp_data <- current_data[, input$supp_vars, drop = FALSE]
      
      # Bind supplementary variables to the dtm_matrix
      combined_matrix <- cbind(dtm_matrix, supp_data)
      
      # Calculate column positions of supplementary variables in the combined matrix
      quali_sup <- (ncol(dtm_matrix) + 1):ncol(combined_matrix)
      
    } else {
      # If no supplementary variables, use dtm_matrix as is
      combined_matrix <- dtm_matrix
      
    }
    #
    if(input$id_varplot!="No ID variable"){
      rownames(combined_matrix)<-dataset()[[input$id_varplot]]
    }
    
    #Deleting rows with only 0s in count
    combined_matrix<-combined_matrix[rowSums(combined_matrix[,1:ncol(dtm_matrix)])>0,]
    #Using logarithms of counts for PCA? Spearman ? or row prop ? 
    combined_matrixp<-combined_matrix
    #combined_matrixp[,1:ncol(dtm_matrix)]<-combined_matrixp[,1:ncol(dtm_matrix)]/rowSums(combined_matrixp[,1:ncol(dtm_matrix)])
    #combined_matrixp[,1:ncol(dtm_matrix)]<-log(combined_matrixp[,1:ncol(dtm_matrix)]+1)
    combined_matrixp <- combined_matrixp %>% as.data.frame() %>%
      mutate(across(where(is.numeric), dense_rank, .names = "{.col}"))
    
    if (input$gda_method == "Principal Component Analysis") {
      pca_res <- PCA(combined_matrixp, 
                     scale.unit = TRUE, 
                     graph = FALSE,
                     quali.sup = quali_sup)
      return(list(type = "Principal Component Analysis", 
                  result = pca_res))
      
    } else {
      ca_res <- CA(combined_matrix, 
                   graph = FALSE,
                   quali.sup = quali_sup)
      return(list(type = "Correspondence Analysis", 
                  result = ca_res))
      
    }
  })
  
  output$plot_gda_act <- renderPlot({
    req(analysis_gda())
    res <- analysis_gda()
    
    if (res$type == "Principal Component Analysis") {
      fviz_pca_var(res$result,col.var="contrib",select.var=list(contrib=input$gda_maxwords),
                   axes=c(as.numeric(input$axish),as.numeric(input$axisv)),
                   title="PCA based on Spearman's rank-order correlation matrix: active variables",
                   labelsize = 5, repel = TRUE,
                   ggtheme = theme_minimal())
      
    } else {
      fviz_ca_col(res$result,label=c("col"),select.col=list(contrib=input$gda_maxwords),
                  col.col="contrib",
                  title="CA: words (columns of the contingency table)",
                  axes=c(as.numeric(input$axish),as.numeric(input$axisv)),
                  labelsize = 5, repel = TRUE,
                  ggtheme = theme_minimal())
    }
  },width=800,height=800)
  
  output$plot_gda_ind <- renderPlot({
    req(analysis_gda())
    res <- analysis_gda()
    req(dataset())
    df<-dataset()
    
    if (res$type == "Principal Component Analysis") {
      # fviz_pca_ind(res$result,select.ind=list(contrib=input$gda_maxtexts),
      #              axes=c(as.numeric(input$axish),as.numeric(input$axisv)),
      #              labelsize = 5, 
      #              ggtheme = theme_minimal())
      
      ind.contrib <- as.data.frame(res$result$ind$contrib)
      
      ind.contrib$sumcontrib<-ind.contrib[,as.numeric(input$axish)]+ind.contrib[,as.numeric(input$axisv)]
      ind.contrib <- ind.contrib[order(-ind.contrib$sumcontrib),]
      ind.contrib<-head(ind.contrib,input$gda_maxtexts)
      
      ind.coord <- as.data.frame(res$result$ind$coord)
      ind.coord$name <- rownames(ind.coord)
      
      ind.coord <- ind.coord[ind.coord$name %in% rownames(ind.contrib),]
      
      ggscatter(ind.coord, x = paste0("Dim.",input$axish), y = paste0("Dim.",input$axisv), label = "name",
                title="PCA based on Spearman's rank-order correlation matrix: individual texts",
                font.label=c(12,"plain"),
                ggtheme = theme_minimal()
      )+
        geom_vline(xintercept = 0, linetype = "dashed")+
        geom_hline(yintercept = 0, linetype = "dashed")
      
      
    } else {
      # fviz_ca_row(res$result,label=c("row"),select.row=list(contrib=input$gda_maxtexts),
      #             axes=c(as.numeric(input$axish),as.numeric(input$axisv)),
      #             labelsize = 5, 
      #             ggtheme = theme_minimal())
      ind.contrib <- as.data.frame(res$result$row$contrib)
      ind.contrib$sumcontrib<-ind.contrib[,as.numeric(input$axish)]+ind.contrib[,as.numeric(input$axisv)]
      ind.contrib <- ind.contrib[order(-ind.contrib$sumcontrib),]
      ind.contrib<-head(ind.contrib,input$gda_maxtexts)
      
      ind.coord <- as.data.frame(res$result$row$coord)
      ind.coord$name <- rownames(ind.coord)
      
      ind.coord <- ind.coord[ind.coord$name %in% rownames(ind.contrib),]
      
      ggscatter(ind.coord, x = paste0("Dim ",input$axish), y = paste0("Dim ",input$axisv), label = "name",
                title="CA: individual texts (rows of the contingency table)",
                font.label=c(12,"plain"),
                ggtheme = theme_minimal()
      )+
        geom_vline(xintercept = 0, linetype = "dashed")+
        geom_hline(yintercept = 0, linetype = "dashed")
      
    }
  },width=800,height=800)
  
  output$plot_gda_sup <- renderPlot({
    req(analysis_gda())
    res <- analysis_gda()
    
    if (res$type == "Principal Component Analysis") {
      
      quali.supcos2 <- as.data.frame(res$result$quali.sup$cos2)
      
      quali.supcos2$sumcos2<-quali.supcos2[,as.numeric(input$axish)]+quali.supcos2[,as.numeric(input$axisv)]
      quali.supcos2 <- quali.supcos2[order(-quali.supcos2$sumcos2),]
      quali.supcos2<-head(quali.supcos2,input$gda_maxcharact)
      
      quali.supcoord <- as.data.frame(res$result$quali.sup$coord)
      quali.supcoord$name <- rownames(quali.supcoord)
      
      quali.supcoord <- quali.supcoord[quali.supcoord$name %in% rownames(quali.supcos2),]
      
      ggscatter(quali.supcoord, x = paste0("Dim.",input$axish), y = paste0("Dim.",input$axisv), label = "name",
                title="PCA based on Spearman's rank-order correlation matrix: supplementary categorical variables",
                font.label=c(14,"plain"),repel=T,
                ggtheme = theme_minimal()
      )+
        geom_vline(xintercept = 0, linetype = "dashed")+
        geom_hline(yintercept = 0, linetype = "dashed")
      
      
    } else {
      quali.supcos2 <- as.data.frame(res$result$quali.sup$cos2)
      
      quali.supcos2$sumcos2<-quali.supcos2[,as.numeric(input$axish)]+quali.supcos2[,as.numeric(input$axisv)]
      quali.supcos2 <- quali.supcos2[order(-quali.supcos2$sumcos2),]
      quali.supcos2<-head(quali.supcos2,input$gda_maxcharact)
      
      quali.supcoord <- as.data.frame(res$result$quali.sup$coord)
      quali.supcoord$name <- rownames(quali.supcoord)
      
      quali.supcoord <- quali.supcoord[quali.supcoord$name %in% rownames(quali.supcos2),]
      
      ggscatter(quali.supcoord, x = paste0("Dim ",input$axish), y = paste0("Dim ",input$axisv), label = "name",
                title="CA: supplementary categorical variables",
                font.label=c(14,"plain"),repel=T,
                ggtheme = theme_minimal()
      )+
        geom_vline(xintercept = 0, linetype = "dashed")+
        geom_hline(yintercept = 0, linetype = "dashed")
      
    }
  },width=800,height=800)
  
  output$plot_gda_eig <- renderPlot({
    req(analysis_gda())
    res <- analysis_gda()
    
    fviz_eig(res$result, addlabels = F,ncp=as.numeric(input$gda_maxaxes))
    
  })
  
  
  
  
  
  
  
  
  # Context server -----------------
  output$word_to_search <- renderUI({
    req(dataset())
    textInput("word_input", "Search textual context of a word:")
  })
  
  observeEvent(input$search_context, {
    req(corpnotransf(), input$word_input)
    
    output$table_context <- renderDT({
      cont <- kwic(tokens(corpnotransf()), pattern = input$word_input, window = 5)
      contsum<-cont #%>% select(docname,pre,keyword,post)
      # Convert the kwic object to a data frame
      # cont_df <- data.frame(
      #   pre = sapply(cont$pre, paste, collapse = " "),
      #   keyword = cont$keyword,
      #   post = sapply(cont$post, paste, collapse = " "),
      #   stringsAsFactors = FALSE
      # )
      
      # Return the data frame
      contsum
    })
  })
  
  
  
  
  # Dual wordcloud -----------------
  output$wordcloud_var_select <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]
    selectInput("wordcloud_var", "Select factor variable for word cloud:",
                choices = factor_vars,
                selected = factor_vars[1])
  })
  
  output$factor_level_select <- renderUI({
    req(input$wordcloud_var)
    levels <- levels(dataset()[[input$wordcloud_var]])
    tagList(
      selectInput("level1", "Select first level:", choices = levels),
      selectInput("level2", "Select second level:", choices = levels)
    )
  })
  
  
  
  observeEvent(input$confirm, {
    req(input$wordcloud_var, input$level1, input$level2)
    
    selected_var <- input$wordcloud_var_select
    
    #levels <- unique(dataset()[[selected_var]])
    
    if (input$level1 != input$level2) {
      showNotification(paste("You have selected levels:", input$level1, "and", input$level2), type = "message")
    } else {
      showNotification("Please select two different levels.", type = "error")
    }
  })
  
  
  output$dualwordcloud_plot <- renderPlot({
    req(input$confirm, input$wordcloud_var, input$level1, input$level2)
    dtm<-dtm()
    data<-dataset()
    selected_var <- input$wordcloud_var
    level1<-input$level1
    level2<-input$level2
    
    
    dtm <- dfm_group(dtm, groups=data[[selected_var]])
    dtm <- dfm_subset(dtm, docnames(dtm) %in%  c(level1,level2))
    
    textplot_wordcloud(dtm, comparison = TRUE,max_words = input$maxdualwords,color = c("darkgreen", "darkorange"))
  })
  
  
  
  output$dualfreq_terms_tableTop <- renderDT({
    
    req(input$confirm, input$wordcloud_var, input$level1, input$level2)
    dtm<-dtm()
    data<-dataset()
    selected_var <- input$wordcloud_var
    level1<-input$level1
    level2<-input$level2
    dtm <- dfm_group(dtm, groups=data[[selected_var]])
    dtm <- dfm_subset(dtm, docnames(dtm) %in%  c(level1,level2))
    tab<-head(textstat_keyness(dtm), input$maxkeyness)
    datatable(tab,caption=paste0("Most characteristics features of ",input$level1)) %>% formatRound(c(2,3),2)
    
  })
  output$dualfreq_terms_tableBttm <- renderDT({
    req(input$confirm, input$wordcloud_var, input$level1, input$level2)
    dtm<-dtm()
    data<-dataset()
    selected_var <- input$wordcloud_var
    level1<-input$level1
    level2<-input$level2
    dtm <- dfm_group(dtm, groups=data[[selected_var]])
    dtm <- dfm_subset(dtm, docnames(dtm) %in%  c(level1,level2))
    tab<-tail(textstat_keyness(dtm), input$maxkeyness) %>% arrange(chi2) 
    datatable(tab,caption=paste0("Most characteristics features of ",input$level2)) %>% formatRound(c(2,3),2)
  })
  
  
  # Cooccurrences server side -----------------
  
  
  output$what_to_coocc <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]
    
    
    tagList(
      radioButtons("what_coocc","Type of co-occurrences",choices=c("Co-occurrences between 50 most frequent features","Co-occurrences of a word")),
      conditionalPanel(
        condition="input.what_coocc == 'Co-occurrences of a word'",
        textInput("word_coocc","Write word:")
      ),
      radioButtons("where_coocc","Which section of the corpus?",choices=c("The entire corpus","Filtered corpus")),
      conditionalPanel(
        condition="input.where_coocc == 'Filtered corpus'",
        selectInput("coocc_varfilter", "Select factor variable to filter corpus:",
                    choices = factor_vars,
                    selected = factor_vars[1])
      )
    )
    
  })
  
  output$level_toocc <- renderUI({
    req(dataset(),input$where_coocc == "Filtered corpus",input$coocc_varfilter)
    levels <- levels(dataset()[[input$coocc_varfilter]])
    
    tagList(
      conditionalPanel(
        condition="input.where_coocc == 'Filtered corpus'",
        selectInput("coocc_levelfilter", "Select level of variable:",
                    choices = levels,
                    selected = levels[1])
      )
      
    )
    
  })
  
  observeEvent(input$display_coocc, {
    req(input$what_coocc, input$where_coocc)
    
    if (input$what_coocc=="Co-occurrences between 50 most frequent features" & input$where_coocc=="The entire corpus") {
      showNotification("Most frequent co-occurrences within segments of words on the entire corpus",type = "message")
    } else if(input$what_coocc=="Co-occurrences between 50 most frequent features" & input$where_coocc=="Filtered corpus"){
      showNotification(paste0("Most frequent co-occurrences within segments of words on the corpus of ",input$coocc_levelfilter),type = "message")
    } else if(input$what_coocc=="Co-occurrences of a word" & input$word_coocc!="" & input$where_coocc=="Filtered corpus"){
      showNotification(paste0("Co-occurrences of ",input$word_coocc," within segments of words on the corpus of ",input$coocc_levelfilter),type = "message")
    } else if(input$what_coocc=="Co-occurrences of a word" & input$where_coocc=="The entire corpus"){
      showNotification(paste0("Co-occurrences of ",input$word_coocc," within segments of words on the entire corpus"),type = "message")
    } else{
      showNotification("Select options!",type = "error")
      
    }
  })
  
  
  output$plot_coocc <- renderPlot({
    req(input$display_coocc, input$what_coocc, input$where_coocc,dataset(), dtmsplit())
    dtmsplit<-dtmsplit()
    data <- dataset()
    
    
    if (input$what_coocc=="Co-occurrences between 50 most frequent features" & input$where_coocc=="The entire corpus") {
      
      featsplit <- names(topfeatures(dtmsplit, 50))
      
      fcm<- fcm(dtmsplit, context="document")
      fcm_selected <- fcm_select(fcm, pattern = featsplit, selection = "keep")
      textplot_network(fcm_selected)
      
      
    } else if(input$what_coocc=="Co-occurrences between 50 most frequent features" & input$where_coocc=="Filtered corpus"){
      req(input$coocc_varfilter, input$coocc_levelfilter)
      
      nsegments<-data.frame(table(sub("\\_.*", "", dtmsplit@docvars$docname_)))
      
      #nsegments<-docvars(dtmsplit) %>% count(segment_source)
      namevarfilt<-input$coocc_varfilter
      namelevelfilt<-input$coocc_levelfilter
      vartofilt<-rep(data[[namevarfilt]],times=nsegments$Freq)
      dtmsplit <- dfm_group(dtmsplit, groups=vartofilt)
      dtmsplit <- dfm_subset(dtmsplit, docnames(dtmsplit) %in%  c(namelevelfilt))
      featsplit <- names(topfeatures(dtmsplit, 50))
      
      fcm<- fcm(dtmsplit, context="document")
      fcm_selected <- fcm_select(fcm, pattern = featsplit, selection = "keep")
      textplot_network(fcm_selected)
      
      
    } else if(input$what_coocc=="Co-occurrences of a word" & input$word_coocc!="" & input$where_coocc=="Filtered corpus"){
      req(input$word_coocc)
      feat<-input$word_coocc
      nsegments<-data.frame(table(sub("\\_.*", "", dtmsplit@docvars$docname_)))
      
      #nsegments<-docvars(dtmsplit) %>% count(segment_source)
      namevarfilt<-input$coocc_varfilter
      namelevelfilt<-input$coocc_levelfilter
      vartofilt<-rep(data[[namevarfilt]],times=nsegments$Freq)
      dtmsplit <- dfm_group(dtmsplit, groups=vartofilt)
      dtmsplit <- dfm_subset(dtmsplit, docnames(dtmsplit) %in%  c(namelevelfilt))
      
      fcm<- fcm(dtmsplit, context="document")
      mfcm<-as.matrix(fcm)
      
      tryCatch({
        cooccurf<-data.frame(n=mfcm[feat,]) %>% arrange(-n) %>% top_n(50)%>% filter(n>0) %>% rownames_to_column(var = "feature")
        
        p<-ggplot(data=cooccurf, aes(x=reorder(feature, n), y=n)) +
          geom_bar(stat="identity")+
          geom_text(aes(label=n), vjust=0.5,hjust=1.2 ,color="white", size=4.5)+
          coord_flip()+
          labs(y="Number of co-occurrences\nin segments",x=paste0("Most frequent words\nco-occurring with ",feat))+
          theme_minimal()+
          theme(axis.text=element_text(size=15),axis.title = element_text(size=15))
        p
      },error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
      
      
      
    } else if(input$what_coocc=="Co-occurrences of a word" & input$where_coocc=="The entire corpus"){
      
      req(input$word_coocc)
      feat<-input$word_coocc
      
      
      fcm<- fcm(dtmsplit, context="document")
      mfcm<-as.matrix(fcm)
      
      tryCatch({
        cooccurf<-data.frame(n=mfcm[feat,]) %>% arrange(-n) %>% top_n(50)%>% filter(n>0) %>% rownames_to_column(var = "feature")
        
        p<-ggplot(data=cooccurf, aes(x=reorder(feature, n), y=n)) +
          geom_bar(stat="identity")+
          geom_text(aes(label=n), vjust=0.5,hjust=1.2 ,color="white", size=4.5)+
          coord_flip()+
          labs(y="Number of co-occurrences\nin segments",x=paste0("Most frequent words\nco-occurring with ",feat))+
          theme_minimal()+
          theme(axis.text=element_text(size=15),axis.title = element_text(size=15))
        p
      },error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
      
      
    } 
    
  })
  
  
  
  
  # Reinert's algorithm -----------------
  
  
  
  clust<-  eventReactive(input$classify, {
    if(input$whatclass=="Documents"){
      dtm<-dtm()
      result<-rainette(dtm, k = input$maxk)
    }
    else if(input$whatclass=="Segments") {
      dtm<-dtmsplit()
      if(input$howclass=="Simple Descendant Hierarchical Classification"){
        result <- rainette(dtm, k = input$maxk,min_segment_size=input$minseg_simple)
      }
      else if(input$howclass=="Dual Descendant Hierarchical Classification"){
        result1 <- rainette(dtm, k = input$maxk,min_segment_size=input$minseg_dual1)
        #result1 <- rainette(dtm, k = 4,min_segment_size=10)
        
        result2 <- rainette(dtm, k = input$maxk,min_segment_size=input$minseg_dual2)
        #result2 <- rainette(dtm, k = 4,min_segment_size=15)
        
        result <- rainette2(result1, result2, max_k = input$maxk,full = TRUE,parallel = TRUE)
        #result <- rainette2(result1, result2, max_k = 4,full = TRUE,parallel = TRUE)
        
      }
    }
    
    result
  })
  
  
  output$dendrogram <- renderPlot({
    req(clust())
    if(input$whatclass=="Documents"){
      rainette_plot(clust(), dtm(), k = input$maxk, type = "bar", n_terms = input$maxfeatshow, free_scales = FALSE,
                    measure = "chi2", show_negative = "TRUE", text_size = 11)
    }
    else if(input$whatclass=="Segments"){
      if(input$howclass=="Simple Descendant Hierarchical Classification"){
        rainette_plot(clust(), dtmsplit(), k = input$maxk, type = "bar", n_terms = input$maxfeatshow, free_scales = FALSE,
                      measure = "chi2", show_negative = "TRUE", text_size = 11)
      }else if(input$howclass=="Dual Descendant Hierarchical Classification"){
        rainette2_plot(clust(), dtmsplit(), k = input$maxk, type = "bar", n_terms = input$maxfeatshow, free_scales = FALSE,
                       measure = "chi2", show_negative = "TRUE", text_size = 11)
        
      }
    }
    
  })
  
  output$class_selector <- renderUI({
    req(clust())
    classes <- 1:input$maxk
    selectInput("selected_class", "Select Class:", choices = classes)
  })
  
  
  # observe({
  #   table_data <- isolate({
  #     req(clust(), input$selected_class)
  #     class_assignments <- cutree_rainette(clust(), k = input$maxk)
  #     docs <- which(class_assignments == as.integer(input$selected_class))
  #     texts <- data.frame(
  #       doc_id = docs,
  #       content = as.character(corp()[docs])
  #     )
  #     texts
  #   })
  #   print("Table data:")
  #   print(table_data)
  # })
  
  output$document_table <- renderTable({
    req(clust(), input$selected_class)
    
    tryCatch({
      
      if(input$howclass=="Simple Descendant Hierarchical Classification"){
        class_assignments <- cutree_rainette(clust(), k = input$maxk)
      }else if(input$howclass=="Dual Descendant Hierarchical Classification"){
        class_assignments <- cutree_rainette2(clust(), k = input$maxk)
        
      }
      
      docs <- which(class_assignments == as.integer(input$selected_class))
      
      if(input$whatclass == "Documents") {
        texts <- data.frame(
          Document_id = docs,
          Content = as.character(corpnotransf()[docs])
        )
      } else if(input$whatclass == "Segments") {
        segment_ids <- docs
        segment_sources <- docvars(corpnotransfsplit())$segment_source[docs]
        contents <- as.character(corpnotransfsplit()[docs])
        
        texts <- data.frame(
          Document_id = sub('text','',segment_sources),
          Segment_id = segment_ids,
          
          Content = contents
        )
      }
      
      search_word <- input$search_word
      
      if(nchar(search_word) > 0 && !is.null(texts$content)) {
        texts <- texts[grep(search_word, texts$content, ignore.case = TRUE), ]
      }
      
      if(nrow(texts) > 0) {
        texts
      } else {
        data.frame(message = "No documents/segments found matching the criteria")
      }
    }, error = function(e) {
      data.frame(error = paste("An error occurred:", e$message))
    })
  }, sanitize.text.function = function(x) x)
  
  # Add cluster variable(s) --------------------
  
  # Add classification result as new variable
  observeEvent(input$add_var, {
    req(clust(), dataset())
    data <- dataset()
    new_var_name <- input$new_var_name_class
    corpsplit <- corpsplit()
    
    print(paste("whatclass:", input$whatclass))
    print(paste("new_var_name:", new_var_name))
    print(paste("maxk:", input$maxk))
    
    if (is.null(input$whatclass) || input$whatclass == "") {
      showNotification("Please select a valid class type.", type = "error")
      return()
    }
    
    if (input$whatclass == "Documents") {
      if (new_var_name=="") {
        showNotification(paste("Write a variable name to add to dataset!"), type = "message")
      }else if(new_var_name %in% names(data)) {
        showNotification(paste("Variable", new_var_name, "already exists! Pick another name."), type = "message")
      } else {
        clusters <- cutree(clust(), k = input$maxk)
        data[[new_var_name]]<-factor(clusters)
        showNotification("Cluster variable added to dataset (check 'Data Management' tab).", type = "message")
        dataset(data)
        
      }
    } else if (input$whatclass == "Segments") {
      if (is.null(input$maxk) || input$maxk <= 0) {
        showNotification("Please enter a valid number of clusters.", type = "error")
        return()
      }
      
      new_var_names <- paste(new_var_name, seq(1, input$maxk, 1), sep = "_")
      new_var_names <- c(new_var_names, paste(new_var_name, "NA", sep = "_"))
      
      if (is.null(new_var_name) || new_var_name == "") {
        showNotification("Write a variable name prefix to add to dataset!", type = "message")
      } else if (new_var_name[1] %in% names(data)) {
        showNotification(paste("Variable", new_var_name, "already exists! Pick another name."), type = "message")
      } else {
        tryCatch({
          print("Debug: Before cutree")
          print(str(clust()))
          corpsplit$groupe <- cutree(clust(), k = input$maxk)
          print("Debug: After cutree")
          print(str(corpsplit$groupe))
          
          print("Debug: Before clusters_by_doc_table")
          distrib <- clusters_by_doc_table(corpsplit, clust_var = "groupe", prop = TRUE)
          print("Debug: After clusters_by_doc_table")
          print(str(distrib))
          
          lastcol <- input$maxk + 2
          colnames(distrib)[2:lastcol] <- new_var_names
          
          for (i in 2:lastcol) { 
            print(paste("Debug: Processing column", i))
            print(str(distrib[,i]))
            data[[new_var_names[i-1]]] <- as.numeric(distrib[[i]])
          }
          showNotification("Cluster variables added to dataset (check 'Data Management' tab).", type = "message")
          dataset(data)
        }, error = function(e) {
          print(paste("Error:", e$message))
          print("Error details:")
          print(str(e))
          showNotification(paste("Error adding cluster variables:", e$message), type = "error")
        })
      }
    } else {
      showNotification("Invalid class type selected.", type = "error")
    }
  })  
  # AFC on results of classification ------------------
  
  output$correspondance_selector <- renderUI({
    req(clust())
    
    maxaxes<-input$maxk-1
    tagList(
      
      selectInput("axis_horizontal", "Select horizontal axis:", choices = c(1:maxaxes)),
      selectInput("axis_vertical", "Select vertical axis:", choices = c(1:maxaxes),selected=2),
      numericInput("maxfeatafc", "Max number of terms to display by cluster:", 10, min = 1, max = 100)
    )
  })
  
  
  output$AFC <- renderPlot({
    req(clust(), dtm(), input$maxfeatafc, input$axis_horizontal, input$axis_vertical)
    
    clustmemb <- cutree(clust(), k = input$maxk)
    
    if(input$whatclass=="Documents"){
      
      keyterms <- rainette_stats(clustmemb, dtm(), n_terms = input$maxfeatafc, show_negative = FALSE)
      keytermsdat <- do.call(rbind, mapply(transform, keyterms, ID = seq_along(keyterms), SIMPLIFY = FALSE))
      dtmclust <- dfm_group(dtm(), groups = clustmemb)
    }
    else {
      keyterms <- rainette_stats(clustmemb, dtmsplit(), n_terms = input$maxfeatafc, show_negative = FALSE)
      keytermsdat <- do.call(rbind, mapply(transform, keyterms, ID = seq_along(keyterms), SIMPLIFY = FALSE))
      dtmclust <- dfm_group(dtmsplit(), groups = clustmemb)
      
    }
    dtmdat <- cbind(convert(dtmclust, to = "data.frame"), docvars(dtmclust)) %>% column_to_rownames("doc_id")
    
    dtmdatsel <- dtmdat %>% select(all_of(keytermsdat$feature))
    
    ca <- CA(dtmdatsel, graph = FALSE)
    
    coord <- data.frame(ca$col$coord) %>% rownames_to_column("feature")
    coord <- coord %>% left_join(keytermsdat, by = "feature")
    
    Dimh <- paste0("Dim.", input$axis_horizontal)
    Dimv <- paste0("Dim.", input$axis_vertical)
    
    eigh_row <- paste0("dim ", input$axis_horizontal)
    eigv_row <- paste0("dim ", input$axis_vertical)
    
    eigh <- sprintf("%0.1f", ca$eig[eigh_row, "percentage of variance"])
    eigv <- sprintf("%0.1f", ca$eig[eigv_row, "percentage of variance"])
    
    # Ensure ID is a factor
    coord$ID <- as.factor(coord$ID)
    
    ggplot(data = coord, aes(x = !!sym(Dimh), y = !!sym(Dimv))) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point(color="white", size = 1) +  # Add points with color aesthetic
      geom_text_repel(
        aes(label = feature, color = ID, 
            size = pmax(chi2, 10)),  # Set a minimum size threshold
        max.overlaps = Inf,
        show.legend = c(size = FALSE)  # Remove size legend
      ) +
      scale_size_continuous(range = c(4, 10)) +  # Adjust the size range as needed
      scale_color_manual(values = paletteer_d("ggthemes::Tableau_10")) +
      labs(x = paste("Dimension", input$axis_horizontal, "(", eigh, "%)"),
           y = paste("Dimension", input$axis_vertical, "(", eigv, "%)"),
           color = "Cluster") +
      theme_minimal() +
      theme(legend.position = "bottom",axis.title = element_text(size=18),axis.text = element_text(size=15),
            legend.title=element_text(size=18),legend.text = element_text(size=18)) +
      guides(colour = guide_legend(override.aes = list(size=8)))
  }, width = 800, height = 800)  
  
}

