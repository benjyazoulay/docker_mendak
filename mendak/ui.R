
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

#french_stopwords<-read.csv2("~/zPublish/shiny/mendak/french_stopwords.csv")
french_stopwords<-read.csv2("/Users/mathieuferry/Documents/Recherche/Shiny apps/Basic textual analysis/french_stopwords.csv")

options(shiny.maxRequestSize=100*1024^2)

# UI
ui <- fluidPage(
  
  tags$script("
  Shiny.addCustomMessageHandler('datasetUpdated', function(message) {
    Shiny.onInputChange('datasetTrigger', Math.random());
  });
"),
  
  
  navbarPage("Mendak",
             theme = bslib::bs_theme(bootswatch = "sandstone"),
             
             # Welcome ------------------
             tabPanel("Welcome",
                      
                      HTML("<title>Welcome to Mendak</title>
</head>
<body>
    <h1>Welcome to Mendak</h1>
    <p>This shiny app is made for (simple) data and textual statistical analysis. The app is divided into three functional tabs of interest:</p>
    <ul>
        <li>
            <strong>Data Management tab:</strong> Upload and format your dataset (you can upload it in different formats but it has to be a row x column dataset, where one of the columns contains your text to be analysed). If you do not have a ready-made dataset, use the sample dataset.
        </li>
        <li>
            <strong>Descriptive Statistics tab:</strong> Run univariate and bivariate statistics on the quantitative and qualitative variables of the dataset.
        </li>
        <li>
            <strong>Textual Analysis tab:</strong> Clean and analyse the text corpus contained in one of the columns of the dataset. Different analyses can be conducted and in case a cluster analysis is conducted, new variable(s) can be added to the dataset to be analysed in the Descriptive Statistics tab.
        </li>
    </ul>
    <p>This shiny app has been written to facilitate statistical analyses for non-R users (and non-coders). 
    Short tutorials in <a href='https://mathieuferry.github.io/Tutorials-for-Mendak/TutorialEN.html'>English</a> or in <a href='https://mathieuferry.github.io/Tutorials-for-Mendak/TutorielFR.html'>French</a> presenting the functionalities of the app are available.
    Some of the tools available here follow the statistical outline presented in this <a href='https://mathieuferry.github.io/PondicherryWorkshop/'>tutorial</a> to conduct these analyses in the R console.</p>
    
    <center><img src='https://cdn.pixabay.com/photo/2020/06/20/01/24/frog-5319326_1280.jpg' width='700' 
     height='500' alt='A nice and funny picture of a frog' ></center>

                           </body>")
             ),
             navbarMenu("Data Management",
                        tabPanel("Upload and Download",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file1", "Choose File", accept = c(".csv", ".xls", ".xlsx", ".rdata", ".rds")),
                                     tags$br(),  # Ajoute un espace
                                     actionButton("load_sample_eng", "Indian matrimonial ads (English)"),
                                     tags$br(),  # Ajoute un espace
                                     tags$br(),  # Ajoute un espace
                                     actionButton("load_sample_fr", "Court rulings on moral harassment (French)"),
                                     tags$br(),  # Ajoute un espace
                                     tags$br(),  # Ajoute un espace
                                     actionButton("load_sample_fr2", "Newspaper articles titled with 'wokisme' (French)"),
                                     tags$br(),  
                                     tags$br(),  
                                     selectInput("export_format", "Export Format:", choices = c("csv", "xls", "xlsx", "rdata", "rds")),
                                     downloadButton("downloadData", "Download Data")
                                   ),
                                   mainPanel(
                                     verbatimTextOutput("data_summary"),
                                     uiOutput("variable_management")
                                   )
                                 )
                        ),
                        tabPanel("View",
                                 DTOutput("data_view")
                        )
                        
             ),
             
             
             
             # UI Descriptive stat analysis ---------------------
             navbarMenu("Descriptive statistics",
                        
                        tabPanel("Univariate",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("var_select_uni"),
                                     uiOutput("var_calc_uni")
                                     
                                   ),
                                   mainPanel(
                                     DTOutput("univariate_table"),
                                     plotOutput("univariate_plot")
                                   )
                                 )
                        ),
                        tabPanel("Bivariate",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("var_select_bi"),
                                     uiOutput("var_select_bi_exp"),
                                     uiOutput("display_option")
                                     
                                   ),
                                   mainPanel(
                                     DTOutput("bivariate_table"),
                                     plotOutput("bivariate_plot")
                                   )
                                 )
                        )
             ),
             
             #UI textual -------------------
             navbarMenu("Textual Analysis",
                        tabPanel("Text Cleaning",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("text_var_select"),
                                     uiOutput("text_cleaning_options"),
                                     uiOutput("minwordsize"),
                                     uiOutput("minwordsmention"),
                                     uiOutput("minwordsegment"),
                                     actionButton("clean_text", "Clean Text")
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Summary statistics",
                                                verbatimTextOutput("corpus_summary")
                                       ),
                                       ##Here: add number of features by document
                                       ##Number of segments in each document
                                       tabPanel("Frequency of cleaned features",
                                                DTOutput("freq_terms_table")
                                       ),
                                       tabPanel("Word cloud",
                                                sliderInput("maxwords", "Maximum number of words to plot", min = 50, max = 1000, value = 100),
                                                plotOutput("wordcloud")
                                       )
                                     )
                                   )
                                 )
                        ),
                        
                        #UI dual word cloud -------------------
                        
                        tabPanel("Dual word cloud",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("wordcloud_var_select"),
                                     uiOutput("factor_level_select"),
                                     actionButton("confirm", "Confirm Selection")
                                     
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Plot",
                                                sliderInput("maxdualwords", "Maximum number of words to plot", min = 20, max = 1000, value = 100),
                                                plotOutput("dualwordcloud_plot",width = "100%",
                                                           height = "800px")
                                       ),
                                       tabPanel("Table",
                                                
                                                p("The most distinctive terms to each category are based on a 'keyness' score, here calculated from the chi-squared value (and associated p-value)."),
                                                p("A high positive chi-squared value is distinctive of features of the 'top' category, whereas a low negative chi-squared value is distinctive of the 'bottom' category (from the word cloud)"),
                                                sliderInput("maxkeyness", "Maximum number of characteristic words to show", min = 1, max = 50, value = 20),
                                                
                                                DTOutput("dualfreq_terms_tableTop"),
                                                DTOutput("dualfreq_terms_tableBttm"),
                                       )
                                     )
                                   )
                                 ) ),
                        #UI Stratified Frequency -------------------
                        
                        tabPanel("Stratified Occurrences",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("strat_var_forfreq", "Stratification Variable", choices = NULL),
                                     selectInput("word_choice_forfreq", "Word Choice", choices = NULL),
                                     actionButton("compute_strat_freq", "Compute Stratified Frequency")
                                   ),
                                   mainPanel(
                                     DTOutput("strat_freq_table")
                                   )
                                 )
                        ),
                        
                        #UI Geometric Data Analysis -------------------
                        
                        tabPanel("Geometric Data Analysis",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("gda_method", "Geometric Data Analysis tool", choices = c("Principal Component Analysis","Correspondence Analysis")),
                                     uiOutput("supplementary_vars"),
                                     radioButtons("axish","Horizontal axis",choices=c(1:5),selected=1,inline = TRUE),
                                     radioButtons("axisv","Vertical axis",choices=c(1:5),selected=2,inline = TRUE),
                                     uiOutput("id_vars"),
                                     uiOutput("whichtexts_agd"),
                                     uiOutput("levels_toagd"),
                                     actionButton("run_gda", "Vizualise")                                     
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Words in factorial plane",
                                                plotOutput("plot_gda_act",inline=TRUE),
                                                br(),
                                                sliderInput("gda_maxwords", "Maximum number of words to plot (by contribution)", min = 1, max = 100, value = 10)
                                                
                                       ),
                                       tabPanel("Texts in factorial cloud",
                                                plotOutput("plot_gda_ind",inline=TRUE),
                                                br(),
                                                sliderInput("gda_maxtexts", "Maximum number of texts to plot (by contribution)", min = 1, max = 1000, value = 10)
                                                
                                       ),
                                       tabPanel("Data characteristics in factorial cloud",
                                                plotOutput("plot_gda_sup",inline=TRUE),
                                                br(),
                                                sliderInput("gda_maxcharact", "Maximum number of categories to plot (by cos2)", min = 1, max = 100, value = 10)
                                                
                                       ),
                                       tabPanel("Explained variance of each factorial dimension",
                                                plotOutput("plot_gda_eig",width = "100%"),
                                                br(),
                                                sliderInput("gda_maxaxes", "Maximum number of dimensions to plot", min = 1, max = 100, value = 10)
                                                
                                       )
                                     )
                                     
                                   )
                                 )
                        ),
                        
                        
                        # UI Context -----------
                        tabPanel("Context",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     uiOutput("word_to_search"),
                                     actionButton("search_context", "Confirm Selection")
                                   ),
                                   mainPanel(
                                     DTOutput("table_context")
                                   ) 
                                 )),
                        
                        # UI Cooccurrences -----------
                        
                        tabPanel("Co-occurrences",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     uiOutput("what_to_coocc"),
                                     uiOutput("level_toocc"),
                                     actionButton("display_coocc", "Display")
                                   ),
                                   mainPanel(
                                     plotOutput("plot_coocc",width = "100%",
                                                height = "800px")
                                   ) 
                                 )
                        ),
                        
                        
                        #Text Classification -------------------
                        
                        tabPanel("Classification",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("whatclass","Cluster unit of analysis:",c("Documents","Segments"),selected="Documents"),
                                     conditionalPanel(condition = "input.whatclass == 'Segments'",
                                                      radioButtons("howclass","Reinert's algorithm:",c("Simple Descendant Hierarchical Classification","Dual Descendant Hierarchical Classification"),selected="Simple Descendant Hierarchical Classification")
                                                      ,
                                                      conditionalPanel(
                                                        condition = "input.howclass == 'Simple Descendant Hierarchical Classification'",
                                                        sliderInput("minseg_simple", "Minimal number of cleaned features per segment", min = 0, max = 200, value = 10)
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.howclass == 'Dual Descendant Hierarchical Classification'",
                                                        sliderInput("minseg_dual1", "Minimal number of cleaned features per segment for first clustering", min = 0, max = 200, value = 10),
                                                        sliderInput("minseg_dual2", "Minimal number of cleaned features per segment for second clustering", min = 0, max = 200, value = 15)
                                                        
                                                      )),
                                     
                                     sliderInput("maxk", "Number of clusters to compute", min = 2, max = 10, value = 6),
                                     actionButton("classify", "Classify"),
                                     
                                     br(),
                                     textInput("new_var_name_class", "New Cluster Variable Name"),
                                     actionButton("add_var", "Add Cluster Variable in the Dataset")
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Classes",
                                                numericInput("maxfeatshow", "Max number of terms to display by cluster:", 10, min = 1, max = 100),
                                                
                                                plotOutput("dendrogram")
                                       ),
                                       tabPanel("Documents/Segments by class",
                                                uiOutput("class_selector"),
                                                textInput("search_word", "Filter documents (type to search):", ""),
                                                
                                                tableOutput("document_table")
                                                
                                       ),
                                       tabPanel("Correspondance Analysis",
                                                
                                                HTML("A Correspondance Analysis is run on the matrix 'clusters x features', where we keep only the most positively-associated features of each cluster (using the chi2 measure). 
                                                     This analysis is similar to the one that can be obtained from Iramuteq.
                                                     We attribute words to one cluster (and color them accordingly) based on how the highest positive association in a given cluster.
                                                     Correspondance analysis helps vizualise how distinct clusters are from each other, particularly when running Dual Clustering, where we cannot vizualise the dendrogram."),
                                                
                                                uiOutput("correspondance_selector"),
                                                
                                                plotOutput("AFC")
                                                
                                       )
                                     )
                                   )
                                 )
                        )
                        
             ),
             #About -----------------------
             tabPanel("About",
                      h3("About Mendak"),
                      HTML("<p>Mendak is the acronym of 'My Easy-to-use Navigator for Data Analysis and [K]lustering.' 
                      It also means frog in Hindi (मेंढक). It is a playful nod to 'rainette' (literally, treefrog in French),
                      the R package developed by Julien Barnier.  It implements the classification
                      algorithm for textual analysis based on Reinert's method. Max Reinert was a data engineer at <a href='https://www.printemps.uvsq.fr/'>Printemps (UVSQ)</a>, 
                           my current research lab (more info about me <a href='https://mathieuferry.github.io/'>here</a>).</p>
                           
                      <p>Reinert's method is highly effective for classifying text documents and is less statistically 
                        intensive compared to the powerful algorithms of modern AI that now surround
                        us. However, it has been challenging to access. Existing softwares (Alceste, Iramuteq) require installation on 
                        a local computer, unlike some alternative tools that also employ 'Benzécri's cookbook' (the French 
                        approach to data analysis based on geometric data analysis) to textual analysis, such as Hyperbase (where however no solution of clustering is available).</p>
                        
                      <p>The recent release of the 'rainette' R package was a significant development, enabling the implementation
                        of Reinert's classification algorithm in R. This facilitated the beta development of this Shiny app, designed 
                        for non-coders to use it online—a crucial factor for its adoption by more 'qualitative' social scientists. The app
                        is designed to streamline textual data analysis and includes functions for conducting univariate and 
                        bivariate statistical analyses, further enhancing its utility.</p>
                      <p>The app uses different key R packages for textual analysis: <a href='https://quanteda.io/'>quanteda</a>, <a href='https://lindat.mff.cuni.cz/services/udpipe/'>udpipe</a> and <a href='https://juba.github.io/rainette/'>rainette</a>.</p>
                      <p>For any question or remark, please send an email to mathieu.ferry[at]uvsq.fr.</p>")
                      
                      
             )
  )
)