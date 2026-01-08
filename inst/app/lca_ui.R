# Fungsi untuk menu LCA
lca_ui <- function(project) {
  tabsetPanel(
    id = "main_tab_lca",
    
    # --- TAB 1: Prepare Data & Model ----
    tabPanel(
      title = tagList(icon("upload"), "Prepare Data & Model"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          actionButton("go_home", 
                       label = tagList(icon("home"), "Main Menu"), 
                       class = "btn btn-danger btn-block",
                       style = "width: 100% !important;"),
          br(),
          selectInput(
            "data_source_lca", 
            "Select Data Source:",
            choices = c("Upload Data" = "upload",
                        "Simulation Data 1" = "simdata3class",
                        "Simulation Data 2" = "simdata4class",
                        "Simulation Data 3" = "simdata5class",
                        "Simulation Data 4" = "simdata6class",
                        "Simulation Data 5" = "simdata7class"
                        
                        ),
            selected = "upload"
          ),
          conditionalPanel(
            condition = "input.data_source_lca == 'upload'",
            fileInput("datafile_lca", "Upload Data (csv/xlsx)", accept = c(".csv", ".xlsx"))
          ),
          uiOutput("id_select_ui_lca"),
          uiOutput("var_select_ui_lca"),
          
          numericInput("min_class_lca", "Min. Number of Class:", 2, min = 1),
          numericInput("max_class_lca", "Max. Number of Class:", 9, min = 2),
          
          br(),
          
          actionButton("run_lca", 
                       label = tagList(icon("play"), "Run LCA"), 
                       class = "btn btn-success btn-block",
                       style = "width: 100% !important;")
        ),
        mainPanel(
          width = 9,
          h5(icon("table"), "Data Preview"),
          DTOutput("data_preview_lca"),
          DTOutput("data_summary_lca")
          
          
        )
      )
    ),
    
    # --- TAB 2: Fit Model Comparison ----
    tabPanel(
      title = tagList(icon("chart-line"), "Fit Model Comparison"),
      value = "fit_tab_lca",  # <--- tambahkan ini juga
      
      fluidRow(
        column(12,
               div(
                 style = "text-align:center;",  # pusatkan kontainer
                 div(
                   style = "display:inline-block; font-size:11px; line-height:0.9; padding:0; margin:0;",
                   h5(icon("info-circle"), "Model Fit Statistics"), 
                   DTOutput("fit_table_lca")
                 )
               )
               
        ),
        column(5, h5(icon("chart-bar"), "AIC & BIC Comparison"), 
               downloadButton("download_plot_AicBic_LCA", "Download Plot AIC/BIC (.png)"),
               plotOutput("fit_plot_lca")
               ),
        column(7, h5(icon("sliders-h"), "Min Class Size Comparison"), 
               downloadButton("download_plot_classSize_LCA", "Download PLot Smallest Class Size (.png)"),
               plotOutput("smallest_class_plot_lca",width = '100%')
               )
      )
    ),
    
    # --- TAB 3: Best Model ----
    tabPanel(
      title = tagList(icon("star"), "Best Model"),
      sidebarLayout(
        sidebarPanel(
          width = 2, 
          numericInput("best_class_lca",label =  "Select the Best Number of Class:",value = 3, min = 1),
          uiOutput("class_name_inputs_lca")
        ),
        mainPanel(
          width = 10, 
          h5(icon("project-diagram"), "Class Plot of the Best Model"),
          downloadButton("download_plot_best_LCA", "Download Plot Best Model (.png)"),
          ggiraph::girafeOutput("best_model_plot_lca", width = '100%', height = 'auto'),
          h5(icon("table"), "Class Size and Item-Category Probabilities"),
          tableOutput("summary_table_lca"),
         
          br()
        )
      )
    ),
    
    # --- TAB 4: Summary & Report ----
    tabPanel(
      title = tagList(icon("file-alt"), "Summary & Report"),
      tags$b("Class Classification Data"),
      DTOutput("profile_table_lca")
      ),
    
    # --- TAB 5: About ----
    tabPanel(
      title = tagList(icon("info-circle"), "About"),
      fluidRow(
        column(
          width = 8, offset = 2,
          br(),
          div(
            style = "text-align:center;",
            tags$hr(),
            tags$h5("projectLSA Was Developed By:"),
            tags$p(
              tags$a(
                href = "https://scholar.google.com/citations?user=PSAwkTYAAAAJ&hl=id",
                target = "_blank",
                "Dr. Hasan Djidu, M.Pd."),
              tags$br(),
              "Universitas Sembilanbelas November Kolaka"
            ),
            tags$h5("Supervised By:"),
            tags$p(
              tags$a(
                href = "https://scholar.google.com/citations?user=7CzPTYIAAAAJ&hl=id",
                target = "_blank",
                "Prof. Dr. Heri Retnawati, M.Pd."), 
              tags$br(),
              "Universitas Negeri Yogyakarta"
            ),
            
            tags$p(tags$a(
              href = "https://scholar.google.com/citations?hl=id&user=VGKeBm0AAAAJ",
              target = "_blank",
              "Prof. Dr. Samsul Hadi"), 
              tags$br(),
              "Universitas Negeri Yogyakarta"
            ),
            tags$p(tags$a(
              href = "https://scholar.google.com/citations?hl=id&user=k4MA8XgAAAAJ",
              target = "_blank",
              "Dr. Drs. Ir. Haryanto, M.Pd., M.T."), 
              tags$br(),
              "Universitas Negeri Yogyakarta"
            ),
            tags$b("Contact:"),
            tags$a("hasandjidu@gmail.com"),
            tags$hr()
          )
        ),
        column(
          width = 8, offset = 2,
          h4("References (R Packages)"),
          uiOutput("package_references_lca"),
          br(),
          div(
            style = "text-align:center;",
            tags$p(
              style = "font-size:13px; color:#777;",
             format(Sys.Date(), "%Y"), 
              "projectLSA. All rights reserved."
            )
          )
        )
      )
    )
  )
}
