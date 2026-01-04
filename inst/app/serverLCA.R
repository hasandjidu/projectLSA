# ==== Reactive Data LCA ====
server_lca <- function(input, output, session) {
  library(poLCA)
  library(tidyverse)
  library(ggiraph)
  best_c_r <- reactiveVal(NULL)
  
observeEvent(input$run_lca, {
  req(data_lca(), input$vars_lca)
  updateTabsetPanel(session, "main_tab_lca", selected = "fit_tab_lca")
})
data_lca <- reactive({
  if (input$data_source_lca == "simdata3class") {
    df <- poLCA::poLCA.simdata(N=2000, nclass=3,ndv=3)$dat %>% rownames_to_column("id_auto")
    
  } else if (input$data_source_lca == "simdata4class") {
    df <- poLCA::poLCA.simdata(N=3000, nclass=4,ndv=4)$dat%>% rownames_to_column("id_auto")
    
  } else if(input$data_source_lca == "simdata5class") {
    df <- poLCA::poLCA.simdata(N=4000, nclass=5,ndv=5)$dat%>% rownames_to_column("id_auto")
    
  } else if (input$data_source_lca == "simdata6class") {
    df <- poLCA::poLCA.simdata(N=5000, nclass=6,ndv=6)$dat%>% rownames_to_column("id_auto")
    
  } else if (input$data_source_lca == "simdata7class") {
    df <- poLCA::poLCA.simdata(N=6000, nclass=7,ndv=7)$dat%>% rownames_to_column("id_auto")
    
  } else {
    req(input$datafile_lca)
    ext <- tools::file_ext(input$datafile_lca$name)
    df <- if(ext == "csv") read.csv(input$datafile_lca$datapath) else readxl::read_excel(input$datafile_lca$datapath)
    df <- df %>% mutate(across(everything(), ~ifelse(.x=="", NA, .x)),
                        id_auto = paste0("id_", sprintf("%04d", 1:n())))
  }
  return(na.omit(df))
})

# ==== Pilih ID ====
output$id_select_ui_lca <- renderUI({
  req(data_lca())
  selectInput(
    "id_lca",
    label = "Select ID Columns (Optional):",
    choices = names(data_lca()),
    selected = names(data_lca())[str_detect(names(data_lca()), "id")],
    multiple = TRUE
    )
})

# ==== Pilih variabel ====
output$var_select_ui_lca <- renderUI({
  req(data_lca())
  selectInput(
    "vars_lca",
    label = "Select Variables for LCA:",
    choices = names(data_lca()),
    selected = names(data_lca()%>% dplyr::select(-c(id_auto)))[1:min(5,ncol(data_lca()))],
    multiple = TRUE  )
})

observeEvent(c(input$data_source_lca, input$datafile_lca), {
  updateSelectInput(session,"vars_lca",selected = "") 
}, ignoreInit = TRUE)

# ==== Preview Data ====
output$data_preview_lca <- DT::renderDT({
  req(data_lca(), input$vars_lca)
  df <- data_lca() %>% dplyr::select(input$vars_lca)
  numeric_cols <- which(sapply(df, function(x) is.numeric(x)))
  
  DT::datatable(df,extensions = 'Buttons',
                options = list(scrollX = TRUE, dom = 'Brtp',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('Data LCA')  
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('Data LCA')
                                 ))),
                rownames = TRUE) %>% 
    formatRound(columns = numeric_cols, digits = 0)
}, server = FALSE)

# ==== Fit LCA ====
lca_models <- eventReactive(input$run_lca, {
  req(data_lca(), input$vars_lca)
  dat <- data_lca()[, input$vars_lca]
  dat <- dat %>% mutate(across(everything(), as.factor))
  
  formula <- as.formula(paste("cbind(", paste(input$vars_lca, collapse=","), ") ~ 1"))
  
  min_k <- input$min_class_lca
  max_k <- input$max_class_lca
  
  model_list <- list()
  withProgress(message = "Running...", {
    for(k in min_k:max_k){
      incProgress(1/(max_k-min_k+1), detail = paste("LCA with", k, "Classes"))
      model_list[[as.character(k)]] <- tryCatch({
        poLCA::poLCA(formula, dat, nclass=k, verbose=FALSE)
      }, error = function(e) NULL)
    }
  })
  model_list
})

# FUNCTION TO CALCULATION OF LCA -----
APCP_poLCA <- function(fit) {
  post <- fit$posterior
  cls  <- fit$predclass
  K <- ncol(post)
  APCP_class <- sapply(1:K, function(k) {
    mean(post[cls == k, k])
  })
  
  APCP_overall <- mean(post[cbind(1:nrow(post), cls)])
  APCP_byclass <- mean(APCP_class)
  list(
    APCP_per_class = APCP_class,
    APCP_overall  = APCP_overall,
    APCP_byClass = APCP_byclass
  )
}
entropy_poLCA <- function(fit) {
  post <- fit$posterior
  N <- nrow(post)
  K <- ncol(post)
  E <- - sum(post * log(post))
  
  1 - E / (N * log(K))
  entropy <- 1 - E / (N * log(K))
  
  return(entropy)
}
# ==== Fit Table ====
fit_lca <- reactive({
  req(lca_models())
  set.seed(123)
  fit <- data.frame(N_class=integer(), AIC=numeric(), BIC=numeric(), Gsq=numeric(), Chisq=numeric(), resid.df=numeric(), 
                    Entropy =numeric(), APCP_byClass=numeric(), APCP_overall=numeric())
  for(k in names(lca_models())){
    m <- lca_models()[[k]]
    if(!is.null(m)){
      fit <- rbind(fit, data.frame(
        N_class = k,
        AIC = round(m$aic,3),
        BIC = round(m$bic,3),
        Gsq = round(m$Gsq,3),
        Chisq = round(m$Chisq,3),
        resid.df = m$resid.df,
        Entropy = round(entropy_poLCA(m),3) ,
        APCP =round(APCP_poLCA(m)$APCP_byClass,3)
        #APCP_overall =round(APCP_poLCA(m)$APCP_overall,3) 
      ))
    }
  }
  fit
})

# ==== Fit Table ====
output$fit_table_lca <- renderDT({
    df <- fit_lca()
    #max_APCP_overall  <- max(df$APCP_overall, na.rm = TRUE)
    aic_vals <- sort(unique(df$AIC))
    bic_vals <- sort(unique(df$BIC))
    ent_vals <- sort(unique(df$Entropy), decreasing = TRUE)
    apcp_vals  <- sort(unique(df$APCP), decreasing = TRUE)
  
    DT::datatable(df,extensions = 'Buttons',
                  options = list(scrollX = TRUE, dom = 'B',
                                 buttons = list(
                                   list(
                                     extend = 'csv',
                                     text = 'Export CSV',
                                     filename = paste0('Fit Comparison LCA')  
                                   ),
                                   list(
                                     extend = 'excel',
                                     text = 'Export Excel',
                                     filename = paste0('Fit Comparison LCA')
                                   ))), 
                  rownames = FALSE) %>% 
      formatStyle('AIC',
                  backgroundColor = styleEqual(
                    c(aic_vals[1], aic_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(aic_vals[1], 'bold')
      ) %>%
      formatStyle('BIC',
                  backgroundColor = styleEqual(
                    c(bic_vals[1], bic_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(bic_vals[1], 'bold')
      ) %>%
      formatStyle('Entropy',
                  backgroundColor = styleEqual(
                    c(ent_vals[1], ent_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(ent_vals[1], 'bold')
      ) %>%
      formatStyle('APCP',
                  backgroundColor = styleEqual(
                    c(apcp_vals[1], apcp_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(ent_vals[1], 'bold')
      )
}, server = FALSE)

# ==== Fit Plot (AIC/BIC) ====
fit_plot_lca_reactive <- reactive({
  req(lca_models())
  fit <- data.frame(N_class=integer(), AIC=numeric(), BIC=numeric())
  for(k in names(lca_models())){
    m <- lca_models()[[k]]
    if(!is.null(m)){
      fit <- rbind(fit, data.frame(
        N_class = as.numeric(k),
        AIC = m$aic,
        BIC = m$bic
      ))
    }
  }
  fit_long <- fit %>% pivot_longer(-N_class, names_to="Index", values_to="Value")
  ggplot(fit_long, aes(x=N_class, y=Value, color=Index, group=Index)) +
    geom_line(size = 1.2) + geom_point(size = 3) +
    geom_text(aes(label = round(Value, 2)), vjust = -0.6, size = 3) +
    labs(title = "Model Comparison (AIC & BIC)", x = "Number of Class", y = "Fit Index") +
    theme_minimal(base_size = 14) + 
    theme(legend.position = "bottom", axis.line = element_line(color = "black"))
  
  
})
# Render Plot
output$fit_plot_lca <- renderPlot({
  fit_plot_lca_reactive()
})

# ==== Download Buttons ====
output$download_plot_AicBic_LCA <- make_download_plot(
  plot_reactive = fit_plot_lca_reactive,
  filename_prefix = "Fit AIC BIC Plot_LCA"
)
# ==== Smallest Class Size Plot ====
smallest_class_plot_lca_reactive <- reactive({ 
  req(lca_models())
  # Buat data frame untuk setiap model
  smallest_df <- purrr::map_df(names(lca_models()),
                               function(k) {
                                 model_k <- lca_models()[[k]]
                                 membership <- model_k$predclass
                                 class_table <- as.data.frame(base::table(membership))
                                 names(class_table) <- c("Class", "N")
                                 class_table$Percent <- round(100 * class_table$N/sum(class_table$N), 1)
                                 # Ambil kelas terkecil
                                 min_class <- class_table %>% slice_min(order_by = N, n = 1)
                                 min_class$model <- paste0(k, "-class")
                                 min_class
                               })
  
  # Plot bar horizontal
  ggplot(smallest_df, aes(x = model, y = Percent, fill = model)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(Percent, "%[n=", N, "]")), hjust = 0, size = 4) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(smallest_df$Percent)+18)) +
    labs(x = "Model", y = "Percentage") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none", axis.line = element_line(color = "black"))
})
# Render Plot
output$smallest_class_plot_lca <- renderPlot({
  smallest_class_plot_lca_reactive()
})

# ==== Download Buttons ====
output$download_plot_classSize_LCA <- make_download_plot(
  plot_reactive = smallest_class_plot_lca_reactive,
  filename_prefix = "BestPlot_LPA"
)

observeEvent(input$best_class_lca, {
  req(input$best_class_lca)
  best_c_r(as.numeric(input$best_class_lca))
}, ignoreInit = TRUE)

# ==== Input nama class ====
output$class_name_inputs_lca <- renderUI({
  req(input$best_class_lca)
  lapply(1:input$best_class_lca, function(i) {
    textInput(paste0("class_name_", i), paste("Class", i, "Name:"), value = paste("Class", i))
  })
})

# === 1. Fungsi pembuat plot ===
build_best_model_plot_lca <- function(lca_models, k, vars_lca, class_names, interactive = TRUE) {
  model_k <- lca_models[[as.character(k)]]
  probs <- model_k$probs
  
  prob <- data.frame(matrix("", ncol = 2 + as.numeric(k), nrow = 0))
  names(prob) <- c("Var", "Level", paste0("Prob.class", 1:as.numeric(k)))
  
  for (j in seq_along(vars_lca)) {
    a <- data.frame(
      Var = vars_lca[j],
      Level = colnames(probs[[j]]),
      t(probs[[j]])
    )
    a <- setNames(a, names(prob))
    prob <- rbind(prob, a)
  }
  
  prob_gathered <- prob %>%
    tidyr::gather(key = "class", value = "probability", 3:ncol(prob)) %>%
    dplyr::mutate(
      class = gsub("Prob.", "", class),
      probability = round(as.numeric(probability), 3)
    ) %>%
    dplyr::arrange(Var)
  
  prob_gathered$class <- factor(
    prob_gathered$class,
    levels = paste0("class", 1:as.numeric(k)),
    labels = class_names
  )
  
  if (interactive) {
    gg <- ggplot(prob_gathered,
                 aes(
                   x = Var, y = probability, fill = Level,
                   tooltip = paste0(
                     "Class: ", class,
                     "<br>Variable: ", Var,
                     "<br>Level: ", Level,
                     "<br>Probability: ", probability
                   ),
                   data_id = class
                 )) +
      geom_bar_interactive(stat = "identity", position = "stack")
  } else {
    gg <- ggplot(prob_gathered,
                 aes(x = Var, y = probability, fill = Level)) +
      geom_bar(stat = "identity", position = "stack")
  }
  
  gg +
    facet_grid(~ class) +
    ylab("Probability") + xlab("") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(face = "bold", size = 12)
    )
}

# === 2. Reactive untuk plot interaktif ===
best_model_plot_lca_reactive <- reactive({
  req(lca_models(), input$best_class_lca, input$vars_lca)
  k <- input$best_class_lca
  class_names <- sapply(1:k, function(i) input[[paste0("class_name_", i)]])
  if (length(class_names) == 0) class_names <- paste("Class", 1:k)
  
  build_best_model_plot_lca(lca_models(), k, input$vars_lca, class_names, interactive = TRUE)
})

# === 3. Render plot interaktif ===
output$best_model_plot_lca <- renderGirafe({
  k <- input$best_class_lca
  filename_plot <- paste0("LCA_best_model_", k, "class")
  
  ggiraph::girafe(
    ggobj = best_model_plot_lca_reactive(),
    width_svg = 9, height_svg = 5,
    options = list(
      opts_hover(css = "fill-opacity:1;"),
      opts_tooltip(css = "background-color:white;color:black;
                    border:1px solid #ccc;padding:6px;
                    font-size:11px;border-radius:5px;"),
      opts_toolbar(saveaspng = TRUE, pngname = filename_plot)
    )
  )
})

# === 4. Reactive versi statis untuk download ===
best_model_plot_lca_static <- reactive({
  req(lca_models(), input$best_class_lca, input$vars_lca)
  k <- input$best_class_lca
  class_names <- sapply(1:k, function(i) input[[paste0("class_name_", i)]])
  if (length(class_names) == 0) class_names <- paste("Class", 1:k)
  
  build_best_model_plot_lca(lca_models(), k, input$vars_lca, class_names, interactive = FALSE)
})


# === 3. Download handler panggil plot statis ===
output$download_plot_best_LCA <- make_download_plot(
  plot_reactive = best_model_plot_lca_static,
  filename_prefix = paste0("LCA_best_model_", input$best_class_lca, "class")
)

# ==== Summary table ====
summary_data_lca <- reactive({
  req(lca_models(), input$best_class_lca, input$vars_lca)
  
  k <- as.numeric(input$best_class_lca)
  model_k <- lca_models()[[as.character(k)]]
  req(model_k)
  
  # Ambil nama class dari input (fallback jika belum diisi)
  class_names <- sapply(1:k, function(i) {
    nm <- input[[paste0("class_name_", i)]]
    if (is.null(nm) || nm == "") paste0("Class ", i) else nm
  })
  
  # === 1. Ukuran kelas ===
  class_size <- data.frame(
    base::table(model_k$predclass)) %>%
    dplyr::rename(Class = Var1, N = Freq) %>%
    dplyr::mutate(Percent=round(100*N/sum(N),2))
  class_size$Class <- class_names[class_size$Class]
  
  # === 2. Probabilitas kategori (adaptasi dari plot Girafe) ===
  probs <- model_k$probs
  listvar <- input$vars_lca
  
  prob <- data.frame(matrix("", ncol = 2 + k, nrow = 0))
  names(prob) <- c("Variable", "Category", paste0("Prob.", class_names))
  
  for (j in seq_along(listvar)) {
    a <- data.frame(
      Variable = listvar[j],
      Category = colnames(probs[[j]]),
      t(probs[[j]])
    )
    names(a) <- names(prob)
    prob <- rbind(prob, a)
  }
  
  prob <- prob %>%
    mutate(across(starts_with("Prob."), as.numeric)) %>%
    arrange(Variable)
  
  # Gabungkan class size + probabilitas (tidak merge, tapi list agar bisa dua tabel)
  list(
    class_size = class_size,
    probability = prob
  )
})


# ==== Render table ====
output$summary_table_lca <- renderUI({
  req(summary_data_lca())
  dat <- summary_data_lca()
  
  tagList(
    tags$h5("Class Size"),
    DTOutput("class_size_table_lca"),
    tags$h5("Item-Category Probabilities"),
    DTOutput("probability_table_lca")
  )
})

output$class_size_table_lca <- renderDT({
  df <- summary_data_lca()$class_size
  DT::datatable(df,extensions = 'Buttons',
                options = list(scrollX = TRUE, dom = 'B',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('Class Size')
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('Class Size')
                                 ))),
                rownames = FALSE)
})

output$probability_table_lca <- renderDT({
  df <- summary_data_lca()$probability
  numeric_cols <- which(sapply(df, function(x) is.numeric(x)))
  DT::datatable(df,extensions = 'Buttons',
                options = list(scrollX = TRUE, pageLength=25,
                               dom = 'Brtp',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('Class Probability')
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('Class Probability')
                                 ))),
                rownames = FALSE) %>%
    formatRound(columns = numeric_cols, digits = 2)
})

# ==== Summary Table & Profile per ID ====
output$profile_table_lca <- renderDT({
  req(lca_models(), input$best_class_lca)
  k <- as.numeric(input$best_class_lca)
  # Ambil nama class dari input (fallback jika belum diisi)
  class_names <- sapply(1:k, function(i) {
    nm <- input[[paste0("class_name_", i)]]
    if (is.null(nm) || nm == "") paste0("Class ", i) else nm
  })
  
  model_k <- lca_models()[[as.character(input$best_class_lca)]]
  req(model_k)
  
  df <- data_lca()
  id_cols <- if(is.null(input$id_lca) || length(input$id_lca)==0) "id_auto" else input$id_lca
  df$Class <- model_k$predclass
  df$Class <- class_names[df$Class]
  
  df <- df[, c(id_cols, input$vars_lca, "Class")]
  
  numeric_cols <- which(sapply(df, is.numeric))
  DT::datatable(df,extensions = 'Buttons',
                options = list(scrollX = TRUE, pageLength=25,
                               dom = 'Brtp',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('LCA Result with ', input$best_class_lca, ' Classes')
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('LCA Result with', input$best_class_lca, ' Classes')
                                 ))),
                rownames = TRUE) %>%
    formatRound(columns = numeric_cols, digits = 2)
  
}, server = FALSE)


}