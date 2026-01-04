server_efa <- function(input, output, session) {
  library(psych)
  library(lavaan)
  
  # ==== LOAD DATA ====
  data_user <- reactive({
    if (input$data_source == "bfi") return(psych::bfi %>% dplyr::select(A1:O5)%>% rownames_to_column("id_auto"))
    if (input$data_source == "HolzingerSwineford1939") return(lavaan::HolzingerSwineford1939%>% dplyr::select(x1:x9)%>% rownames_to_column("id_auto"))
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    if (ext == "csv") read.csv(input$datafile$datapath)
    else read_excel(input$datafile$datapath)
  })
  

  # ==== Pilih ID ====
  output$id_select_ui_fa <- renderUI({
    req(data_user())
    selectizeInput(
      "id_lca",
      label = "Select ID Columns (Optional):",
      choices = names(data_user()),
      selected = names(data_user())[str_detect(names(data_user()), "id")],
      multiple = TRUE,
      options = list(placeholder = 'Choose one or more ID columns')
    )
  })
  
  # ==== Pilih Variabel (selain ID) ====
  output$var_select_ui <- renderUI({
    req(data_user())
    all_vars <- names(data_user())
    id_cols <- input$id_lca
    
    # Hilangkan kolom ID dari pilihan variabel
    available_vars <- setdiff(all_vars, id_cols)
    selectInput(
      "selected_vars",
      label = "Select Variables:",
      choices = available_vars,
      selected = available_vars,  # default pilih semua yang tersisa
      multiple = TRUE
    )
  })

  output$data_preview_fa <- renderDT({
    req(data_user(),input$selected_vars)
    df <- data_user()[,input$selected_vars]
    df <- df %>%
      dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
    
    DT::datatable(
      head(df, 50),extensions = 'Buttons',
      options = list(dom='Brtp',scrollX = TRUE, pageLength = 25,  
                     buttons = list(
                       list(extend = 'csv',
                            text = 'Export CSV',
                            filename = 'Data EFA'
                       ),
                       list(extend = 'excel',
                            text = 'Export Excel',
                            filename = 'Data EFA'
                       ))),
      rownames = FALSE
    )
  }, server = FALSE)
  
  
  # === Ketika user memilih CFA ===
  observeEvent(input$fa_type, {
    if (input$fa_type == "cfa") {
      message("Running CFA server...")
      cfa_server(input, output, session, data_user = data_user)
    }
  })
  
  
  
  
  # ==== Dynamic input nama faktor ====
  output$factor_names_ui <- renderUI({
    req(input$n_factors)
    n <- input$n_factors
    div(
      style = "
      display: flex;
      justify-content: center;
      flex-wrap: wrap;
      gap: 8px;
      margin-top: 5px;
    ",
      
      lapply(1:n, function(i) {
        textInput(
          inputId = paste0("factor_name_", i),
          label = NULL,
          value = paste0("Factor ", i),
          width = "100px"
        )
      }),
      br(),
    )
  })
  
  # ==== EFA ====
  efa_result <- eventReactive(c(input$run_efa, input$n_factors), {
    req(data_user(), input$selected_vars)
    
    withProgress(message = "Running Exploratory Factor Analysis (EFA)...", value = 0, {
      incProgress(0.1, detail = "Preparing data...")
      
      df <- as.data.frame(data_user())[ , input$selected_vars, drop = FALSE]
      
      # ensure numeric columns
      nonnum <- which(!sapply(df, is.numeric))
      if (length(nonnum) > 0) {
        df[nonnum] <- lapply(df[nonnum], function(x) as.numeric(as.character(x)))
      }
      
      # handle missing
      if (input$missing_method_efa == "mean") {
        for (j in seq_along(df)) df[is.na(df[, j]), j] <- mean(df[, j], na.rm = TRUE)
        missing_arg <- "no"
      } else if (input$missing_method_efa == "pairwise") {
        missing_arg <- "pairwise"
      } else {
        df <- na.omit(df)
        missing_arg <- "listwise"
      }
      
      incProgress(0.3, detail = "Computing KMO and Bartlett tests...")
      # compute KMO and Bartlett
      kmo_res <- tryCatch(psych::KMO(df), error = function(e) NULL)
      bartlett_res <- tryCatch(
        psych::cortest.bartlett(cor(df, use = "pairwise.complete.obs"), n = nrow(df)),
        error = function(e) NULL
      )
      
      incProgress(0.6, detail = "Performing Parallel Analysis...")
      par <- tryCatch(
        psych::fa.parallel(df, n.iter = 20, main = "Parallel Analysis (fa.parallel)"),
        error = function(e) NULL
      )
      
      incProgress(0.9, detail = paste("Extracting", input$n_factors, "factors..."))
      fa_res <- tryCatch(
        psych::fa(df, nfactors = input$n_factors, rotate = input$rotation_method),
        error = function(e) NULL
      )
      
      incProgress(1, detail = "Finalizing results...")
      
      efa_out <- list(
        kmo = kmo_res,
        bartlett = bartlett_res,
        parallel = par,
        fa = fa_res,
        loadings = if (!is.null(fa_res)) fa_res$loadings else NULL,
        df_used = df,
        scores = as.data.frame(psych::factor.scores(df, fa_res, Phi = NULL,rho=NULL,missing=FALSE,impute="mean")$scores)
      )
      
      efa_out
    })
  })

  
  # ==== Pindah ke tab hasil EFA setelah dijalankan ====
  observeEvent(input$run_efa, {
    updateTabsetPanel(session, "main_tab_fa", selected = "KMO & Bartlett Tests")
  })
  
  
  # ==== Tampil hasil tes EFA dengan gaya HTML ====
  output$efa_tests <- renderUI({
    req(efa_result())
    out <- efa_result()
    n_fact <- if (!is.null(out$parallel$nfact)) out$parallel$nfact else NA
    n_comp <- if (!is.null(out$parallel$ncomp)) out$parallel$ncomp else NA
    bart <- out$bartlett
    kmo <- out$kmo
    
    # ==== Format hasil KMO ====
    if (!is.null(kmo)) {
      MSA_overall <- round(kmo$MSA, 3)
      interpret <- if (MSA_overall >= 0.90) {
        "Excellent sampling adequacy — the data are highly suitable for factor analysis."
      } else if (MSA_overall >= 0.80) {
        "Meritorious sampling adequacy — the data are suitable for factor analysis."
      } else if (MSA_overall >= 0.70) {
        "Middling sampling adequacy — acceptable for factor analysis."
      } else if (MSA_overall >= 0.60) {
        "Mediocre sampling adequacy — factor analysis may still be appropriate."
      } else if (MSA_overall >= 0.50) {
        "Miserable sampling adequacy — consider improving data quality or sample size."
      } else {
        "Unacceptable sampling adequacy — factor analysis is not recommended."
      }
      
      kmo_html <- paste0(
        "<ul style='margin-top:4px;'>",
        "<li><b>Overall MSA:</b> ", MSA_overall, "</li>",
        "</ul>",
        "<p style='margin-left:10px; color:#2c3e50;'>", interpret, "</p>"
      )
    } else {
      kmo_html <- "<p style='color:#999;'>KMO test could not be computed (check data).</p>"
    }
    
    # ==== Format hasil Bartlett ====
    if (!is.null(bart)) {
      pval <- bart$p.value
      signif_text <- if (pval < 0.05) {
        "<span style='color:#2ecc71;font-weight:500;'>Significant (p < 0.05)</span> — the correlation matrix is <b>not an identity matrix</b>, indicating that factor analysis is appropriate."
      } else {
        "<span style='color:#e74c3c;font-weight:500;'>Not significant (p ≥ 0.05)</span> — the correlation matrix is <b>close to an identity matrix</b>, suggesting that factor analysis may <b>not be suitable</b> for this data."
      }
      
      bartlett_html <- paste0(
        "<ul style='margin-top:4px;'>",
        "<li><b>Chi-Square:</b> ", round(bart$chisq, 2), "</li>",
        "<li><b>df:</b> ", bart$df, "</li>",
        "<li><b>p-value:</b> ", format.pval(pval, digits = 3), "</li>",
        "</ul>",
        "<p style='margin-left:10px;'>", signif_text, "</p>"
      )
    } else {
      bartlett_html <- "<p style='color:#999;'>Bartlett test could not be computed (check data).</p>"
    }
    
    # ==== Gabungkan semua hasil ====
    HTML(paste0(
      "<div style='font-family:Segoe UI, sans-serif; line-height:1.6; background:#f9f9f9;
               border-radius:12px; padding:12px 18px; border:1px solid #ddd;'>",
      
      # --- Parallel Analysis ---
      "<h4 style='font-weight:bold; color:#2c3e50; margin-bottom:6px;'>📊 Parallel Analysis Recommendation</h4>",
      "<p style='margin-left:10px;'>
       <a>Suggested Number of Factors =</a> ", n_fact, "<br>
       <a>Suggested Number of Components =</a> ", n_comp, "
     </p>",
      
      "<hr style='border: none; border-top: 1px solid #ccc;'>",
      
      # --- KMO and MSA ---
      "<h4 style='font-weight:bold; color:#2c3e50; margin-bottom:6px;'>🧮 Kaiser-Meyer-Olkin (KMO) Measure</h4>",
      kmo_html,
      
      "<hr style='border: none; border-top: 1px solid #ccc;'>",
      
      # --- Bartlett's Test ---
      "<h4 style='font-weight:bold; color:#2c3e50; margin-bottom:6px;'>🧪 Bartlett's Test of Sphericity</h4>",
      bartlett_html,
      
      "</div>"
    ))
  })
  
  
  # ==== Plot KMO per Item (Lollipop Chart) ====
  output$kmo_item <- renderPlot({
    req(efa_result())
    out <- efa_result()
    kmo_res <- out$kmo
    req(!is.null(kmo_res))
    # Ambil nilai MSA per item (measure of sampling adequacy)
    kmo_values <- kmo_res$MSAi
    MSA_all <- data.frame(Item = 'Overall', KMO= kmo_res$MSA)
    kmo_data <- data.frame(
      Item = names(kmo_values),
      KMO = as.numeric(kmo_values)
    ) %>% rbind(MSA_all)
    
    # Tambahkan kategori kualitas KMO
    kmo_data <- kmo_data %>%
      dplyr::mutate(Kategori = dplyr::case_when(
        KMO < 0.5 ~ "Unacceptable",
        KMO < 0.6 ~ "Miserable",
        KMO < 0.7 ~ "Mediocre",
        KMO < 0.8 ~ "Meritorious",
        TRUE ~ "Marvelous"
      ))
    
    # Skema warna kategori
    kategori_colors <- c(
      "Unacceptable" = "#d73027",
      "Miserable"    = "#fc8d59",
      "Mediocre"     = "#fee08b",
      "Meritorious"  = "#4575b4",
      "Marvelous"    = "blue"
    )
    kmo_data$Item <- factor(kmo_data$Item, levels = kmo_data$Item)
    
    # Plot lollipop
    ggplot(kmo_data, aes(x = KMO, y = Item)) +
      geom_segment(aes(x = 0, xend = KMO, yend = Item, color = Kategori), linewidth = 1.5) +
      geom_point(aes(color = Kategori), size = 4) +
      # === Tambahkan label nilai di ujung ===
      geom_text(aes(label = round(KMO, 2)), 
                hjust = -0.3,  # geser sedikit ke kanan
                size = 3, 
                color = "black") +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1) +
      scale_color_manual(values = kategori_colors) +
      labs(
        title = "Kaiser-Meyer-Olkin (KMO) per Item",
        subtitle = "Measure of Sampling Adequacy (MSA)",
        x = "MSA Value",
        y = "Item",
        color = "Category"
      ) +
      xlim(0, 1) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11),
        axis.text.y = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  
  # Scree plot behaviour:
  output$scree_plot <- renderPlot({
    req(efa_result())
    out <- efa_result()
    df <- out$df_used
    invisible(capture.output(
      psych::fa.parallel(df, fm = "ml", n.iter = 20, main = "Parallel Analysis (fa.parallel)")
    ))    
  })

  # ==== Output Ringkasan Hasil FA (Centered) ====
  output$efa_summary <- renderUI({
    req(efa_result())
    out <- efa_result()
    
    if (is.null(out$fa)) {
      return(tags$p("No factor analysis result available.", style = "color: #999; font-style: italic; text-align:center;"))
    }
    
    fa <- out$fa
    n_factors <- fa$factors
    method <- fa$method
    rotation <- fa$rotation
    total_var <- sum(fa$Vaccounted["Proportion Var", ]) * 100
    
    # --- Ambil nama faktor dari input teks ---
    factor_names <- sapply(1:n_factors, function(i) {
      input[[paste0("factor_name_", i)]] %||% paste0("Factor ", i)
    })
    
    # --- Matriks loading ---
    L <- as.matrix(fa$loadings)
    if (is.null(colnames(L))) colnames(L) <- paste0("Factor", seq_len(ncol(L)))
    if (length(factor_names) == ncol(L)) colnames(L) <- factor_names
    rown <- rownames(L)
    
    # Tentukan loading terbesar per item
    absmat <- abs(L)
    max.idx <- apply(absmat, 1, function(x) if (all(is.na(x))) NA_integer_ else which.max(x))
    
    # --- Buat tabel loading HTML ---
    hdr <- paste0(
      "<tr><th style='text-align:left;padding:6px;border:1px solid #ccc;background:#f8f8f8;'>Item</th>",
      paste0("<th style='padding:6px;border:1px solid #ccc;background:#f8f8f8;'>", colnames(L), "</th>", collapse = ""),
      "</tr>"
    )
    
    rows_html <- vapply(seq_len(nrow(L)), FUN.VALUE = character(1), function(i) {
      cells <- vapply(seq_len(ncol(L)), FUN.VALUE = character(1), function(j) {
        val <- L[i, j]
        if (is.na(val)) txt <- "" else txt <- format(round(val, 3), nsmall = 3)
        if (!is.na(max.idx[i]) && j == max.idx[i] && abs(L[i, j]) > 0) {
          paste0("<td style='background:#b2f0b2;padding:6px;border:1px solid #ccc;text-align:center;font-weight:600;'>", txt, "</td>")
        } else {
          paste0("<td style='padding:6px;border:1px solid #ccc;text-align:center;'>", txt, "</td>")
        }
      }, USE.NAMES = FALSE)
      paste0("<tr><td style='text-align:left;padding:6px;border:1px solid #ccc;'>", rown[i], "</td>", paste(cells, collapse = ""), "</tr>")
    })
    
    tbl_html <- paste0("<table style='margin:auto;border-collapse:collapse;width:auto;font-size:13px;font-family:Arial,Helvetica,sans-serif;'>",
                       hdr, paste(rows_html, collapse = ""), "</table>")
    
    # --- Variance explained ---
    variance_df <- as.data.frame(round(fa$Vaccounted * 100, 2))
    variance_df <- tibble::rownames_to_column(variance_df, var = "Metric")
    if (length(factor_names) == ncol(variance_df) - 1)
      colnames(variance_df)[-1] <- factor_names
    
    # Gunakan table HTML agar tetap center
    variance_tbl <- paste0(
      "<table style='margin:auto;border-collapse:collapse;width:auto;font-size:13px;font-family:Arial,Helvetica,sans-serif;'>",
      paste0(
        "<tr><th style='padding:6px;border:1px solid #ccc;background:#f0f0f0;'>", 
        paste(colnames(variance_df), collapse = "</th><th style='padding:6px;border:1px solid #ccc;background:#f0f0f0;'>"), 
        "</th></tr>"
      ),
      paste(
        apply(variance_df, 1, function(row) {
          paste0("<tr>", paste0("<td style='text-align:center;padding:6px;border:1px solid #ccc;'>", row, "</td>", collapse = ""), "</tr>")
        }),
        collapse = ""
      ),
      "</table>"
    )
    
    tagList(
      div(
        style = "text-align:center;",
        tags$h4("Exploratory Factor Analysis Summary", style = "font-weight:bold;color:#2c3e50;"),
        tags$p(HTML(paste0(
          "<b>Extraction method:</b> ", method, "<br>",
          "<b>Rotation:</b> ", rotation, "<br>",
          "<b>Number of factors extracted:</b> ", n_factors, "<br>",
          "<b>Total variance explained:</b> ", sprintf("%.2f%%", total_var)
        ))),
        tags$h5("Factor Loadings", style = "margin-top:15px;font-weight:bold;"),
        HTML(tbl_html),
        tags$h5("Variance Explained by Each Factor", style = "margin-top:20px;font-weight:bold;"),
        HTML(variance_tbl)
      )
    )
  })
  
  # =====Factor Scores EFA =====
  output$efa_scores_table <- DT::renderDataTable({
    req(efa_result())
    out <- efa_result()
    
    if (is.null(out$fa)) {
      return(tags$p("No factor analysis result available.", style = "color: #999; font-style: italic; text-align:center;"))
    }
    n_factors <- out$fa$factors
    
    # --- Ambil nama faktor dari input teks ---
    factor_names <- sapply(1:n_factors, function(i) {
      input[[paste0("factor_name_", i)]] %||% paste0("Factor ", i)
    })
    
    scores <- efa_result()$scores
    if (length(factor_names) == ncol(scores))
      colnames(scores) <- factor_names
    datatable(
      round(as.data.frame(scores), 3),extensions = 'Buttons',
      options = list(dom='Brtp',scrollX = TRUE, pageLength = 25,  
                     buttons = list(
                       list(extend = 'csv',
                            text = 'Export CSV',
                            filename = 'Factor Scores EFA'
                       ),
                       list(extend = 'excel',
                            text = 'Export Excel',
                            filename = 'Factor Scores EFA'
                       ))),
      caption = "Estimated Factor Scores for Each Observation"
    )
  })
  # ==== 3. DOWNLOAD BUTTONS ====
  output$download_loading <- downloadHandler(
    filename = function() {
      paste0("efa_loadings_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- efa_result()
      load_tab <- as.data.frame(round(res$fa$loadings[, ], 3))
      load_tab$Item <- rownames(res$fa$loadings)
      load_tab <- load_tab[, c(ncol(load_tab), 1:(ncol(load_tab)-1))]
      write.csv(load_tab, file, row.names = FALSE)
    }
  )
  
  output$download_scores <- downloadHandler(
    filename = function() {
      paste0("efa_factor_scores_", Sys.Date(), ".csv")
    },
    content = function(file) {
      scores <- efa_result()$scores
      write.csv(round(as.data.frame(scores), 3), file, row.names = FALSE)
    }
  )
}