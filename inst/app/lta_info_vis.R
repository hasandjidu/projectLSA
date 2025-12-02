
library(viridisLite)
# ---- Helper functions ----
# Create Theta grid for visualization
make_theta_grid <- function(nfactors, dim_x = 1, dim_y = 2, seq_x = seq(-4,4,0.25), seq_y = seq(-4,4,0.25), fixed_vals = NULL) {
  # nfactors: total factors in model
  # dim_x, dim_y: indices of the two dimensions to visualise
  # fixed_vals: named numeric vector for other dims (length nfactors - 2), default 0
  if (nfactors == 1) {
    Theta <- matrix(seq_x, ncol = 1)
    colnames(Theta) <- paste0("F", 1)
    list(Theta = Theta, grid_x = seq_x, grid_y = NULL)
  } else if (nfactors == 2) {
    g <- expand.grid(seq_x, seq_y)
    colnames(g) <- paste0("F", c(dim_x, dim_y))
    Theta <- as.matrix(g)
    colnames(Theta) <- paste0("F", 1:2)
    list(Theta = Theta, grid_x = seq_x, grid_y = seq_y)
  } else {
    # generate grid for dim_x and dim_y; fix others
    g <- expand.grid(seq_x, seq_y)
    # build full Theta matrix with nfactors cols
    Theta_full <- matrix(0, nrow = nrow(g), ncol = nfactors)
    colnames(Theta_full) <- paste0("F", 1:nfactors)
    Theta_full[, dim_x] <- g[[1]]
    Theta_full[, dim_y] <- g[[2]]
    # set fixed values
    if (!is.null(fixed_vals)) {
      for (i in seq_len(nfactors)) {
        if (i != dim_x && i != dim_y) {
          if (!is.null(fixed_vals[paste0('F', i)])) Theta_full[, i] <- fixed_vals[paste0('F', i)]
        }
      }
    }
    list(Theta = Theta_full, grid_x = seq_x, grid_y = seq_y)
  }
}

# compute testinfo and SE given a mirt model and Theta matrix
# --- Helper: compute test information and SE (auto degrees) ---
compute_info_se <- function(mod, Theta_mat) {
  # deteksi jumlah dimensi laten dari model
  n_dim <- tryCatch(extract.mirt(mod, "nfact"), error = function(e) 1)
  
  # buat vektor degrees otomatis sepanjang jumlah dimensi
  degrees <- rep(0, n_dim)
  
  # hitung test information
  tinfo <- testinfo(mod, Theta = Theta_mat, degrees = degrees)
  
  # hitung standard error (hindari pembagian 0)
  se <- 1 / sqrt(pmax(tinfo, .Machine$double.eps))
  
  list(info = tinfo, se = se)
}

# reshape vector of info into matrix matching grid_x x grid_y (row-major by grid_x)
vec_to_matrix <- function(vec, len_x, len_y) {
  # mirt returns values in the order of expand.grid(seq_x, seq_y) => first varies X then Y?
  # In R, expand.grid(seq_x, seq_y) returns first column varying slowest: seq_x repeats for each seq_y.
  # We used expand.grid(seq_x, seq_y) meaning rows correspond to combinations (x1,y1),(x2,y1)...? To be safe, reshape by length(seq_x)
  matrix(vec, nrow = len_x, ncol = len_y, byrow = FALSE)
}

# ---- Shiny module UI ----
infoVisUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # === 5️⃣ Info Function Multi-dim ===
      column(3,
             selectInput(ns('dim_x'), 'X dimension', choices = NULL),
             selectInput(ns('dim_y'), 'Y dimension', choices = NULL),
             numericInput(ns('grid_res'), 'Grid resolution (step)', value = 0.25, min = 0.05, step = 0.05),
             uiOutput(ns('fix_other_ui')),
             actionButton(ns('update_plot'), 'Update')
      ),
      column(9,
             tabsetPanel(
               tabPanel('Surface / Heatmap',
                        plotlyOutput(ns('surface_plot'), height = '500px'),
                        br(),
                        plotOutput(ns('heatmap_plot'), height = '300px')
               ),
               tabPanel('SE Surface',
                        plotlyOutput(ns('se_surface'), height = '500px'),
                        br(),
                        plotOutput(ns('se_heatmap'), height = '300px')
               ),
               tabPanel('Item Info',
                        selectInput(ns('which_item'), 'Which item (optional)', choices = NULL),
                        plotOutput(ns('item_info_plot'), height = '400px')
               ),
               tabPanel('Interpretation',
                        verbatimTextOutput(ns('interpretation'))
               )
             )
      )
    )
  )
}

# ---- Shiny module server ----
infoVisServer <- function(id, model_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(model_reactive(), {
      mod <- model_reactive()
      req(mod)
      nf <- mod@Data$ncol
      # number of latent dimensions
      nfactors <- mod@Model$nfact
      dims <- paste0('F', seq_len(nfactors))
      updateSelectInput(session, 'dim_x', choices = dims, selected = dims[1])
      if (nfactors >= 2) updateSelectInput(session, 'dim_y', choices = dims, selected = dims[2])
      updateSelectInput(session, 'which_item', choices = c('All testinfo', rownames(as.data.frame(coef(mod, simplify=TRUE)$items))))
    })
    
    output$fix_other_ui <- renderUI({
      mod <- model_reactive()
      req(mod)
      nfactors <- mod@Model$nfact
      dims <- paste0('F', seq_len(nfactors))
      if (nfactors <= 2) return(NULL)
      # create sliders for other dimensions
      others <- setdiff(dims, c(input$dim_x, input$dim_y))
      lapply(others, function(d) {
        sliderInput(ns(paste0('fix_', d)), label = paste('Fix', d), min = -4, max = 4, value = 0, step = 0.5)
      })
    })
    
    # gather fixed vals
    fixed_vals_reactive <- reactive({
      mod <- model_reactive()
      req(mod)
      nfactors <- mod@Model$nfact
      dims <- paste0('F', seq_len(nfactors))
      fixed <- numeric(0)
      if (nfactors > 2) {
        others <- setdiff(dims, c(input$dim_x, input$dim_y))
        for (d in others) {
          val <- input[[paste0('fix_', d)]]
          if (is.null(val)) val <- 0
          fixed[paste0(d)] <- val
        }
      }
      fixed
    })
    
    # build grid and compute info when user clicks update or model changes
    grid_info <- eventReactive(list(input$update_plot, model_reactive()), {
      mod <- model_reactive()
      req(mod)
      nfactors <- mod@Model$nfact
      seq_step <- input$grid_res
      seq_x <- seq(-4, 4, by = seq_step)
      seq_y <- seq(-4, 4, by = seq_step)
      dx <- as.integer(sub('F', '', input$dim_x))
      dy <- if (!is.null(input$dim_y)) as.integer(sub('F', '', input$dim_y)) else dx
      fixed_vals <- fixed_vals_reactive()
      g <- make_theta_grid(nfactors = nfactors, dim_x = dx, dim_y = dy, seq_x = seq_x, seq_y = seq_y, fixed_vals = fixed_vals)
      res <- compute_info_se(mod, g$Theta)
      list(grid = g, info = res$info, se = res$se, nfactors = nfactors)
    }, ignoreNULL = FALSE)
    
    output$surface_plot <- renderPlotly({
      gi <- grid_info()
      req(gi)
      g <- gi$grid
      len_x <- length(g$grid_x)
      len_y <- ifelse(is.null(g$grid_y), 1, length(g$grid_y))
      zmat <- vec_to_matrix(gi$info, len_x, len_y)
      plot_ly(x = g$grid_x, y = g$grid_y, z = zmat, type = 'surface') %>%
        layout(scene = list(xaxis = list(title = input$dim_x), yaxis = list(title = input$dim_y), zaxis = list(title = 'Information')))
    })
    
    output$heatmap_plot <- renderPlot({
      gi <- grid_info()
      req(gi)
      g <- gi$grid
      df_plot <- if (is.null(g$grid_y)) {
        tibble::tibble(X = g$grid_x, info = gi$info)
      } else {
        # reconstruct expand.grid order
        d <- expand.grid(g$grid_x, g$grid_y)
        tibble::tibble(X = d[[1]], Y = d[[2]], info = gi$info)
      }
      if ('Y' %in% names(df_plot)) {
        ggplot(df_plot, aes(X, Y, fill = info)) +
          geom_raster() +
          scale_fill_viridis_c() +
          labs(x = input$dim_x, y = input$dim_y, fill = 'Information') +
          theme_minimal()
      } else {
        ggplot(df_plot, aes(X, info)) + geom_line() + geom_point() + theme_minimal() + labs(x = input$dim_x, y = 'Information')
      }
    })
    
    output$se_surface <- renderPlotly({
      gi <- grid_info()
      req(gi)
      g <- gi$grid
      len_x <- length(g$grid_x)
      len_y <- ifelse(is.null(g$grid_y), 1, length(g$grid_y))
      zmat <- vec_to_matrix(gi$se, len_x, len_y)
      plot_ly(x = g$grid_x, y = g$grid_y, z = zmat, type = 'surface') %>%
        layout(scene = list(xaxis = list(title = input$dim_x), yaxis = list(title = input$dim_y), zaxis = list(title = 'SE')))
    })
    
    output$se_heatmap <- renderPlot({
      gi <- grid_info()
      req(gi)
      g <- gi$grid
      df_plot <- if (is.null(g$grid_y)) {
        tibble::tibble(X = g$grid_x, se = gi$se)
      } else {
        d <- expand.grid(g$grid_x, g$grid_y)
        tibble::tibble(X = d[[1]], Y = d[[2]], se = gi$se)
      }
      if ('Y' %in% names(df_plot)) {
        ggplot(df_plot, aes(X, Y, fill = se)) +
          geom_raster() +
          scale_fill_viridis_c(direction = -1) +
          labs(x = input$dim_x, y = input$dim_y, fill = 'SE') +
          theme_minimal()
      } else {
        ggplot(df_plot, aes(X, se)) + geom_line() + geom_point() + theme_minimal() + labs(x = input$dim_x, y = 'SE')
      }
    })
    
    output$item_info_plot <- renderPlot({
      mod <- model_reactive()
      req(mod)
      which_item <- input$which_item
      if (is.null(which_item) || which_item == 'All testinfo') {
        plot(mod, type = 'info')
      } else {
        it <- which(rownames(as.data.frame(coef(mod, simplify=TRUE)$items)) == which_item)
        if (length(it) == 0) {
          plot(mod, type = 'info')
        } else {
          # compute iteminfo over a reasonable grid (unidim projection)
          nfactors <- mod@Model$nfact
          if (nfactors == 1) {
            Theta <- matrix(seq(-4,4,0.1), ncol=1)
            item_info <- iteminfo(extract.item(mod, it), Theta)
            plot(seq(-4,4,0.1), item_info, type='l', xlab='Theta', ylab='Item Information')
          } else {
            # For multidim, compute item info on slice fixing others at 0
            g <- make_theta_grid(nfactors = mod@Model$nfact, dim_x = 1, dim_y = 2, 
                                 seq_x = seq(-4,4,0.25), seq_y = seq(-4,4,0.25), fixed_vals = NULL)
            # build degrees vector with correct length:
            n_dim <- mod@Model$nfact
            # If you want a single angle replicated across dims (safe default 0)
            degrees_vec <- rep(45, n_dim)
            # iteminfo expects Theta matrix matching nfactors
            ii <- iteminfo(extract.item(mod, it), g$Theta,degrees = degrees_vec)
            mat <- vec_to_matrix(ii, length(g$grid_x), length(g$grid_y))
            image(x = g$grid_x, y = g$grid_y, z = t(mat), xlab='F1', ylab='F2', main = paste('Item info -', which_item))
          }
        }
      }
    })
    output$interpretation <- renderText({
      gi <- grid_info()
      req(gi)
      g <- gi$grid
      # find peak info
      idx <- which.max(gi$info)
      if (gi$nfactors == 1) {
        peak_x <- g$grid_x[idx]
        paste0('Peak information at ', input$dim_x, ' = ', round(peak_x, 2), '. SE at that point = ', round(gi$se[idx], 3))
      } else {
        # map idx to coordinates
        dfx <- expand.grid(g$grid_x, g$grid_y)
        peak_x <- dfx[[1]][idx]
        peak_y <- dfx[[2]][idx]
        paste0('Peak test information found at ', input$dim_x, ' ≈ ', round(peak_x,2), ' and ', input$dim_y, ' ≈ ', round(peak_y,2),
               '. Standard error there ≈ ', round(gi$se[idx], 3),
               '\nInterpretation: the test is most precise for respondents with this combination of latent traits. Areas far from this point show lower information and higher SE.')
      }
    })
  })
}

