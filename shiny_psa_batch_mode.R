
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, grid, ggrepel, patchwork,
               DT, grid, shiny)
# UI
ui = fluidPage(
  titlePanel("Productivity-Susceptibility Analysis (PSA) in batch mode"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # 1. Upload data
      fileInput("datafile", "1. Upload CSV Data File",
                accept = c("text/csv", ".csv")),
      
      hr(),
      
      # 2. Threshold settings
      h4("2. Adjust Thresholds & Weights"),
      
      tabsetPanel(
        id = "threshold_tabs",
        
        tabPanel("Productivity",
                 br(),
                 fluidRow(
                   column(8, h5("Intrinsic growth rate (r)")),
                   column(4, numericInput("weight_r", "Weight", 2, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("r_high", "High (>)", 0.5, step = 0.01),
                 numericInput("r_low", "Low (<)", 0.16, step = 0.01),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Maximum age (tmax)")),
                   column(4, numericInput("weight_tmax", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("tmax_high", "High (<)", 10, step = 1),
                 numericInput("tmax_low", "Low (>)", 30, step = 1),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Maximum size (Lmax)")),
                   column(4, numericInput("weight_lmax", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("lmax_high", "High (<)", 60, step = 1),
                 numericInput("lmax_low", "Low (>)", 150, step = 1),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Von Bertalanffy K")),
                   column(4, numericInput("weight_k", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("k_high", "High (>)", 0.25, step = 0.01),
                 numericInput("k_low", "Low (<)", 0.15, step = 0.01),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Natural mortality (M)")),
                   column(4, numericInput("weight_m", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("m_high", "High (>)", 0.4, step = 0.01),
                 numericInput("m_low", "Low (<)", 0.2, step = 0.01),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Fecundity")),
                   column(4, numericInput("weight_fec", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("fec_high", "High (>)", 100000, step = 1000),
                 numericInput("fec_low", "Low (<)", 10000, step = 1000),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Breeding strategy")),
                   column(4, numericInput("weight_breed", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: see original code for values)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Recruitment frequency")),
                   column(4, numericInput("weight_rec", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: highfreq/modfreq/lowfreq)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Age at maturity (tmat)")),
                   column(4, numericInput("weight_tmat", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("tmat_high", "High (>)", 4, step = 0.1),
                 numericInput("tmat_low", "Low (<)", 2, step = 0.1),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Mean trophic level")),
                   column(4, numericInput("weight_troph", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("troph_high", "High (<)", 2.5, step = 0.1),
                 numericInput("troph_low", "Low (>)", 3.5, step = 0.1)
        ),
        
        tabPanel("Susceptibility",
                 br(),
                 fluidRow(
                   column(8, h5("Areal overlap")),
                   column(4, numericInput("weight_area_over", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("area_over_high", "High (>)", 0.5, step = 0.01),
                 numericInput("area_over_low", "Low (<)", 0.25, step = 0.01),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Geographic concentration")),
                   column(4, numericInput("weight_geog_conc", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: high/mod/low)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Vertical overlap")),
                   column(4, numericInput("weight_vert_over", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: high/mod/low)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Seasonal migrations")),
                   column(4, numericInput("weight_seas_migr", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: increase_fish/no_effect/decrease_fish)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Schooling behavior")),
                   column(4, numericInput("weight_school", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: increase_fish/no_effect/decrease_fish)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Morphology affecting capture")),
                   column(4, numericInput("weight_morph", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: high_selec/mod_selec/low_selec)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Desirability")),
                   column(4, numericInput("weight_desire", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: high/mod/low)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Management strategy")),
                   column(4, numericInput("weight_mng_strat", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: no_strat/reactive/proactive)", style = "font-size: 11px; color: gray;"),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("F/M ratio")),
                   column(4, numericInput("weight_f_over_m", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("f_over_m_high", "High (>)", 1, step = 0.1),
                 numericInput("f_over_m_low", "Low (<)", 0.5, step = 0.1),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("B/B0 ratio")),
                   column(4, numericInput("weight_b_over_b0", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("b_over_b0_high", "High (<)", 0.25, step = 0.01),
                 numericInput("b_over_b0_low", "Low (>)", 0.4, step = 0.01),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Survival probability")),
                   column(4, numericInput("weight_surv_prob", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 numericInput("surv_prob_high", "High (<)", 0.33, step = 0.01),
                 numericInput("surv_prob_low", "Low (>)", 0.67, step = 0.01),
                 
                 hr(),
                 fluidRow(
                   column(8, h5("Habitat impact")),
                   column(4, numericInput("weight_hat_impact", "Weight", 1, min = 0, step = 0.5, width = "100%"))
                 ),
                 p("(Categorical: high/mod/low)", style = "font-size: 11px; color: gray;")
        )
      ),
      
      hr(),
      
      # Analysis settings
      h4("Analysis Settings"),
      numericInput("num_samples", "Bootstrap samples", 999, min = 100, step = 100),
      numericInput("vul_high", "High vulnerability (≥)", 2.2, step = 0.1),
      numericInput("vul_low", "Low vulnerability (<)", 1.8, step = 0.1),
      
      hr(),
      
      actionButton("run_analysis", "Run Analysis", class = "btn-primary btn-lg")
    ),
    
    mainPanel(
      width = 8,
      
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel("Data Preview",
                 br(),
                 DTOutput("data_preview")
        ),
        
        tabPanel("Results Table",
                 br(),
                 downloadButton("download_results", "Download Results CSV"),
                 br(), br(),
                 DTOutput("results_table")
        ),
        
        tabPanel("PSA Plot",
                 br(),
                 downloadButton("download_plot", "Download Plot (PNG)"),
                 br(), br(),
                 plotOutput("psa_plot", height = "800px")
        )
      )
    )
  )
)

# Server
server = function(input, output, session) {
  
  # Reactive value to store uploaded data
  uploaded_data = reactiveVal(NULL)
  
  # Reactive value to store results
  analysis_results = reactiveVal(NULL)
  psa_plot = reactiveVal(NULL)
  
  # Reactive value to store probability assignments
  probability_assignments = reactiveVal(list())
  
  # Reactive value to store categorized data and species list
  categorized_data = reactiveVal(NULL)
  species_list_data = reactiveVal(NULL)
  
  # Load data
  observeEvent(input$datafile, {
    req(input$datafile)
    df = read.csv(input$datafile$datapath)
    uploaded_data(df)
  })
  
  # Preview uploaded data
  output$data_preview = renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Probability assignment modal
  showProbabilityModal = function(species_list, df_cat) {
    showModal(modalDialog(
      title = "3. Assign Probabilities (Optional)",
      size = "l",
      
      fluidRow(
        column(6, selectInput("modal_species", "Select Species", 
                              choices = c("", unique(df_cat$species)))),
        column(6, selectInput("modal_attribute", "Select Attribute", 
                              choices = c("", setdiff(names(df_cat), 'species'))))
      ),
      
      h5("Current Categorization:"),
      verbatimTextOutput("current_categorization"),
      
      hr(),
      
      h5("Set Probabilities:"),
      fluidRow(
        column(4, numericInput("modal_prob_low", "Low", 0, min = 0, max = 1, step = 0.1)),
        column(4, numericInput("modal_prob_mod", "Moderate", 0, min = 0, max = 1, step = 0.1)),
        column(4, numericInput("modal_prob_high", "High", 0, min = 0, max = 1, step = 0.1))
      ),
      
      fluidRow(
        column(12, 
               actionButton("modal_assign_prob", "Assign Probability", class = "btn-warning"),
               actionButton("modal_clear_probs", "Clear All Probabilities", class = "btn-danger btn-sm")
        )
      ),
      
      hr(),
      
      h5("Current Probability Assignments:"),
      verbatimTextOutput("modal_prob_status"),
      
      footer = tagList(
        actionButton("modal_continue", "Continue to Analysis", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  }
  
  # Display current categorization in modal
  output$current_categorization = renderText({
    req(input$modal_species, input$modal_attribute, categorized_data())
    
    if (input$modal_species == "" || input$modal_attribute == "") {
      return("Select a species and attribute to view categorization")
    }
    
    df_cat = categorized_data()
    value = df_cat[df_cat$species == input$modal_species, input$modal_attribute]
    
    if (length(value) > 0 && !is.na(value)) {
      paste0("Current category for ", input$modal_species, " - ", 
             input$modal_attribute, ": ", value)
    } else {
      paste0("No data for ", input$modal_species, " - ", input$modal_attribute)
    }
  })
  
  # Assign probability in modal
  observeEvent(input$modal_assign_prob, {
    req(input$modal_species, input$modal_attribute)
    
    if (input$modal_species == "" || input$modal_attribute == "") {
      showNotification("Please select both species and attribute", 
                       type = "warning", duration = 3)
      return()
    }
    
    # Validate probabilities sum to 1
    total = input$modal_prob_low + input$modal_prob_mod + input$modal_prob_high
    if (abs(total - 1) > 0.01 && total > 0) {
      showNotification("Warning: Probabilities should sum to 1.0", 
                       type = "warning", duration = 3)
    }
    
    # Store probability assignment
    probs = probability_assignments()
    key = paste(input$modal_species, input$modal_attribute, sep = "||")
    probs[[key]] = list(
      species = input$modal_species,
      attribute = input$modal_attribute,
      low = input$modal_prob_low,
      mod = input$modal_prob_mod,
      high = input$modal_prob_high
    )
    probability_assignments(probs)
    
    showNotification(
      paste("Assigned probabilities for", input$modal_species, "-", input$modal_attribute),
      type = "message", duration = 2
    )
  })
  
  # Clear all probabilities in modal
  observeEvent(input$modal_clear_probs, {
    probability_assignments(list())
    showNotification("All probability assignments cleared", 
                     type = "message", duration = 2)
  })
  
  # Display probability status in modal
  output$modal_prob_status = renderText({
    probs = probability_assignments()
    if (length(probs) == 0) {
      return("No probability assignments made.")
    }
    
    assignments = sapply(names(probs), function(key) {
      p = probs[[key]]
      sprintf("%s - %s: L=%.2f, M=%.2f, H=%.2f", 
              p$species, p$attribute, p$low, p$mod, p$high)
    })
    
    paste("Probability Assignments:\n", 
          paste(assignments, collapse = "\n"), sep = "")
  })
  
  # Run analysis - First categorize, then show modal
  observeEvent(input$run_analysis, {
    req(uploaded_data())
    
    df = uploaded_data()
    
    # Define categorization functions with user inputs
    cat_r = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x > input$r_high, 'high', 
                    ifelse(x >= input$r_low & x <= input$r_high, 'mod', 'low')))
    }
    
    cat_tmax = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x < input$tmax_high, 'high', 
                    ifelse(x >= input$tmax_high & x <= input$tmax_low, 'mod', 'low')))
    }
    
    cat_lmax = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x < input$lmax_high, 'high', 
                    ifelse(x >= input$lmax_high & x <= input$lmax_low, 'mod', 'low')))
    }
    
    cat_k = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x > input$k_high, 'high', 
                    ifelse(x >= input$k_low & x <= input$k_high, 'mod', 'low')))
    }
    
    cat_m = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x > input$m_high, 'high', 
                    ifelse(x >= input$m_low & x <= input$m_high, 'mod', 'low')))
    }
    
    cat_fec = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x > input$fec_high, 'high', 
                    ifelse(x >= input$fec_low & x <= input$fec_high, 'mod', 'low')))
    }
    
    cat_breed = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 0, 'high', 
                    ifelse(x %in% 1:3, 'mod',
                           ifelse(x %in% 4:14, 'low', NA))))
    }
    
    cat_rec = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'highfreq', 'high', 
                    ifelse(x == 'modfreq', 'mod',
                           ifelse(x == 'lowfreq', 'low', NA))))
    }
    
    cat_tmat = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x > input$tmat_high, 'high', 
                    ifelse(x >= input$tmat_low & x <= input$tmat_high, 'mod', 'low')))
    }
    
    cat_troph = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x < input$troph_high, 'high', 
                    ifelse(x >= input$troph_high & x <= input$troph_low, 'mod', 'low')))
    }
    
    cat_area_over = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x > input$area_over_high, 'high', 
                    ifelse(x >= input$area_over_low & x <= input$area_over_high, 'mod', 'low')))
    }
    
    cat_geog_conc = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'high', 'high', 
                    ifelse(x == 'mod', 'mod',
                           ifelse(x == 'low', 'low', NA))))
    }
    
    cat_vert_over = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'high', 'high', 
                    ifelse(x == 'mod', 'mod',
                           ifelse(x == 'low', 'low', NA))))
    }
    
    cat_seas_migr = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'increase_fish', 'high', 
                    ifelse(x == 'no_effect', 'mod',
                           ifelse(x == 'decrease_fish', 'low', NA))))
    }
    
    cat_school = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'increase_fish', 'high', 
                    ifelse(x == 'no_effect', 'mod',
                           ifelse(x == 'decrease_fish', 'low', NA))))
    }
    
    cat_morph = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'high_selec', 'high', 
                    ifelse(x == 'mod_selec', 'mod',
                           ifelse(x == 'low_selec', 'low', NA))))
    }
    
    cat_desire = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'high', 'high', 
                    ifelse(x == 'mod', 'mod',
                           ifelse(x == 'low', 'low', NA))))
    }
    
    cat_mng_strat = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'no_strat', 'high', 
                    ifelse(x == 'reactive', 'mod',
                           ifelse(x == 'proactive', 'low', NA))))
    }
    
    cat_f_over_m = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x > input$f_over_m_high, 'high', 
                    ifelse(x >= input$f_over_m_low & x <= input$f_over_m_high, 'mod', 'low')))
    }
    
    cat_b_over_b0 = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x < input$b_over_b0_high, 'high', 
                    ifelse(x >= input$b_over_b0_high & x <= input$b_over_b0_low, 'mod', 'low')))
    }
    
    cat_surv_prob = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x < input$surv_prob_high, 'high', 
                    ifelse(x >= input$surv_prob_high & x <= input$surv_prob_low, 'mod', 'low')))
    }
    
    cat_hat_impact = function(x) {
      ifelse(is.na(x), NA, 
             ifelse(x == 'high', 'high', 
                    ifelse(x == 'mod', 'mod',
                           ifelse(x == 'low', 'low', NA))))
    }
    
    # Categorize attributes
    categorize_attributes = function(df) {
      cols = setdiff(names(df), 'species')
      for (col in cols) {
        func_name = paste0('cat_', col)
        if (exists(func_name, mode = 'function')) {
          func = get(func_name)
          df[[col]] = func(df[[col]])
        }
      }
      return(df)
    }
    
    df_cat = categorize_attributes(df)
    
    # Create species list
    create_species_list = function(df) {
      species_list = list()
      attribute_cols = setdiff(names(df), 'species')
      for (species in unique(df$species)) {
        species_data = df[df$species == species, ]
        transformed_df = data.frame(
          attribute = attribute_cols,
          low = 0,
          mod = 0,
          high = 0,
          weight = 1
        )
        for (attr in attribute_cols) {
          value = species_data[[attr]]
          if (!is.na(value)) {
            transformed_df[[value]][transformed_df$attribute == attr] = 1
          } else {
            transformed_df$weight[transformed_df$attribute == attr] = 0
          }
        }
        species_list[[species]] = transformed_df
      }
      return(species_list)
    }
    
    species_list = create_species_list(df_cat)
    
    # Apply user-defined weights to all species
    weight_mapping = list(
      r = input$weight_r,
      tmax = input$weight_tmax,
      lmax = input$weight_lmax,
      k = input$weight_k,
      m = input$weight_m,
      fec = input$weight_fec,
      breed = input$weight_breed,
      rec = input$weight_rec,
      tmat = input$weight_tmat,
      troph = input$weight_troph,
      area_over = input$weight_area_over,
      geog_conc = input$weight_geog_conc,
      vert_over = input$weight_vert_over,
      seas_migr = input$weight_seas_migr,
      school = input$weight_school,
      morph = input$weight_morph,
      desire = input$weight_desire,
      mng_strat = input$weight_mng_strat,
      f_over_m = input$weight_f_over_m,
      b_over_b0 = input$weight_b_over_b0,
      surv_prob = input$weight_surv_prob,
      hat_impact = input$weight_hat_impact
    )
    
    for (species_name in names(species_list)) {
      species_df = species_list[[species_name]]
      
      # Apply weights from user inputs
      for (attr_name in names(weight_mapping)) {
        attr_rows = which(species_df$attribute == attr_name)
        if (length(attr_rows) > 0) {
          species_df$weight[attr_rows] = weight_mapping[[attr_name]]
        }
      }
      
      species_list[[species_name]] = species_df
    }
    
    # Store categorized data and species list
    categorized_data(df_cat)
    species_list_data(species_list)
    
    # Show probability assignment modal
    showProbabilityModal(species_list, df_cat)
  })
  
  # Continue with analysis after modal
  observeEvent(input$modal_continue, {
    req(categorized_data(), species_list_data())
    
    removeModal()
    
    species_list = species_list_data()
    
    # Apply probability assignments (probabilities override the default 0/1 categorizations)
    probs = probability_assignments()
    if (length(probs) > 0) {
      for (key in names(probs)) {
        p = probs[[key]]
        species_name = p$species
        attr_name = p$attribute
        
        if (species_name %in% names(species_list)) {
          species_df = species_list[[species_name]]
          attr_row = which(species_df$attribute == attr_name)
          
          if (length(attr_row) > 0) {
            species_df$low[attr_row] = p$low
            species_df$mod[attr_row] = p$mod
            species_df$high[attr_row] = p$high
            species_list[[species_name]] = species_df
          }
        }
      }
    }
    
    # PSA calculation
    num_samples = input$num_samples
    num_prod_attr = 10
    num_susc_attr = 12
    
    vulnerability_scores = list()
    
    for (species in names(species_list)) {
      species_data = species_list[[species]]
      species_data_numeric = species_data
      species_data_numeric[, c("low", "mod", "high")] = lapply(
        species_data[, c("low", "mod", "high")], as.numeric
      )
      
      prodMatrix = matrix(NA, nrow = num_samples, ncol = num_prod_attr + 1)
      suscMatrix = matrix(NA, nrow = num_samples, ncol = num_susc_attr + 1)
      
      sumProdWeights = sum(species_data$weight[1:num_prod_attr], na.rm = TRUE)
      sumSuscWeights = sum(species_data$weight[(num_prod_attr + 1):(num_prod_attr + num_susc_attr)], na.rm = TRUE)
      
      for (i in 1:num_prod_attr) {
        prob_high = species_data_numeric$high[i]
        prob_mod = species_data_numeric$mod[i]
        prob_low = species_data_numeric$low[i]
        weight = species_data$weight[i]
        
        # Check if probabilities sum to something positive
        prob_sum = prob_high + prob_mod + prob_low
        
        if (!is.na(weight) && weight > 0 && prob_sum > 0) {
          prodMatrix[, i] = weight * sample(c(3, 2, 1), num_samples, replace = TRUE, 
                                            prob = c(prob_high, prob_mod, prob_low))
        } else {
          prodMatrix[, i] = 0
        }
      }
      
      prodMatrix[, num_prod_attr + 1] = apply(prodMatrix[, 1:num_prod_attr], 1, sum) / sumProdWeights
      
      for (i in 1:num_susc_attr) {
        index = num_prod_attr + i
        prob_high = species_data_numeric$high[index]
        prob_mod = species_data_numeric$mod[index]
        prob_low = species_data_numeric$low[index]
        weight = species_data$weight[index]
        
        # Check if probabilities sum to something positive
        prob_sum = prob_high + prob_mod + prob_low
        
        if (!is.na(weight) && weight > 0 && prob_sum > 0) {
          suscMatrix[, i] = weight * sample(c(3, 2, 1), num_samples, replace = TRUE, 
                                            prob = c(prob_high, prob_mod, prob_low))
        } else {
          suscMatrix[, i] = 0
        }
      }
      
      suscMatrix[, num_susc_attr + 1] = apply(suscMatrix[, 1:num_susc_attr], 1, sum) / sumSuscWeights
      
      vuln = sqrt((((3 - prodMatrix[, num_prod_attr + 1])^2) + ((suscMatrix[, num_susc_attr + 1] - 1)^2)))
      
      vulnerability_scores[[species]] = list(
        productivity = prodMatrix[, num_prod_attr + 1],
        susceptibility = suscMatrix[, num_susc_attr + 1],
        vulnerability = vuln
      )
    }
    
    # Create results dataframe
    species_vulnerability_df = data.frame(
      species = character(),
      mean_vulnerability = numeric(),
      min_vulnerability = numeric(),
      max_vulnerability = numeric(),
      mean_productivity = numeric(),
      min_productivity = numeric(),
      max_productivity = numeric(),
      mean_susceptibility = numeric(),
      min_susceptibility = numeric(),
      max_susceptibility = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (species in names(vulnerability_scores)) {
      vuln_values = vulnerability_scores[[species]]$vulnerability
      prod_values = vulnerability_scores[[species]]$productivity
      susc_values = vulnerability_scores[[species]]$susceptibility
      
      species_vulnerability_df = rbind(species_vulnerability_df, data.frame(
        species = species,
        mean_vulnerability = mean(vuln_values),
        min_vulnerability = min(vuln_values),
        max_vulnerability = max(vuln_values),
        mean_productivity = mean(prod_values),
        min_productivity = min(prod_values),
        max_productivity = max(prod_values),
        mean_susceptibility = mean(susc_values),
        min_susceptibility = min(susc_values),
        max_susceptibility = max(susc_values)
      ))
    }
    
    # Vulnerability classification
    vul_class = function(x) {
      ifelse(x >= input$vul_high, 'High', 
             ifelse(x > input$vul_low & x < input$vul_high, 'Moderate', 'Low'))
    }
    
    species_vulnerability_df$vul_category = vul_class(species_vulnerability_df$mean_vulnerability)
    
    # Store results
    analysis_results(species_vulnerability_df)
    
    # Create plot
    xcolor = seq(0, 1, length.out = 200)
    ycolor = seq(0, 1, length.out = 200)
    x = seq(3, 1, length.out = 200)
    y = seq(1, 3, length.out = 200)
    df_col = cbind(expand.grid(x = xcolor, y = ycolor), expand.grid(x = x, y = y))
    colnames(df_col) = c("xcolor", "ycolor", "x", "y")
    df_col$zcolor = (df_col$xcolor^2 + df_col$ycolor^2)
    
    prod_susc_plot = 
      ggplot() +
      geom_tile(data = df_col, aes(x, y, fill = zcolor)) +
      geom_linerange(data = species_vulnerability_df,
                     aes(y = mean_susceptibility, x = mean_productivity,
                         ymin = min_susceptibility, ymax = max_susceptibility), 
                     alpha = 0.4, linewidth = 1) +
      geom_linerange(data = species_vulnerability_df,
                     aes(y = mean_susceptibility, x = mean_productivity,
                         xmin = max_productivity, xmax = min_productivity),
                     alpha = 0.4, linewidth = 1) +
      geom_point(data = species_vulnerability_df,
                 aes(y = mean_susceptibility, x = mean_productivity),
                 alpha = 0.5, size = 2) +
      geom_text_repel(data = species_vulnerability_df,
                      aes(label = species, y = mean_susceptibility,
                          x = mean_productivity), 
                      fontface = 'italic', force = 50, size = 2.3) +
      labs(x = 'Productivity', y = 'Susceptibility') +
      theme_test() +
      theme(axis.text = element_text(color = "black"), legend.position = 'none') +
      coord_cartesian(expand = F) +
      scale_fill_gradientn(colors = c('forestgreen', 'green3', 'green2',
                                      'greenyellow', 'orange3', 'red', 
                                      'red2', 'red3', 'red4')) +
      xlim(1, 3) + ylim(1, 3) +
      scale_x_reverse()
    
    dens_prod_plot = 
      ggplot() +
      geom_density(data = species_vulnerability_df,
                   aes(x = mean_productivity),
                   fill = 'grey',
                   color = 'grey') +
      theme_void() +
      scale_x_continuous(limits = c(1, 3),
                         breaks = c(1, 1.5, 2, 2.5, 3)) +
      coord_cartesian(expand = F) +
      scale_x_reverse()
    
    dens_susc_plot = 
      ggplot() +
      geom_density(data = species_vulnerability_df,
                   aes(y = mean_susceptibility),
                   fill = 'grey', color = 'grey') +
      theme_void() +
      coord_cartesian(expand = F) +
      scale_y_continuous(limits = c(1, 3),
                         breaks = c(1, 1.5, 2, 2.5, 3))
    
    psa_main = 
      ((dens_prod_plot / prod_susc_plot) + plot_layout(heights = c(0.15, 1)) | 
         (plot_spacer() / dens_susc_plot) + plot_layout(heights = c(0.15, 1))) +
      plot_layout(widths = c(1, 0.15))
    
    hist_plot =
      ggplot() +
      geom_histogram(data = species_vulnerability_df,
                     aes(x = mean_vulnerability,
                         fill = vul_category),
                     binwidth = 0.03) +
      scale_fill_manual(values = c('greenyellow', 'orange', 'red2'),
                        breaks = c('Low', 'Moderate', 'High')) +
      labs(x = 'Vulnerability', y = 'No. species', fill = '') +
      theme_bw() +
      theme(axis.text = element_text(color = "black")) +
      theme(legend.position = 'bottom') +
      scale_x_continuous(limits = c(1, 3),
                         breaks = c(1, 1.5, 2, 2.5, 3))
    
    plots_psa = psa_main / (hist_plot + plot_spacer() + plot_layout(widths = c(1, 0.2))) + 
      plot_layout(heights = c(1, 0.5))
    
    psa_plot(plots_psa)
    
    # Switch to results tab
    updateTabsetPanel(session, "main_tabs", selected = "Results Table")
  })
  
  # Display results table
  output$results_table = renderDT({
    req(analysis_results())
    datatable(analysis_results(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Download results
  output$download_results = downloadHandler(
    filename = function() {
      paste0("psa_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(analysis_results(), file, row.names = FALSE)
    }
  )
  
  # Display plot
  output$psa_plot = renderPlot({
    req(psa_plot())
    psa_plot()
  })
  
  # Download plot
  output$download_plot = downloadHandler(
    filename = function() {
      paste0("psa_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = psa_plot(), dpi = 600, 
             height = 40/5.5, width = 25/5.5)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)