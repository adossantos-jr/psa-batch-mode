if (!require("remotes")) install.packages("remotes")
if (!require("FishLife")) remotes::install_github("James-Thorson-NOAA/James-Thorson-NOAA/FishLife")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, MASS, stringr, dplyr, ggplot2, ggrepel, patchwork, FishLife, DT, tidyr)

# --- Define Attribute Custom Lists (Aligned with User Inputs) ---
prod_attrs = c("r", "tmax", "lmax", "k", "m", "fec", "breed", "rec", "tmat", "troph")
susc_attrs = c("area_over", "geog_conc", "vert_over", "seas_migr", "school", "morph", "desire", "mng_strat", "f_over_m", "b_over_b0", "surv_prob", "hab_impact")
all_attributes = c(prod_attrs, susc_attrs)

# Maps FishLife variables to the user's specific attribute naming conventions
fl_map = c(r = "r", k = "K", m = "M", tmax = "tmax", lmax = "Loo", tmat = "tm")
fl_sub_attrs = names(fl_map)

# --- Directional Logic ---
high_val_is_high_cat = c(
  r = TRUE, tmax = FALSE, lmax = FALSE, k = TRUE, m = TRUE, fec = TRUE, 
  breed = FALSE, rec = NA, tmat = TRUE, troph = FALSE,
  area_over = TRUE, geog_conc = NA, vert_over = NA, seas_migr = NA, 
  school = NA, morph = NA, desire = NA, mng_strat = NA, 
  f_over_m = TRUE, b_over_b0 = FALSE, surv_prob = FALSE, hab_impact = NA
)

# Baseline Layout Defaults
threshold_defaults = list(
  r = c(0.16, 0.50), tmax = c(10, 30), lmax = c(60, 150), k = c(0.15, 0.25), m = c(0.20, 0.40),
  fec = c(1000, 100000), breed = c(1, 3), rec = c(1, 2), tmat = c(2, 4), troph = c(2.5, 3.5),
  area_over = c(0.25, 0.50), geog_conc = c(1, 2), vert_over = c(1, 2), seas_migr = c(1, 2),
  school = c(1, 2), morph = c(1, 2), desire = c(1, 2), mng_strat = c(1, 2),
  f_over_m = c(0.5, 1.0), b_over_b0 = c(0.25, 0.40), surv_prob = c(0.33, 0.67), hab_impact = c(1, 2)
)

create_empty_species_list = function(species_names) {
  lapply(setNames(as.list(species_names), species_names), function(sp) {
    # Added "source" column to track FishLife vs CSV vs Manual scoring
    data.frame(attribute = all_attributes, low = 0, mod = 0, high = 0, weight = 1, source = "None", stringsAsFactors = FALSE)
  })
}

vul_class = function(x) ifelse(x >= 2.2, "High", ifelse(x > 1.8, "Moderate", "Low"))

fetch_fishlife_single = function(sciname, n_samples = 5000) {
  col_names = c("Loo","K","Winfinity","tmax","tm","M","Lm","Temperature",
                "ln_var","rho","ln_MASPS","ln_margsd","h","logitbound_h",
                "ln_Fmsy_over_M","ln_Fmsy","ln_r","r","ln_G","G")
  back_transform = function(v) c(exp(v[1:7]), v[8], v[9:20])
  
  genus = word(sciname, 1)
  sp    = word(sciname, 2, length(strsplit(sciname, " ")[[1]]))
  sp    = ifelse(is.na(sp) || sp == "spp" || sp == "", "predictive", sp)
  
  res = try(FishLife::Search_species(Genus = genus, Species = sp), silent = TRUE)
  if (inherits(res, "try-error")) return(NULL)
  spp = try(FishLife::Plot_taxa(res$match_taxonomy), silent = TRUE)
  if (inherits(spp, "try-error")) return(NULL)
  
  pred    = spp[[1]]
  samples = as.data.frame(t(apply(MASS::mvrnorm(n_samples, pred$Mean_pred, pred$Cov_pred), 1, back_transform)))
  colnames(samples) = col_names
  list(samples = samples, matched_taxon = paste(res$match_taxonomy, collapse = " > "))
}

calculate_single_probs = function(s, t_inputs, high_cat_dir) {
  res_list = list()
  for (attr in names(fl_map)) {
    fl_col = fl_map[[attr]]
    vals = s[[fl_col]]
    t1 = t_inputs[[attr]]$t1
    t2 = t_inputs[[attr]]$t2
    is_high_cat = high_cat_dir[[attr]]
    
    if (!is.null(vals) && !is.null(t1) && !is.null(t2)) {
      if (is_high_cat) {
        cats = ifelse(vals > t2, "high", ifelse(vals < t1, "low", "mod"))
      } else {
        cats = ifelse(vals < t1, "high", ifelse(vals > t2, "low", "mod"))
      }
      res_list[[attr]] = list(
        low  = mean(cats == "low", na.rm = TRUE),
        mod  = mean(cats == "mod", na.rm = TRUE),
        high = mean(cats == "high", na.rm = TRUE)
      )
    }
  }
  return(res_list)
}

df_col = {
  xc = seq(0, 1, length.out = 200); yc = seq(0, 1, length.out = 200)
  g  = cbind(expand.grid(xcolor = xc, ycolor = yc), expand.grid(x = seq(3, 1, length.out = 200), y = seq(1, 3, length.out = 200)))
  g$zcolor = g$xcolor^2 + g$ycolor^2
  g
}

ui = fluidPage(
  titlePanel("PSA in batch mode"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_csv", "Choose CSV file", accept = ".csv"),
      hr(),
      tags$h4(tags$b("Step 1: Set attribute thresholds"), style = "color: #337ab7; margin-bottom: 12px;"),
      tags$p("Define thresholds (Low to Moderate / Moderate to High) for attributes:", style = "font-size: 12px; color: #666;"),
      
      tags$div(
        style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #fafafa; border-radius: 4px; margin-bottom: 15px;",
        lapply(all_attributes, function(attr) {
          defs = threshold_defaults[[attr]]
          fluidRow(
            style = "margin-bottom: 8px; padding-bottom: 4px; border-bottom: 1px dashed #eee;",
            column(4, tags$b(attr, style = "font-size: 13px; display: block; margin-top: 10px;")),
            column(4, style = 'padding:2px;', numericInput(paste0("g_t_", attr, "_1"), "Low to Moderate", value = defs[1])),
            column(4, style = 'padding:2px;', numericInput(paste0("g_t_", attr, "_2"), "Moderate to High", value = defs[2]))
          )
        })
      ),
      hr(),
      actionButton("run_analysis", "Run PSA", class = "btn-success", style = "width:100%; font-size: 14px; font-weight: bold; padding: 12px;")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Configuration",
                 tags$br(),
                 wellPanel(
                   style = "border: 2px solid #f0ad4e;",
                   tags$h4(tags$b("Step 2: Batch score options"), style = "color: #f0ad4e;"),
                   tags$p("CSV attributes map automatically upon upload."),
                   actionButton("bulk_score_fl", "Score life-history traits via FishLife (all species)", class = "btn-warning", style = "width:100%; font-weight:bold;")
                 ),
                 uiOutput("step3_panel")
        ),
        tabPanel("PSA Plots", 
                 tags$br(),
                 downloadButton("dl_psa_plot", "Download PSA Plot (PNG)"),
                 tags$br(), tags$br(),
                 plotOutput("psa_plot", height = "600px"),
                 tags$hr(),
                 downloadButton("dl_density_plot", "Download Prod. Density Plot (PNG)"),
                 tags$br(), tags$br(),
                 plotOutput("density_plot", height = "400px")),
        tabPanel("Category Proportions Plot",
                 tags$br(),
                 downloadButton("dl_proportions_plot", "Download Proportions Plot (PNG)"),
                 tags$br(), tags$br(),
                 plotOutput("proportions_plot", height = "500px")),
        tabPanel("Results Summary", 
                 tags$br(),
                 downloadButton("dl_results_table", "Download CSV"),
                 tags$br(), tags$br(),
                 DTOutput("results_table")),
        tabPanel("Vulnerability Outcomes", 
                 tags$br(),
                 downloadButton("dl_probs_table", "Download CSV"),
                 tags$br(), tags$br(),
                 DTOutput("probs_table")),
        tabPanel("Attribute Probability Table", 
                 tags$br(),
                 downloadButton("dl_fishlife_matrix", "Download CSV"),
                 tags$br(), tags$br(),
                 DTOutput("fishlife_matrix"))
      )
    )
  )
)

server = function(input, output, session) {
  
  reactive_data = reactiveValues(
    df = NULL, 
    species_list = NULL, 
    matched_taxa = list(), 
    psa_results = NULL,
    initialized = FALSE
  )
  
  # Live probability auto-balancer logic
  lapply(all_attributes, function(attr) {
    observeEvent(input[[paste0("tw_low_", attr)]], {
      val = input[[paste0("tw_low_", attr)]]
      if (is.null(val) || is.na(val)) return()
      if (val > 1) val = 1; if (val < 0) val = 0
      
      mod_val = isolate(input[[paste0("tw_mod_", attr)]])
      high_val = isolate(input[[paste0("tw_high_", attr)]])
      if (is.null(mod_val) || is.na(mod_val)) mod_val = 0
      if (is.null(high_val) || is.na(high_val)) high_val = 0
      
      if (abs(val + mod_val + high_val - 1) > 0.01) {
        rem = 1 - val
        if (mod_val + high_val > 0) {
          new_mod = rem * (mod_val / (mod_val + high_val))
          new_high = rem * (high_val / (mod_val + high_val))
        } else {
          new_mod = rem / 2
          new_high = rem / 2
        }
        updateNumericInput(session, paste0("tw_mod_", attr), value = round(new_mod, 3))
        updateNumericInput(session, paste0("tw_high_", attr), value = round(new_high, 3))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0("tw_mod_", attr)]], {
      val = input[[paste0("tw_mod_", attr)]]
      if (is.null(val) || is.na(val)) return()
      if (val > 1) val = 1; if (val < 0) val = 0
      
      low_val = isolate(input[[paste0("tw_low_", attr)]])
      high_val = isolate(input[[paste0("tw_high_", attr)]])
      if (is.null(low_val) || is.na(low_val)) low_val = 0
      if (is.null(high_val) || is.na(high_val)) high_val = 0
      
      if (abs(val + low_val + high_val - 1) > 0.01) {
        rem = 1 - val
        if (low_val + high_val > 0) {
          new_low = rem * (low_val / (low_val + high_val))
          new_high = rem * (high_val / (low_val + high_val))
        } else {
          new_low = rem / 2
          new_high = rem / 2
        }
        updateNumericInput(session, paste0("tw_low_", attr), value = round(new_low, 3))
        updateNumericInput(session, paste0("tw_high_", attr), value = round(new_high, 3))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0("tw_high_", attr)]], {
      val = input[[paste0("tw_high_", attr)]]
      if (is.null(val) || is.na(val)) return()
      if (val > 1) val = 1; if (val < 0) val = 0
      
      low_val = isolate(input[[paste0("tw_low_", attr)]])
      mod_val = isolate(input[[paste0("tw_mod_", attr)]])
      if (is.null(low_val) || is.na(low_val)) low_val = 0
      if (is.null(mod_val) || is.na(mod_val)) mod_val = 0
      
      if (abs(val + low_val + mod_val - 1) > 0.01) {
        rem = 1 - val
        if (low_val + mod_val > 0) {
          new_low = rem * (low_val / (low_val + mod_val))
          new_mod = rem * (mod_val / (low_val + mod_val))
        } else {
          new_low = rem / 2
          new_mod = rem / 2
        }
        updateNumericInput(session, paste0("tw_low_", attr), value = round(new_low, 3))
        updateNumericInput(session, paste0("tw_mod_", attr), value = round(new_mod, 3))
      }
    }, ignoreInit = TRUE)
  })
  
  # Master CSV Extraction function handling BOTH numerics and defined character mappings
  score_single_attr_from_csv = function(sp, attr) {
    req(reactive_data$df)
    row_data = reactive_data$df[reactive_data$df$species == sp, ][1, ]
    idx = which(reactive_data$species_list[[sp]]$attribute == attr)
    
    if (length(idx) > 0 && attr %in% names(row_data)) {
      val_raw = row_data[[attr]]
      
      # 1. Categorical String Assignment Logic
      val_str = tolower(trimws(as.character(val_raw)))
      is_categorical = val_str %in% c("low", "decrease_fish", "low_selec", "lowfreq", "proactive",
                                      "mod", "no_effect", "mod_select", "mod_selec", "modfreq", "reactive", "moderate",
                                      "high", "increase_fish", "high_selec", "highfreq", "no_strat")
      
      if (is_categorical) {
        if (val_str %in% c("low", "decrease_fish", "low_selec", "lowfreq", "proactive")) {
          mag = "low"
        } else if (val_str %in% c("mod", "no_effect", "mod_select", "mod_selec", "modfreq", "reactive", "moderate")) {
          mag = "mod"
        } else {
          mag = "high"
        }
        reactive_data$species_list[[sp]]$low[idx]  = ifelse(mag == "low", 1, 0)
        reactive_data$species_list[[sp]]$mod[idx]  = ifelse(mag == "mod", 1, 0)
        reactive_data$species_list[[sp]]$high[idx] = ifelse(mag == "high", 1, 0)
        reactive_data$species_list[[sp]]$source[idx] = "CSV"
        
      } else {
        # 2. Numerical Threshold Check Logic
        val = suppressWarnings(as.numeric(val_raw))
        t1 = input[[paste0("g_t_", attr, "_1")]]
        t2 = input[[paste0("g_t_", attr, "_2")]]
        is_high_cat = high_val_is_high_cat[[attr]]
        
        if (!is.na(val) && !is.null(t1) && !is.null(t2) && !is.na(is_high_cat)) {
          if (is_high_cat) {
            mag = ifelse(val > t2, "high", ifelse(val < t1, "low", "mod"))
          } else {
            mag = ifelse(val < t1, "high", ifelse(val > t2, "low", "mod"))
          }
          reactive_data$species_list[[sp]]$low[idx]  = ifelse(mag == "low", 1, 0)
          reactive_data$species_list[[sp]]$mod[idx]  = ifelse(mag == "mod", 1, 0)
          reactive_data$species_list[[sp]]$high[idx] = ifelse(mag == "high", 1, 0)
          reactive_data$species_list[[sp]]$source[idx] = "CSV"
        } else {
          reactive_data$species_list[[sp]]$low[idx]  = 0
          reactive_data$species_list[[sp]]$mod[idx]  = 0
          reactive_data$species_list[[sp]]$high[idx] = 0
          reactive_data$species_list[[sp]]$source[idx] = "CSV"
        }
      }
    }
  }
  
  observeEvent(input$file_csv, {
    req(input$file_csv)
    df_raw = read.csv(input$file_csv$datapath, stringsAsFactors = FALSE)
    reactive_data$df = df_raw
    sp_names = unique(df_raw$species)
    reactive_data$species_list = create_empty_species_list(sp_names)
    reactive_data$initialized = TRUE
    
    for (sp in sp_names) {
      for (attr in all_attributes) {
        score_single_attr_from_csv(sp, attr)
      }
    }
    showNotification("Done!", type = "message")
  })
  
  get_threshold_inputs = function() {
    t_inputs = list()
    for(attr in all_attributes) {
      t_inputs[[attr]] = list(
        t1 = input[[paste0("g_t_", attr, "_1")]],
        t2 = input[[paste0("g_t_", attr, "_2")]]
      )
    }
    t_inputs
  }
  
  observeEvent(input$bulk_score_fl, {
    req(reactive_data$df)
    sp_names = unique(reactive_data$df$species)
    t_inputs = get_threshold_inputs()
    
    failed_species = c()
    withProgress(message = 'Scoring via FishLife...', value = 0, {
      for (i in seq_along(sp_names)) {
        sp = sp_names[i]
        setProgress(value = i / length(sp_names), detail = paste("querying:", sp))
        
        res = fetch_fishlife_single(sp)
        if (is.null(res)) {
          failed_species = c(failed_species, sp)
          next
        }
        
        probs = calculate_single_probs(res$samples, t_inputs, high_val_is_high_cat)
        reactive_data$matched_taxa[[sp]] = res$matched_taxon
        
        for (attr in names(probs)) {
          idx = which(reactive_data$species_list[[sp]]$attribute == attr)
          if (length(idx) > 0) {
            reactive_data$species_list[[sp]]$low[idx]  = round(probs[[attr]]$low, 4)
            reactive_data$species_list[[sp]]$mod[idx]  = round(probs[[attr]]$mod, 4)
            reactive_data$species_list[[sp]]$high[idx] = round(probs[[attr]]$high, 4)
            reactive_data$species_list[[sp]]$source[idx] = "FishLife"
          }
        }
      }
    })
    if (length(failed_species) > 0) {
      showNotification(paste("Omitted variants:", paste(failed_species, collapse = ", ")), type = "warning")
    } else {
      showNotification("Successfully scored all species via FishLife.", type = "message")
    }
  })
  
  observeEvent(input$fetch_full_species_fl, {
    req(input$tweak_sp_selector)
    sp = input$tweak_sp_selector
    t_inputs = get_threshold_inputs()
    
    res = fetch_fishlife_single(sp)
    if(is.null(res)) {
      showNotification(paste("FishLife taxon matching failed: try another name.", sp), type = "error")
      return()
    }
    
    probs = calculate_single_probs(res$samples, t_inputs, high_val_is_high_cat)
    reactive_data$matched_taxa[[sp]] = res$matched_taxon
    
    for (attr in all_attributes) {
      if (attr %in% fl_sub_attrs && attr %in% names(probs)) {
        idx = which(reactive_data$species_list[[sp]]$attribute == attr)
        if (length(idx) > 0) {
          reactive_data$species_list[[sp]]$low[idx]  = round(probs[[attr]]$low, 4)
          reactive_data$species_list[[sp]]$mod[idx]  = round(probs[[attr]]$mod, 4)
          reactive_data$species_list[[sp]]$high[idx] = round(probs[[attr]]$high, 4)
          reactive_data$species_list[[sp]]$source[idx] = "FishLife"
        }
      } else {
        score_single_attr_from_csv(sp, attr)
      }
    }
    showNotification(paste("Scored attributes via FishLife:", sp), type = "message")
  })
  
  observeEvent(input$reset_species_csv_btn, {
    req(input$tweak_sp_selector)
    sp = input$tweak_sp_selector
    for (attr in all_attributes) {
      score_single_attr_from_csv(sp, attr)
    }
    showNotification(paste("Successfully re-scored all attributes from CSV data for:", sp), type = "message")
  })
  
  lapply(all_attributes, function(target_attr) {
    observeEvent(input[[paste0("fetch_single_attr_", target_attr)]], {
      req(input$tweak_sp_selector)
      sp = input$tweak_sp_selector
      t_inputs = get_threshold_inputs()
      
      if (target_attr %in% fl_sub_attrs) {
        res = fetch_fishlife_single(sp)
        if(is.null(res)) {
          showNotification("Taxonomy resolved empty for targeted metric", type = "error")
          return()
        }
        probs = calculate_single_probs(res$samples, t_inputs, high_val_is_high_cat)
        reactive_data$matched_taxa[[sp]] = res$matched_taxon
        
        if (target_attr %in% names(probs)) {
          idx = which(reactive_data$species_list[[sp]]$attribute == target_attr)
          if (length(idx) > 0) {
            reactive_data$species_list[[sp]]$low[idx]  = round(probs[[target_attr]]$low, 4)
            reactive_data$species_list[[sp]]$mod[idx]  = round(probs[[target_attr]]$mod, 4)
            reactive_data$species_list[[sp]]$high[idx] = round(probs[[target_attr]]$high, 4)
            reactive_data$species_list[[sp]]$source[idx] = "FishLife"
            showNotification(paste("Scored [", target_attr, "] via FishLife."), type = "message")
          }
        }
      } else {
        score_single_attr_from_csv(sp, target_attr)
        showNotification(paste("Re-scored [", target_attr, "] through CSV data."), type = "message")
      }
    })
  })
  
  output$step3_panel = renderUI({
    if(!reactive_data$initialized) {
      return(wellPanel(tags$em("Modify specific data")))
    }
    wellPanel(
      style = "border: 2px solid #5cb85c;",
      tags$h4(tags$b("Step 3: Fine-tune / tweak single species"), style = "color: #5cb85c;"),
      tags$p("Manually tweak attributes"),
      
      fluidRow(
        column(4, selectInput("tweak_sp_selector", "Target scope:", choices = names(reactive_data$species_list))),
        column(4, style = "margin-top: 25px;", actionButton("fetch_full_species_fl", "Score via FishLife", class = "btn-info", style="width:100%; font-weight:bold;")),
        column(4, style = "margin-top: 25px;", actionButton("reset_species_csv_btn", "Re-score with CSV data", class = "btn-warning", style="width:100%; font-weight:bold;"))
      ),
      hr(),
      uiOutput("individual_tweak_grid"),
      tags$br(),
      actionButton("save_tweaks_btn", "Save modifications", class = "btn-success", style = "width:100%; font-weight:bold;")
    )
  })
  
  output$individual_tweak_grid = renderUI({
    req(input$tweak_sp_selector)
    sp = input$tweak_sp_selector
    sp_df = reactive_data$species_list[[sp]]
    
    tagList(
      fluidRow(
        style = "font-weight: bold; border-bottom: 2px solid #ccc; padding-bottom: 5px; margin-bottom: 10px;",
        column(3, "Attribute Identity"), column(2, "Low"), column(2, "Moderate"), column(2, "High"), column(2, "Weight"), column(1, "")
      ),
      tags$div(
        style = "max-height: 380px; overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
        lapply(all_attributes, function(attr) {
          row_data = sp_df[sp_df$attribute == attr, ]
          if(nrow(row_data) == 0) row_data = data.frame(low=0, mod=0, high=0, weight=1, source="None")
          
          btn_label = if(attr %in% fl_sub_attrs) paste0("Fetch ", attr) else "Reset to CSV"
          fl_action_btn = actionLink(paste0("fetch_single_attr_", attr), btn_label, style = "font-size:11px; font-weight:bold; display:block; margin-top:8px;")
          
          fluidRow(
            style = "margin-bottom: 4px; padding-bottom: 4px; border-bottom: 1px solid #f1f1f1;",
            column(3, tags$b(attr, style = "display:block; margin-top:6px;")),
            column(2, style='padding:2px;', numericInput(paste0("tw_low_", attr), NULL, value = row_data$low, min = 0, max = 1, step = 0.01)),
            column(2, style='padding:2px;', numericInput(paste0("tw_mod_", attr), NULL, value = row_data$mod, min = 0, max = 1, step = 0.01)),
            column(2, style='padding:2px;', numericInput(paste0("tw_high_", attr), NULL, value = row_data$high, min = 0, max = 1, step = 0.01)),
            column(2, style='padding:2px;', numericInput(paste0("tw_w_", attr), NULL, value = row_data$weight, min = 0, step = 0.5)),
            column(1, style='padding:2px;', fl_action_btn)
          )
        })
      )
    )
  })
  
  observeEvent(input$save_tweaks_btn, {
    req(input$tweak_sp_selector)
    sp = input$tweak_sp_selector
    
    for (attr in all_attributes) {
      idx = which(reactive_data$species_list[[sp]]$attribute == attr)
      l_val = input[[paste0("tw_low_", attr)]]
      m_val = input[[paste0("tw_mod_", attr)]]
      h_val = input[[paste0("tw_high_", attr)]]
      w_val = input[[paste0("tw_w_", attr)]]
      
      if(!is.null(l_val)) reactive_data$species_list[[sp]]$low[idx]  = l_val
      if(!is.null(m_val)) reactive_data$species_list[[sp]]$mod[idx]  = m_val
      if(!is.null(h_val)) reactive_data$species_list[[sp]]$high[idx] = h_val
      if(!is.null(w_val)) reactive_data$species_list[[sp]]$weight[idx] = w_val
      
      # Flag it as manually tweaked
      reactive_data$species_list[[sp]]$source[idx] = "Manual"
    }
    showNotification(paste("Modification saved for:", sp), type = "message")
  })
  
  # --- MONTE CARLO ENGINE EXECUTED FORCEFULLY UPON BUTTON CLICK ---
  observeEvent(input$run_analysis, {
    req(reactive_data$species_list)
    num_samples = 999
    sp_list = reactive_data$species_list
    
    for (sp in names(sp_list)) {
      sp_list[[sp]]$weight[sp_list[[sp]]$attribute == "r"] = 2
    }
    
    vulnerability_scores = list()
    sp_names = names(sp_list)
    
    withProgress(message = "Running Monte Carlo simulation...", value = 0, {
      for (idx_sp in seq_along(sp_names)) {
        species = sp_names[idx_sp]
        incProgress(1 / length(sp_names), detail = paste("Simulating species:", species))
        
        d = sp_list[[species]]
        d[, c("low","mod","high")] = lapply(d[, c("low","mod","high")], as.numeric)
        
        prod_rows = which(d$attribute %in% prod_attrs)
        susc_rows = which(d$attribute %in% susc_attrs)
        
        prod_vals_list = list()
        prod_weights = c()
        for(i in prod_rows) {
          p = as.numeric(d[i, c("high","mod","low")])
          p[is.na(p)] = 0
          w = d$weight[i]
          if (!is.na(w) && w > 0 && sum(p) > 0) {
            p = p / sum(p)
            prod_vals_list[[length(prod_vals_list) + 1]] = w * sample(c(3,2,1), num_samples, replace=TRUE, prob=p)
            prod_weights = c(prod_weights, w)
          }
        }
        
        susc_vals_list = list()
        susc_weights = c()
        for(i in susc_rows) {
          p = as.numeric(d[i, c("high","mod","low")])
          p[is.na(p)] = 0
          w = d$weight[i]
          if (!is.na(w) && w > 0 && sum(p) > 0) {
            p = p / sum(p)
            susc_vals_list[[length(susc_vals_list) + 1]] = w * sample(c(3,2,1), num_samples, replace=TRUE, prob=p)
            susc_weights = c(susc_weights, w)
          }
        }
        
        if (length(prod_vals_list) > 0) {
          prod_scores = rowSums(do.call(cbind, prod_vals_list)) / sum(prod_weights)
        } else {
          prod_scores = rep(2, num_samples)
        }
        
        if (length(susc_vals_list) > 0) {
          susc_scores = rowSums(do.call(cbind, susc_vals_list)) / sum(susc_weights)
        } else {
          susc_scores = rep(2, num_samples)
        }
        
        vuln = sqrt((3 - prod_scores)^2 + (susc_scores - 1)^2)
        vulnerability_scores[[species]] = list(productivity = prod_scores, susceptibility = susc_scores, vulnerability = vuln)
      }
      
      results_df = do.call(rbind, lapply(names(vulnerability_scores), function(sp) {
        v = vulnerability_scores[[sp]]
        taxon = reactive_data$matched_taxa[[sp]]
        
        # Calculate precise FishLife scoring status based on tracking matrix
        fl_count = sum(sp_list[[sp]]$source[sp_list[[sp]]$attribute %in% fl_sub_attrs] == "fishlife_score")
        fl_status = "no_fishlife"
        if (fl_count == length(fl_sub_attrs)) {
          fl_status = "full_fishlife"
        } else if (fl_count > 0) {
          fl_status = "partial_fishlife"
        }
        
        data.frame(
          species             = sp,
          matched_taxon       = if(is.null(taxon)) NA else taxon,
          has_fishlife        = if(is.null(taxon)) FALSE else TRUE,
          fishlife_scoring_status = fl_status,
          mean_productivity   = mean(v$productivity),
          lci_productivity    = quantile(v$productivity,  0.025),
          uci_productivity    = quantile(v$productivity,  0.975),
          sd_productivity     = sd(v$productivity),
          mean_susceptibility = mean(v$susceptibility),
          lci_susceptibility  = quantile(v$susceptibility, 0.025),
          uci_susceptibility  = quantile(v$susceptibility, 0.975),
          sd_susceptibility   = sd(v$susceptibility),
          mean_vulnerability  = mean(v$vulnerability),
          lci_vulnerability   = quantile(v$vulnerability,  0.025),
          uci_vulnerability   = quantile(v$vulnerability,  0.975),
          sd_vulnerability    = sd(v$vulnerability),
          stringsAsFactors    = FALSE
        )
      }))
      results_df$vul_category = vul_class(results_df$mean_vulnerability)
      
      probs_df = do.call(rbind, lapply(names(vulnerability_scores), function(sp) {
        v = vulnerability_scores[[sp]]
        cats = sapply(v$vulnerability, vul_class)
        data.frame(
          species = sp,
          probability_low_vulnerability = round(mean(cats == "Low"), 4),
          probability_moderate_vulnerability = round(mean(cats == "Moderate"), 4),
          probability_high_vulnerability = round(mean(cats == "High"), 4),
          stringsAsFactors = FALSE
        )
      }))
      
      fishlife_scores_df = do.call(rbind, lapply(names(sp_list), function(sp) {
        d = sp_list[[sp]]
        sub_d = d[d$attribute %in% fl_sub_attrs, ]
        if(nrow(sub_d) == 0) return(NULL)
        data.frame(
          species = sp, attribute = sub_d$attribute,
          prob_low = sub_d$low, prob_mod = sub_d$mod, prob_high = sub_d$high,
          expected_score = sub_d$low * 1 + sub_d$mod * 2 + sub_d$high * 3,
          source_type = sub_d$source, row.names = NULL
        )
      }))
      
      prod_density_df = do.call(rbind, lapply(names(vulnerability_scores), function(sp)
        data.frame(species = sp, productivity = vulnerability_scores[[sp]]$productivity)
      ))
      
      # Assign fully completed data structures to global reactive list
      reactive_data$psa_results = list(
        results_df = results_df, 
        probs_df = probs_df, 
        fishlife_scores_df = fishlife_scores_df, 
        prod_density_df = prod_density_df
      )
    })
    
    # After calculating, forcefully jump user screen to the PSA Plots tab
    updateTabsetPanel(session, "main_tabs", selected = "PSA Plots")
    showNotification("Done!", type = "message")
  })
  
  # --- GRAPH RENDERING EXTRACTED INTO REACTIVES ---
  build_psa_plot <- reactive({
    req(reactive_data$psa_results)
    res = reactive_data$psa_results
    results_df = res$results_df
    
    prod_susc_plot = ggplot() +
      geom_tile(data = df_col, aes(x, y, fill = zcolor)) +
      geom_linerange(data = results_df, aes(y = mean_susceptibility, x = mean_productivity, ymin = lci_susceptibility, ymax = uci_susceptibility), alpha = 0.4, linewidth = 1) +
      geom_linerange(data = results_df, aes(y = mean_susceptibility, x = mean_productivity, xmin = uci_productivity, xmax = lci_productivity), alpha = 0.4, linewidth = 1) +
      geom_point(data = results_df, aes(y = mean_susceptibility, x = mean_productivity), alpha = 0.7, size = 2) +
      geom_text_repel(data = results_df, aes(label = species, y = mean_susceptibility, x = mean_productivity), fontface = "italic", force = 50, size = 2.3) +
      scale_fill_gradientn(colors = c("forestgreen","green3","green2","greenyellow","orange3","red","red2","red3","red4"), guide = "none") +
      labs(x = "Productivity", y = "Susceptibility") +
      xlim(1, 3) + ylim(1, 3) + scale_x_reverse() +
      coord_cartesian(expand = FALSE) + theme_test() + theme(axis.text = element_text(color = "black"), legend.position = "bottom")
    
    dens_prod_plot = ggplot() +
      geom_density(data = results_df, aes(x = mean_productivity), fill = "grey", color = "grey") +
      scale_x_continuous(limits = c(1, 3)) + scale_x_reverse() + coord_cartesian(expand = FALSE) + theme_void()
    
    dens_susc_plot = ggplot() +
      geom_density(data = results_df, aes(y = mean_susceptibility), fill = "grey", color = "grey") +
      scale_y_continuous(limits = c(1, 3)) + coord_cartesian(expand = FALSE) + theme_void()
    
    psa_main = ((dens_prod_plot / prod_susc_plot) + plot_layout(heights = c(0.15, 1)) | (plot_spacer() / dens_susc_plot) + plot_layout(heights = c(0.15, 1))) + plot_layout(widths = c(1, 0.15))
    
    hist_plot = ggplot() +
      geom_histogram(data = results_df, aes(x = mean_vulnerability, fill = vul_category), binwidth = 0.03) +
      scale_fill_manual(values = c("greenyellow","orange","red2"), breaks = c("Low","Moderate","High")) +
      labs(x = "Vulnerability", y = "No. species", fill = "") +
      scale_x_continuous(limits = c(1, 3), breaks = seq(1, 3, 0.5)) +
      theme_bw() + theme(axis.text = element_text(color = "black"), legend.position = "bottom")
    
    final_plot = psa_main / (hist_plot + plot_spacer() + plot_layout(widths = c(1, 0.2))) + plot_layout(heights = c(1, 0.5))
    return(final_plot)
  })
  
  build_density_plot <- reactive({
    req(reactive_data$psa_results)
    res = reactive_data$psa_results
    final_plot = ggplot(res$prod_density_df, aes(x = productivity, fill = species, color = species)) +
      geom_density(alpha = 0.35, linewidth = 0.7) +
      geom_vline(data = res$results_df, aes(xintercept = mean_productivity, color = species), linetype = "dashed", linewidth = 0.6) +
      labs(x = "Productivity score", y = "Density", fill = "", color = "") +
      scale_x_continuous(limits = c(1, 3), breaks = seq(1, 3, 0.5)) +
      theme_bw() + theme(axis.text = element_text(color = "black"), legend.text = element_text(face = "italic"), legend.position = "bottom")
    return(final_plot)
  })
  
  build_proportions_plot <- reactive({
    req(reactive_data$psa_results)
    raw_probs = reactive_data$psa_results$probs_df
    res_df = reactive_data$psa_results$results_df
    
    ordered_species = res_df[order(res_df$mean_vulnerability), "species"]
    
    plot_data = raw_probs %>%
      pivot_longer(
        cols = c(probability_low_vulnerability, probability_moderate_vulnerability, probability_high_vulnerability),
        names_to = "vul_category",
        values_to = "proportion"
      ) %>%
      mutate(
        vul_category = case_when(
          vul_category == "probability_low_vulnerability" ~ "Low",
          vul_category == "probability_moderate_vulnerability" ~ "Moderate",
          vul_category == "probability_high_vulnerability" ~ "High"
        ),
        vul_category = factor(vul_category, levels = c("Low", "Moderate", "High")),
        species = factor(species, levels = ordered_species)
      )
    
    final_plot = ggplot(plot_data, aes(x = species, y = proportion, fill = vul_category)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = c(Low = "greenyellow", Moderate = "orange", High = "red2"), name = "Vulnerability") +
      labs(x = "Species by vulnerability", y = "Proportion of bootstrap samples") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", color = "black"),
        axis.text.y = element_text(color = "black")
      )
    return(final_plot)
  })
  
  # Render Plots
  output$psa_plot = renderPlot({ build_psa_plot() })
  output$density_plot = renderPlot({ build_density_plot() })
  output$proportions_plot = renderPlot({ build_proportions_plot() })
  
  # Render Tables
  output$results_table = renderDT({
    req(reactive_data$psa_results)
    datatable(reactive_data$psa_results$results_df, rownames = FALSE)
  })
  
  output$probs_table = renderDT({
    req(reactive_data$psa_results)
    datatable(reactive_data$psa_results$probs_df, rownames = FALSE)
  })
  
  output$fishlife_matrix = renderDT({
    req(reactive_data$psa_results)
    datatable(reactive_data$psa_results$fishlife_scores_df, rownames = FALSE)
  })
  
  # --- DOWNLOAD HANDLERS ---
  
  output$dl_psa_plot <- downloadHandler(
    filename = function() { paste0("PSA_Plot_", Sys.Date(), ".png") },
    content = function(file) {
      # Made the height slightly taller as requested
      ggsave(file, plot = build_psa_plot(), width = 7, height = 10, dpi = 300)
    }
  )
  
  output$dl_density_plot <- downloadHandler(
    filename = function() { paste0("Prod_Density_Plot_", Sys.Date(), ".png") },
    content = function(file) {
      ggsave(file, plot = build_density_plot(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$dl_proportions_plot <- downloadHandler(
    filename = function() { paste0("Proportions_Plot_", Sys.Date(), ".png") },
    content = function(file) {
      ggsave(file, plot = build_proportions_plot(), width = 10, height = 7, dpi = 300)
    }
  )
  
  output$dl_results_table <- downloadHandler(
    filename = function() { paste0("Results_Summary_", Sys.Date(), ".csv") },
    content = function(file) {
      req(reactive_data$psa_results)
      write.csv(reactive_data$psa_results$results_df, file, row.names = FALSE)
    }
  )
  
  output$dl_probs_table <- downloadHandler(
    filename = function() { paste0("Vulnerability_Outcomes_", Sys.Date(), ".csv") },
    content = function(file) {
      req(reactive_data$psa_results)
      write.csv(reactive_data$psa_results$probs_df, file, row.names = FALSE)
    }
  )
  
  output$dl_fishlife_matrix <- downloadHandler(
    filename = function() { paste0("Attribute_Probability_Table_", Sys.Date(), ".csv") },
    content = function(file) {
      req(reactive_data$psa_results)
      write.csv(reactive_data$psa_results$fishlife_scores_df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)