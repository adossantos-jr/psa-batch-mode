# This is a shiny app to run PSA (Productivity-Susceptibility Analysis) 
# for multiple species/stocks at once in batch mode
# See https://github.com/adossantos-jr/psa-batch-mode

if (!require("remotes")) install.packages("remotes")
if (!require("FishLife")) remotes::install_github("James-Thorson-NOAA/James-Thorson-NOAA/FishLife")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, MASS, stringr, dplyr, ggplot2, ggrepel, patchwork, FishLife, DT, tidyr)

prod_attrs = c("r","tmax","lmax","k","m","fec","breed","rec","tmat","troph")
susc_attrs = c("area_over","geog_conc","vert_over","seas_migr","school","morph","desire","mng_strat","f_over_m","b_over_b0","surv_prob","hab_impact")
all_attributes = c(prod_attrs, susc_attrs)

fl_map = c(r="r", k="K", m="M", tmax="tmax", lmax="Loo", tmat="tm")
fl_sub_attrs = names(fl_map)

high_val_is_high_cat = c(
  r=TRUE,  tmax=FALSE, lmax=FALSE, k=TRUE,  m=TRUE,  fec=TRUE,
  breed=FALSE, rec=NA, tmat=TRUE, troph=FALSE,
  area_over=TRUE, geog_conc=NA, vert_over=NA, seas_migr=NA,
  school=NA, morph=NA, desire=NA, mng_strat=NA,
  f_over_m=TRUE, b_over_b0=FALSE, surv_prob=FALSE, hab_impact=NA
)

threshold_defaults = list(
  r=c(0.16,0.50), tmax=c(10,30), lmax=c(60,150), k=c(0.15,0.25), m=c(0.20,0.40),
  fec=c(1000,100000), breed=c(1,3), rec=c(1,2), tmat=c(2,4), troph=c(2.5,3.5),
  area_over=c(0.25,0.50), geog_conc=c(1,2), vert_over=c(1,2), seas_migr=c(1,2),
  school=c(1,2), morph=c(1,2), desire=c(1,2), mng_strat=c(1,2),
  f_over_m=c(0.5,1.0), b_over_b0=c(0.25,0.40), surv_prob=c(0.33,0.67), hab_impact=c(1,2)
)

create_empty_species_list = function(species_names) {
  lapply(setNames(as.list(species_names), species_names), function(sp)
    data.frame(attribute=all_attributes, low=0, mod=0, high=0, weight=1, source="None", stringsAsFactors=FALSE)
  )
}

vul_class = function(x) ifelse(x >= 2.2, "High", ifelse(x > 1.8, "Moderate", "Low"))

fetch_fishlife_single = function(sciname, n_samples=5000) {
  col_names = c("Loo","K","Winfinity","tmax","tm","M","Lm","Temperature",
                "ln_var","rho","ln_MASPS","ln_margsd","h","logitbound_h",
                "ln_Fmsy_over_M","ln_Fmsy","ln_r","r","ln_G","G")
  back_transform = function(v) c(exp(v[1:7]), v[8], v[9:20])
  
  genus = word(sciname, 1)
  sp    = word(sciname, 2, length(strsplit(sciname, " ")[[1]]))
  sp    = ifelse(is.na(sp) || sp %in% c("spp",""), "predictive", sp)
  
  res = try(FishLife::Search_species(Genus=genus, Species=sp), silent=TRUE)
  if (inherits(res, "try-error")) return(NULL)
  spp = try(FishLife::Plot_taxa(res$match_taxonomy), silent=TRUE)
  if (inherits(spp, "try-error")) return(NULL)
  
  pred    = spp[[1]]
  samples = as.data.frame(t(apply(MASS::mvrnorm(n_samples, pred$Mean_pred, pred$Cov_pred), 1, back_transform)))
  colnames(samples) = col_names
  list(samples=samples, matched_taxon=paste(res$match_taxonomy, collapse=" > "))
}

calculate_single_probs = function(s, t_inputs, high_cat_dir) {
  res_list = list()
  for (attr in names(fl_map)) {
    fl_col = fl_map[[attr]]
    vals = s[[fl_col]]
    t1   = t_inputs[[attr]]$t1
    t2   = t_inputs[[attr]]$t2
    is_high_cat = high_cat_dir[[attr]]
    if (!is.null(vals) && !is.null(t1) && !is.null(t2)) {
      cats = if (is_high_cat) ifelse(vals > t2, "high", ifelse(vals < t1, "low", "mod")) else
        ifelse(vals < t1, "high", ifelse(vals > t2, "low", "mod"))
      res_list[[attr]] = list(low=mean(cats=="low",na.rm=TRUE), mod=mean(cats=="mod",na.rm=TRUE), high=mean(cats=="high",na.rm=TRUE))
    }
  }
  res_list
}

df_col = {
  xc = seq(0, 1, length.out=200)
  yc = seq(0, 1, length.out=200)
  g  = cbind(expand.grid(xcolor=xc, ycolor=yc), expand.grid(x=seq(3,1,length.out=200), y=seq(1,3,length.out=200)))
  g$zcolor = g$xcolor^2 + g$ycolor^2
  g
}

ui = fluidPage(
  titlePanel("PSA in batch-processing mode"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_csv", "Choose CSV file", accept=".csv"),
      hr(),
      tags$h4(tags$b("Step 1: Set attribute thresholds"), style="color:#337ab7; margin-bottom:12px;"),
      tags$p("Define thresholds (Low/Moderate and Moderate/High) for each attribute:", style="font-size:12px; color:#666;"),
      tags$div(
        style="max-height:400px; overflow-y:auto; border:1px solid #ddd; padding:10px; background-color:#fafafa; border-radius:4px; margin-bottom:15px;",
        lapply(all_attributes, function(attr) {
          defs = threshold_defaults[[attr]]
          fluidRow(
            style="margin-bottom:8px; padding-bottom:4px; border-bottom:1px dashed #eee;",
            column(4, tags$b(attr, style="font-size:13px; display:block; margin-top:10px;")),
            column(4, style="padding:2px;", numericInput(paste0("g_t_",attr,"_1"), "Low to Moderate", value=defs[1])),
            column(4, style="padding:2px;", numericInput(paste0("g_t_",attr,"_2"), "Moderate to High", value=defs[2]))
          )
        })
      ),
      hr(),
      actionButton("run_analysis", "Run PSA", class="btn-success", style="width:100%; font-size:14px; font-weight:bold; padding:12px;")
    ),
    mainPanel(
      tabsetPanel(
        id="main_tabs",
        tabPanel("Configuration",
                 tags$br(),
                 wellPanel(
                   style="border:2px solid #f0ad4e;",
                   tags$h4(tags$b("Step 2: Batch score options"), style="color:#f0ad4e;"),
                   tags$p("Attributes in your data are categorized within thresholds automatically"),
                   actionButton("bulk_score_fl", "Score life-history traits via FishLife (all species)", class="btn-warning", style="width:100%; font-weight:bold;")
                 ),
                 uiOutput("step3_panel")
        ),
        tabPanel("PSA plots",
                 tags$br(),
                 downloadButton("dl_psa_plot", "Download PSA plot (PNG)"),
                 tags$br(), tags$br(),
                 plotOutput("psa_plot", height="600px"),
                 tags$hr(),
                 downloadButton("dl_density_plot", "Download prod. density plot (PNG)"),
                 tags$br(), tags$br(),
                 plotOutput("density_plot", height="400px")
        ),
        tabPanel("Vulnerability prob. plot",
                 tags$br(),
                 downloadButton("dl_proportions_plot", "Download vulnerability prob. plot (PNG)"),
                 tags$br(), tags$br(),
                 plotOutput("proportions_plot", height="500px")
        ),
        tabPanel("Results summary",
                 tags$br(),
                 downloadButton("dl_results_table", "Download CSV"),
                 tags$br(), tags$br(),
                 DTOutput("results_table")
        ),
        tabPanel("Vulnerability outcomes",
                 tags$br(),
                 downloadButton("dl_probs_table", "Download CSV"),
                 tags$br(), tags$br(),
                 DTOutput("probs_table")
        ),
        tabPanel("Attribute probability table",
                 tags$br(),
                 downloadButton("dl_fishlife_matrix", "Download CSV"),
                 tags$br(), tags$br(),
                 DTOutput("fishlife_matrix")
        )
      )
    )
  )
)

server = function(input, output, session) {
  
  rv = reactiveValues(
    df=NULL, species_list=NULL, matched_taxa=list(), psa_results=NULL, initialized=FALSE
  )
  
  # Auto-balance low/mod/high probabilities to sum to 1
  lapply(all_attributes, function(attr) {
    rebalance = function(changed, other1_id, other2_id) {
      observeEvent(input[[changed]], {
        val = input[[changed]]
        if (is.null(val) || is.na(val)) return()
        val = max(0, min(1, val))
        o1  = isolate(input[[other1_id]]) %||% 0
        o2  = isolate(input[[other2_id]]) %||% 0
        if (abs(val + o1 + o2 - 1) > 0.01) {
          rem = 1 - val
          if (o1 + o2 > 0) { n1 = rem * o1/(o1+o2); n2 = rem * o2/(o1+o2) } else { n1 = n2 = rem/2 }
          updateNumericInput(session, other1_id, value=round(n1, 3))
          updateNumericInput(session, other2_id, value=round(n2, 3))
        }
      }, ignoreInit=TRUE)
    }
    rebalance(paste0("tw_low_",  attr), paste0("tw_mod_",  attr), paste0("tw_high_", attr))
    rebalance(paste0("tw_mod_",  attr), paste0("tw_low_",  attr), paste0("tw_high_", attr))
    rebalance(paste0("tw_high_", attr), paste0("tw_low_",  attr), paste0("tw_mod_",  attr))
  })
  
  get_threshold_inputs = function() {
    lapply(setNames(all_attributes, all_attributes), function(attr)
      list(t1=input[[paste0("g_t_",attr,"_1")]], t2=input[[paste0("g_t_",attr,"_2")]])
    )
  }
  
  score_from_csv = function(sp, attr) {
    req(rv$df)
    row_data = rv$df[rv$df$species == sp, ][1, ]
    idx = which(rv$species_list[[sp]]$attribute == attr)
    if (length(idx) == 0 || !(attr %in% names(row_data))) return()
    
    val_raw = row_data[[attr]]
    val_str = tolower(trimws(as.character(val_raw)))
    
    low_cats  = c("low","decrease_fish","low_selec","lowfreq","proactive")
    mod_cats  = c("mod","no_effect","mod_select","mod_selec","modfreq","reactive","moderate")
    high_cats = c("high","increase_fish","high_selec","highfreq","no_strat")
    
    if (val_str %in% c(low_cats, mod_cats, high_cats)) {
      mag = if (val_str %in% low_cats) "low" else if (val_str %in% mod_cats) "mod" else "high"
    } else {
      val = suppressWarnings(as.numeric(val_raw))
      t1  = input[[paste0("g_t_",attr,"_1")]]
      t2  = input[[paste0("g_t_",attr,"_2")]]
      dir = high_val_is_high_cat[[attr]]
      if (!is.na(val) && !is.null(t1) && !is.null(t2) && !is.na(dir)) {
        mag = if (dir) ifelse(val > t2, "high", ifelse(val < t1, "low", "mod")) else
          ifelse(val < t1, "high", ifelse(val > t2, "low", "mod"))
      } else {
        mag = NA
      }
    }
    
    rv$species_list[[sp]]$low[idx]    = ifelse(!is.na(mag) && mag=="low",  1, 0)
    rv$species_list[[sp]]$mod[idx]    = ifelse(!is.na(mag) && mag=="mod",  1, 0)
    rv$species_list[[sp]]$high[idx]   = ifelse(!is.na(mag) && mag=="high", 1, 0)
    rv$species_list[[sp]]$source[idx] = "CSV"
  }
  
  observeEvent(input$file_csv, {
    req(input$file_csv)
    df_raw = read.csv(input$file_csv$datapath, stringsAsFactors=FALSE)
    rv$df  = df_raw
    sp_names = unique(df_raw$species)
    rv$species_list  = create_empty_species_list(sp_names)
    rv$initialized   = TRUE
    for (sp in sp_names) for (attr in all_attributes) score_from_csv(sp, attr)
    showNotification("CSV loaded.", type="message")
  })
  
  observeEvent(input$bulk_score_fl, {
    req(rv$df)
    sp_names = unique(rv$df$species)
    t_inputs = get_threshold_inputs()
    failed   = c()
    withProgress(message="Scoring via FishLife...", value=0, {
      for (i in seq_along(sp_names)) {
        sp = sp_names[i]
        setProgress(i/length(sp_names), detail=paste("querying:", sp))
        res = fetch_fishlife_single(sp)
        if (is.null(res)) { failed = c(failed, sp); next }
        probs = calculate_single_probs(res$samples, t_inputs, high_val_is_high_cat)
        rv$matched_taxa[[sp]] = res$matched_taxon
        for (attr in names(probs)) {
          idx = which(rv$species_list[[sp]]$attribute == attr)
          if (length(idx) > 0) {
            rv$species_list[[sp]]$low[idx]    = round(probs[[attr]]$low,  4)
            rv$species_list[[sp]]$mod[idx]    = round(probs[[attr]]$mod,  4)
            rv$species_list[[sp]]$high[idx]   = round(probs[[attr]]$high, 4)
            rv$species_list[[sp]]$source[idx] = "FishLife"
          }
        }
      }
    })
    if (length(failed) > 0) showNotification(paste("Failed:", paste(failed, collapse=", ")), type="warning") else
      showNotification("All species scored via FishLife.", type="message")
  })
  
  score_species_via_fl = function(sp) {
    t_inputs = get_threshold_inputs()
    res = fetch_fishlife_single(sp)
    if (is.null(res)) { showNotification(paste("FishLife match failed for:", sp), type="error"); return() }
    probs = calculate_single_probs(res$samples, t_inputs, high_val_is_high_cat)
    rv$matched_taxa[[sp]] = res$matched_taxon
    for (attr in all_attributes) {
      idx = which(rv$species_list[[sp]]$attribute == attr)
      if (length(idx) == 0) next
      if (attr %in% fl_sub_attrs && attr %in% names(probs)) {
        rv$species_list[[sp]]$low[idx]    = round(probs[[attr]]$low,  4)
        rv$species_list[[sp]]$mod[idx]    = round(probs[[attr]]$mod,  4)
        rv$species_list[[sp]]$high[idx]   = round(probs[[attr]]$high, 4)
        rv$species_list[[sp]]$source[idx] = "FishLife"
      } else {
        score_from_csv(sp, attr)
      }
    }
  }
  
  observeEvent(input$fetch_full_species_fl, {
    req(input$tweak_sp_selector)
    score_species_via_fl(input$tweak_sp_selector)
    showNotification(paste("FishLife scored:", input$tweak_sp_selector), type="message")
  })
  
  observeEvent(input$reset_species_csv_btn, {
    req(input$tweak_sp_selector)
    sp = input$tweak_sp_selector
    for (attr in all_attributes) score_from_csv(sp, attr)
    showNotification(paste("Re-scored from CSV:", sp), type="message")
  })
  
  lapply(all_attributes, function(target_attr) {
    observeEvent(input[[paste0("fetch_single_attr_", target_attr)]], {
      req(input$tweak_sp_selector)
      sp = input$tweak_sp_selector
      if (target_attr %in% fl_sub_attrs) {
        t_inputs = get_threshold_inputs()
        res = fetch_fishlife_single(sp)
        if (is.null(res)) { showNotification("FishLife match failed.", type="error"); return() }
        probs = calculate_single_probs(res$samples, t_inputs, high_val_is_high_cat)
        rv$matched_taxa[[sp]] = res$matched_taxon
        if (target_attr %in% names(probs)) {
          idx = which(rv$species_list[[sp]]$attribute == target_attr)
          if (length(idx) > 0) {
            rv$species_list[[sp]]$low[idx]    = round(probs[[target_attr]]$low,  4)
            rv$species_list[[sp]]$mod[idx]    = round(probs[[target_attr]]$mod,  4)
            rv$species_list[[sp]]$high[idx]   = round(probs[[target_attr]]$high, 4)
            rv$species_list[[sp]]$source[idx] = "FishLife"
            showNotification(paste("FishLife scored [", target_attr, "]"), type="message")
          }
        }
      } else {
        score_from_csv(sp, target_attr)
        showNotification(paste("Re-scored [", target_attr, "] from CSV."), type="message")
      }
    })
  })
  
  output$step3_panel = renderUI({
    if (!rv$initialized) return(wellPanel(tags$em("Load a CSV file to enable specific fine-tuning.")))
    wellPanel(
      style="border:2px solid #5cb85c;",
      tags$h4(tags$b("Step 3: Fine-tune single species"), style="color:#5cb85c;"),
      fluidRow(
        column(4, selectInput("tweak_sp_selector", "Species:", choices=names(rv$species_list))),
        column(4, style="margin-top:25px;", actionButton("fetch_full_species_fl", "Score via FishLife", class="btn-info", style="width:100%; font-weight:bold;")),
        column(4, style="margin-top:25px;", actionButton("reset_species_csv_btn", "Reset to CSV", class="btn-warning", style="width:100%; font-weight:bold;"))
      ),
      hr(),
      uiOutput("individual_tweak_grid"),
      tags$br(),
      actionButton("save_tweaks_btn", "Save modifications", class="btn-success", style="width:100%; font-weight:bold;")
    )
  })
  
  output$individual_tweak_grid = renderUI({
    req(input$tweak_sp_selector)
    sp    = input$tweak_sp_selector
    sp_df = rv$species_list[[sp]]
    tagList(
      fluidRow(
        style="font-weight:bold; border-bottom:2px solid #ccc; padding-bottom:5px; margin-bottom:10px;",
        column(3,"Attribute"), column(2,"Low"), column(2,"Moderate"), column(2,"High"), column(2,"Weight"), column(1,"")
      ),
      tags$div(
        style="max-height:380px; overflow-y:auto; overflow-x:hidden; padding-right:10px;",
        lapply(all_attributes, function(attr) {
          row_data = sp_df[sp_df$attribute == attr, ]
          if (nrow(row_data) == 0) row_data = data.frame(low=0, mod=0, high=0, weight=1, source="None")
          btn_label = if (attr %in% fl_sub_attrs) paste0("Fetch ", attr) else "Reset to CSV"
          fluidRow(
            style="margin-bottom:4px; padding-bottom:4px; border-bottom:1px solid #f1f1f1;",
            column(3, tags$b(attr, style="display:block; margin-top:6px;")),
            column(2, style="padding:2px;", numericInput(paste0("tw_low_",  attr), NULL, value=row_data$low,    min=0, max=1, step=0.01)),
            column(2, style="padding:2px;", numericInput(paste0("tw_mod_",  attr), NULL, value=row_data$mod,    min=0, max=1, step=0.01)),
            column(2, style="padding:2px;", numericInput(paste0("tw_high_", attr), NULL, value=row_data$high,   min=0, max=1, step=0.01)),
            column(2, style="padding:2px;", numericInput(paste0("tw_w_",    attr), NULL, value=row_data$weight, min=0,        step=0.5)),
            column(1, style="padding:2px;", actionLink(paste0("fetch_single_attr_", attr), btn_label, style="font-size:11px; font-weight:bold; display:block; margin-top:8px;"))
          )
        })
      )
    )
  })
  
  observeEvent(input$save_tweaks_btn, {
    req(input$tweak_sp_selector)
    sp = input$tweak_sp_selector
    for (attr in all_attributes) {
      idx = which(rv$species_list[[sp]]$attribute == attr)
      if (!is.null(input[[paste0("tw_low_",  attr)]])) rv$species_list[[sp]]$low[idx]    = input[[paste0("tw_low_",  attr)]]
      if (!is.null(input[[paste0("tw_mod_",  attr)]])) rv$species_list[[sp]]$mod[idx]    = input[[paste0("tw_mod_",  attr)]]
      if (!is.null(input[[paste0("tw_high_", attr)]])) rv$species_list[[sp]]$high[idx]   = input[[paste0("tw_high_", attr)]]
      if (!is.null(input[[paste0("tw_w_",    attr)]])) rv$species_list[[sp]]$weight[idx] = input[[paste0("tw_w_",    attr)]]
      rv$species_list[[sp]]$source[idx] = "Manual"
    }
    showNotification(paste("Saved modifications for:", sp), type="message")
  })
  
  observeEvent(input$run_analysis, {
    req(rv$species_list)
    n_sim    = 999
    sp_list  = rv$species_list
    sp_names = names(sp_list)
    
    # Double weight for intrinsic rate of increase
    for (sp in sp_names) sp_list[[sp]]$weight[sp_list[[sp]]$attribute == "r"] = 2
    
    vuln_scores = list()
    withProgress(message="Running permutations...", value=0, {
      for (i in seq_along(sp_names)) {
        sp = sp_names[i]
        incProgress(1/length(sp_names), detail=paste("Simulating:", sp))
        d = sp_list[[sp]]
        d[, c("low","mod","high")] = lapply(d[, c("low","mod","high")], as.numeric)
        
        sample_scores = function(row_indices) {
          vals_list = list(); weights = c()
          for (i in row_indices) {
            p = as.numeric(d[i, c("high","mod","low")]); p[is.na(p)] = 0
            w = d$weight[i]
            if (!is.na(w) && w > 0 && sum(p) > 0) {
              vals_list[[length(vals_list)+1]] = w * sample(c(3,2,1), n_sim, replace=TRUE, prob=p/sum(p))
              weights = c(weights, w)
            }
          }
          if (length(vals_list) > 0) rowSums(do.call(cbind, vals_list)) / sum(weights) else rep(2, n_sim)
        }
        
        prod_scores = sample_scores(which(d$attribute %in% prod_attrs))
        susc_scores = sample_scores(which(d$attribute %in% susc_attrs))
        vuln_scores[[sp]] = list(
          productivity   = prod_scores,
          susceptibility = susc_scores,
          vulnerability  = sqrt((3 - prod_scores)^2 + (susc_scores - 1)^2)
        )
      }
    })
    
    results_df = do.call(rbind, lapply(sp_names, function(sp) {
      v = vuln_scores[[sp]]
      data.frame(
        species             = sp,
        matched_taxon       = rv$matched_taxa[[sp]] %||% NA,
        has_fishlife        = !is.null(rv$matched_taxa[[sp]]),
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
    
    probs_df = do.call(rbind, lapply(sp_names, function(sp) {
      cats = sapply(vuln_scores[[sp]]$vulnerability, vul_class)
      data.frame(
        species = sp,
        probability_low_vulnerability      = round(mean(cats=="Low"),      4),
        probability_moderate_vulnerability = round(mean(cats=="Moderate"), 4),
        probability_high_vulnerability     = round(mean(cats=="High"),     4),
        stringsAsFactors = FALSE
      )
    }))
    
    fishlife_scores_df = do.call(rbind, lapply(sp_names, function(sp) {
      d = sp_list[[sp]]
      sub_d = d[d$attribute %in% fl_sub_attrs, ]
      if (nrow(sub_d) == 0) return(NULL)
      data.frame(
        species=sp, attribute=sub_d$attribute,
        prob_low=sub_d$low, prob_mod=sub_d$mod, prob_high=sub_d$high,
        expected_score=sub_d$low*1 + sub_d$mod*2 + sub_d$high*3,
        source_type=sub_d$source, row.names=NULL
      )
    }))
    
    prod_density_df = do.call(rbind, lapply(sp_names, function(sp)
      data.frame(species=sp, productivity=vuln_scores[[sp]]$productivity)
    ))
    
    rv$psa_results = list(
      results_df=results_df, probs_df=probs_df,
      fishlife_scores_df=fishlife_scores_df, prod_density_df=prod_density_df
    )
    
    updateTabsetPanel(session, "main_tabs", selected="PSA plots")
    showNotification("Done!", type="message")
  })
  
  build_psa_plot = reactive({
    req(rv$psa_results)
    res = rv$psa_results$results_df
    
    scatter = ggplot() +
      geom_tile(data=df_col, aes(x, y, fill=zcolor)) +
      geom_linerange(data=res, aes(y=mean_susceptibility, x=mean_productivity, ymin=lci_susceptibility, ymax=uci_susceptibility), alpha=0.4, linewidth=1) +
      geom_linerange(data=res, aes(y=mean_susceptibility, x=mean_productivity, xmin=uci_productivity,   xmax=lci_productivity),   alpha=0.4, linewidth=1) +
      geom_point(data=res, aes(y=mean_susceptibility, x=mean_productivity), alpha=0.7, size=2) +
      geom_text_repel(data=res, aes(label=species, y=mean_susceptibility, x=mean_productivity), fontface="italic", force=50, size=2.3) +
      scale_fill_gradientn(colors=c("forestgreen","green3","green2","greenyellow","orange3","red","red2","red3","red4"), guide="none") +
      labs(x="Productivity", y="Susceptibility") +
      xlim(1,3) + ylim(1,3) + scale_x_reverse() +
      coord_cartesian(expand=FALSE) + theme_test() +
      theme(axis.text=element_text(color="black"), legend.position="bottom")
    
    d_prod = ggplot() + geom_density(data=res, aes(x=mean_productivity), fill="grey", color="grey") +
      scale_x_continuous(limits=c(1,3)) + scale_x_reverse() + coord_cartesian(expand=FALSE) + theme_void()
    
    d_susc = ggplot() + geom_density(data=res, aes(y=mean_susceptibility), fill="grey", color="grey") +
      scale_y_continuous(limits=c(1,3)) + coord_cartesian(expand=FALSE) + theme_void()
    
    psa_main = ((d_prod / scatter) + plot_layout(heights=c(0.15,1)) | (plot_spacer() / d_susc) + plot_layout(heights=c(0.15,1))) + plot_layout(widths=c(1,0.15))
    
    hist = ggplot() +
      geom_histogram(data=res, aes(x=mean_vulnerability, fill=vul_category), binwidth=0.03) +
      scale_fill_manual(values=c("greenyellow","orange","red2"), breaks=c("Low","Moderate","High")) +
      labs(x="Vulnerability (mean)", y="No. species", fill="") +
      scale_x_continuous(limits=c(1,3), breaks=seq(1,3,0.5)) +
      theme_bw() + theme(axis.text=element_text(color="black"), legend.position="bottom")
    
    psa_main / (hist + plot_spacer() + plot_layout(widths=c(1,0.2))) + plot_layout(heights=c(1,0.5))
  })
  
  build_density_plot = reactive({
    req(rv$psa_results)
    ggplot(rv$psa_results$prod_density_df, aes(x=productivity, fill=species, color=species)) +
      geom_density(alpha=0.35, linewidth=0.7) +
      geom_vline(data=rv$psa_results$results_df, aes(xintercept=mean_productivity, color=species), linetype="dashed", linewidth=0.6) +
      labs(x="Productivity score", y="Density", fill="", color="") +
      scale_x_continuous(limits=c(1,3), breaks=seq(1,3,0.5)) +
      theme_bw() + theme(axis.text=element_text(color="black"), legend.text=element_text(face="italic"), legend.position="bottom")
  })
  
  build_proportions_plot = reactive({
    req(rv$psa_results)
    ordered_sp = rv$psa_results$results_df[order(rv$psa_results$results_df$mean_vulnerability), "species"]
    plot_data  = rv$psa_results$probs_df %>%
      pivot_longer(cols=starts_with("probability_"), names_to="vul_category", values_to="proportion") %>%
      mutate(
        vul_category = case_when(
          vul_category == "probability_low_vulnerability"      ~ "Low",
          vul_category == "probability_moderate_vulnerability" ~ "Moderate",
          vul_category == "probability_high_vulnerability"     ~ "High"
        ),
        vul_category = factor(vul_category, levels=c("Low","Moderate","High")),
        species      = factor(species, levels=ordered_sp)
      )
    ggplot(plot_data, aes(x=species, y=proportion, fill=vul_category)) +
      geom_bar(stat="identity", width=1) +
      scale_fill_manual(values=c(Low="greenyellow", Moderate="orange", High="red2"), name="Vulnerability") +
      labs(x="Species (ordered by vulnerability)", y="Proportion of bootstrap samples") +
      theme_bw() +
      theme(axis.text.x=element_text(angle=45, hjust=1, face="italic", color="black"), axis.text.y=element_text(color="black"))
  })
  
  output$psa_plot         = renderPlot({ build_psa_plot() })
  output$density_plot     = renderPlot({ build_density_plot() })
  output$proportions_plot = renderPlot({ build_proportions_plot() })
  
  output$results_table    = renderDT({ req(rv$psa_results); datatable(rv$psa_results$results_df,        rownames=FALSE) })
  output$probs_table      = renderDT({ req(rv$psa_results); datatable(rv$psa_results$probs_df,          rownames=FALSE) })
  output$fishlife_matrix  = renderDT({ req(rv$psa_results); datatable(rv$psa_results$fishlife_scores_df, rownames=FALSE) })
  
  dl_plot = function(plot_fn, w, h) downloadHandler(
    filename = function() paste0(deparse(substitute(plot_fn)), "_", Sys.Date(), ".png"),
    content  = function(file) ggsave(file, plot=plot_fn(), width=w, height=h, dpi=300)
  )
  
  output$dl_psa_plot         = downloadHandler(filename=function() paste0("psa_plot_",        Sys.Date(), ".png"), content=function(file) ggsave(file, plot=build_psa_plot(),         w=10.5/2,  h=15/2, dpi=300))
  output$dl_density_plot     = downloadHandler(filename=function() paste0("density_plot_",    Sys.Date(), ".png"), content=function(file) ggsave(file, plot=build_density_plot(),     w=15/2, h=9/2,  dpi=300))
  output$dl_proportions_plot = downloadHandler(filename=function() paste0("proportions_plot_",Sys.Date(), ".png"), content=function(file) ggsave(file, plot=build_proportions_plot(), w=15/2, height=7.5/2,  dpi=300))
  
  output$dl_results_table  = downloadHandler(filename=function() paste0("results_summary_",        Sys.Date(), ".csv"), content=function(file) { req(rv$psa_results); write.csv(rv$psa_results$results_df,         file, row.names=FALSE) })
  output$dl_probs_table    = downloadHandler(filename=function() paste0("vulnerability_outcomes_",  Sys.Date(), ".csv"), content=function(file) { req(rv$psa_results); write.csv(rv$psa_results$probs_df,           file, row.names=FALSE) })
  output$dl_fishlife_matrix = downloadHandler(filename=function() paste0("attribute_prob_table_",   Sys.Date(), ".csv"), content=function(file) { req(rv$psa_results); write.csv(rv$psa_results$fishlife_scores_df,  file, row.names=FALSE) })
}

shinyApp(ui, server)