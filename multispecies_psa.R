# Productivity-Susceptibility for multiple species at one
# more information can be found at https://github.com/adossantos-jr/psa_multispecies

# Set your working directory

setwd('your wd')

# Importing your data frame 

df = read.csv('test_psa_data.csv') 

# For a standard extended PSA, just run all the following script (Ctrl + A & Ctrl + Enter)
# More modifications to the PSA can be found below in the script if needed

# Loading/installing packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, grid, ggrepel, patchwork)

# Below are functions to categorize productivity attributes
# If wished, modify attribute thresholds in the following functions:

# Intrinsic growth rate (r)
cat_r = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 0.5, 'high', 
                ifelse(x >= 0.16 & x <= 0.5, 'mod', 'low')))
}
# Maximum age (tmax)
cat_tmax = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x < 10, 'high', 
                ifelse(x >= 10 & x <= 30, 'mod', 'low')))
}
# Maximum size (Lmax)
cat_lmax = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x < 60, 'high', 
                ifelse(x >= 60 & x <= 150, 'mod', 'low')))
}
# Von Bertalanffy Growth Rate (K)
cat_k = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 0.25, 'high', 
                ifelse(x >= 0.15 & x <= 0.25, 'mod', 'low')))
}
# Natural mortality rate (M)
cat_m = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 0.4, 'high', 
                ifelse(x >= 0.2 & x <= 0.4, 'mod', 'low')))
}
# Measured fecundity 
cat_fec = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 100000, 'high', 
                ifelse(x >= 1000 & x <= 10000, 'mod', 'low')))
}
# Breeding strategy by Winemiller's index 
cat_breed = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x == 0, 'high', 
                ifelse(x %in% 1:3, 'mod',
                       ifelse(x %in% 4:14, 'low', NA))))
}
# Frequency of recruitment 
cat_rec = function(x) {
    ifelse(is.na(x), NA, 
           ifelse(x == 'highfreq', 'high', 
                  ifelse(x == 'modfreq', 'mod',
                         ifelse(x == 'lowfreq', 'low', NA))))
  }
# Age at maturity (tmat)
cat_tmat = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 4, 'high', 
                ifelse(x >= 2 & x <= 4, 'mod', 'low')))
}
# Mean trophic level 
cat_troph = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x < 2.5, 'high', 
                ifelse(x >= 2.5 & x <= 3.5, 'mod', 'low')))
}

## Functions to categorize susceptibility attributes 

# Areal overlap
cat_area_over = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 0.5, 'high', 
                ifelse(x >= 0.25 & x <= 0.5, 'mod', 'low')))
}

# Geographic concentration 
cat_geog_conc = function(x) {
    ifelse(is.na(x), NA, 
           ifelse(x == 'high', 'high', 
                  ifelse(x == 'mod', 'mod',
                         ifelse(x == 'low', 'low', NA))))
  }

# Vertical overlap 
cat_vert_over = function(x) {
    ifelse(is.na(x), NA, 
           ifelse(x == 'high', 'high', 
                  ifelse(x == 'mod', 'mod',
                         ifelse(x == 'low', 'low', NA))))
  }

# Seasonal migrations
cat_seas_migr = function(x) {
    ifelse(is.na(x), NA, 
           ifelse(x == 'increase_fish', 'high', 
                  ifelse(x == 'no_effect', 'mod',
                         ifelse(x == 'decrease_fish', 'low', NA))))
  }

# Schooling or other behavior
cat_school = function(x) {
    ifelse(is.na(x), NA, 
           ifelse(x == 'increase_fish', 'high', 
                  ifelse(x == 'no_effect', 'mod',
                         ifelse(x == 'decrease_fish', 'low', NA))))
  }

# Morphology affecting capture 
cat_morph = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x == 'high_selec', 'high', 
                ifelse(x == 'mod_selec', 'mod',
                       ifelse(x == 'low_selec', 'low', NA))))
}

# Desirability  
cat_desire = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x == 'high', 'high', 
                ifelse(x == 'mod', 'mod',
                       ifelse(x == 'low', 'low', NA))))
}

# Management strategy
cat_mng_strat = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x == 'no_strat', 'high', 
                ifelse(x == 'reactive', 'mod',
                       ifelse(x == 'proactive', 'low', NA))))
}

# Fishing mortality relative to natural mortality
cat_f_over_m = function(x) {
    ifelse(is.na(x), NA, 
           ifelse(x > 1, 'high', 
                  ifelse(x >= 0.5 & x <= 1, 'mod', 'low')))
}

# Biomass relative to virgin biomass
cat_b_over_b0 = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x < 0.25, 'high', 
                ifelse(x >= 0.25 & x <= 0.4, 'mod', 'low')))
}
# Probability of survival after capture
cat_surv_prob = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x < 0.33, 'high', 
                ifelse(x >= 0.33 & x <= 0.67, 'mod', 'low')))
}
# Habitat impact of the fishery
cat_hat_impact = function(x) {
    ifelse(is.na(x), NA, 
           ifelse(x == 'high', 'high', 
                  ifelse(x == 'mod', 'mod',
                         ifelse(x == 'low', 'low', NA))))
  }


# Applying functions on the data frame

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

# Creating a species list

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

# Modify attribute weights here: 

 for (species_name in names(species_list)) {
 species_df = species_list[[species_name]]
  
 species_df$weight[species_df$attribute == 'r'] = 2
  
 species_list[[species_name]] = species_df
 }

# Assign probabilities here:

# species_list[['Acanthurus bahianus']]['high'][species_list[['Acanthurus bahianus']]['attribute'] == 'weight', ] = 1

# species_list[['Acanthurus bahianus']]['mod'][species_list[['Acanthurus bahianus']]['attribute'] == 'r', ] = 0.5

# species_list[['Acanthurus bahianus']]['high'][species_list[['Acanthurus bahianus']]['attribute'] == 'r', ] = 0.5

# species_list[['Acanthurus bahianus']]['mod'][species_list[['Acanthurus bahianus']]['attribute'] == 'area_over', ] = 0.5

# species_list[['Acanthurus bahianus']]['high'][species_list[['Acanthurus bahianus']]['attribute'] == 'area_over', ] = 0.5

# species_list[['Acanthurus bahianus']] # Check if attributes are right


# PSA itself

# Set number of bootstrap samples
num_samples = 999
num_prod_attr = 10  
num_susc_attr = 12  

vulnerability_scores = list()  
for (species in names(species_list)) {
  species_data = species_list[[species]]  
  species_data_numeric = species_data
  species_data_numeric[, c("low", "mod", "high")] = lapply(species_data[, c("low", "mod", "high")], 
                                                           as.numeric)
  
  prodMatrix = matrix(NA, nrow = num_samples, ncol = num_prod_attr + 1)
  suscMatrix = matrix(NA, nrow = num_samples, ncol = num_susc_attr + 1)
  
  sumProdWeights = sum(species_data$weight[1:num_prod_attr], na.rm = TRUE)
  sumSuscWeights = sum(species_data$weight[(num_prod_attr + 1):(num_prod_attr + num_susc_attr)], na.rm = TRUE)
  
  for (i in 1:num_prod_attr) {
    prob_high = species_data_numeric$high[i]
    prob_mod = species_data_numeric$mod[i]
    prob_low = species_data_numeric$low[i]
    weight = species_data$weight[i]
    
    if (!is.na(weight) && weight > 0) {
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
    
    if (!is.na(weight) && weight > 0) {
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

# Creating an workable data frame for  outputs 

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

# Adjusting vulnerability thresholds

vul_class = function(x) {
  ifelse(x >= 2.2, 'High', 
         ifelse(x > 1.8 & x < 2.2, 'Moderate', 'Low')) 
}

species_vulnerability_df$vul_category = vul_class(species_vulnerability_df$mean_vulnerability)

write.csv(species_vulnerability_df, 'psa_result.csv')

# Plotting 

xcolor = seq(0,1,length.out=200)  
ycolor = seq(0,1,length.out=200)
x = seq(3,1,length.out=200) 
y = seq(1,3, length.out=200)
df_col = cbind(expand.grid(x=xcolor, y=ycolor), expand.grid(x=x, y=y)) 
colnames(df_col) = c("xcolor","ycolor","x","y")
df_col$zcolor = (df_col$xcolor^2+df_col$ycolor^2)


prod_susc_plot = 
ggplot()+
  geom_tile(data = df_col, aes(x, y,fill = zcolor))+
  geom_linerange(data = species_vulnerability_df,
             aes(y = mean_susceptibility, x = mean_productivity,
                 ymin = min_susceptibility, ymax = max_susceptibility), 
             alpha = 0.4, linewidth = 1)+  
  geom_linerange(data = species_vulnerability_df,
                 aes(y = mean_susceptibility, x = mean_productivity,
                     xmin = max_productivity, xmax = min_productivity),
                 alpha = 0.4, linewidth = 1)+  
  geom_point(data = species_vulnerability_df,
                 aes(y = mean_susceptibility, x = mean_productivity),
             alpha = 0.5, size = 2)+
    geom_text_repel(data  = species_vulnerability_df,
                  aes(label = species, y = mean_susceptibility,
                    x = mean_productivity), 
                  fontface = 'italic', force = 50, size = 2.3)+
  labs(x = 'Productivity', y = 'Susceptibility')+
  theme_test()+
  theme(axis.text = element_text(color = "black"), legend.position = 'none')+
  coord_cartesian(expand = F)+
  scale_fill_gradientn(colors = c('forestgreen' ,'green3', 'green2',
                                  'greenyellow', 'orange3', 'red', 
                                  'red2', 'red3', 'red4'))+
  xlim(1,3)+ylim(1,3)+
  scale_x_reverse()

dens_prod_plot = 
ggplot()+
  geom_density(data = species_vulnerability_df,
               aes(x = mean_productivity),
               fill = 'grey',
               color = 'grey')+
  theme_void()+
  scale_x_continuous(limits = c(1,3),
                     breaks = c(1, 1.5, 2, 2.5, 3))+
  coord_cartesian(expand = F)+
  scale_x_reverse()
  

dens_susc_plot = 
  ggplot()+
  geom_density(data = species_vulnerability_df,
               aes(y = mean_susceptibility),
               fill = 'grey', color = 'grey')+
  theme_void()+
   coord_cartesian(expand = F)+
    scale_y_continuous(limits = c(1,3),
                     breaks = c(1, 1.5, 2, 2.5, 3))


psa_main = 
((dens_prod_plot/prod_susc_plot) + plot_layout(heights = c(0.15,1)) | 
  (plot_spacer()/dens_susc_plot) + plot_layout(heights = c(0.15,1))) +
  plot_layout(widths = c(1,0.15))

hist_plot =
ggplot()+
  geom_histogram(data = species_vulnerability_df,
                 aes(x = mean_vulnerability,
                     fill = vul_category),
                 binwidth = 0.03)+
  scale_fill_manual(values = c('greenyellow', 'orange', 'red2'),
                    breaks = c('Low', 'Moderate', 'High'))+
  labs(x = 'Vulnerability', y = 'No. species', fill = '')+
  theme_bw()+
  theme(axis.text = element_text(color = "black"))+
  theme(legend.position = 'bottom')+
  scale_x_continuous(limits = c(1,3),
    breaks = c(1, 1.5, 2, 2.5, 3))
  
plots_psa = psa_main/(hist_plot+plot_spacer() + plot_layout(widths = c(1,0.2))) + 
  plot_layout(heights = c(1,0.5))

ggsave(plots_psa, filename = 'plots_psa.png', dpi = 600, 
       h = 40/5.5, w = 25/5.5)

print('Done! check your working directory for the results')




  


