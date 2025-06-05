## Productivity-Susceptiblity Analysis (PSA) for multiple species at once
This is an R routine for rapidly performing Productivity-Susceptibility Analysis (PSA) for multiple species/stocks at once in a simple way. The code for the PSA itself is mostly sourced from the PSA Web Application at the Fisheries Integrated Toolbox (FIT) by the U. S. National Oceanic and Atmospheric Administration (NOAA), developed by Dr. Nathan Vaughan & Dr. Jason Cope. Like in the NOAA PSA Web Application, this routine allows for probabilistic attribute scoring and generates a bootstrapping-based confidence interval. 

### Basic instructions
This routine is built in the R language. To use it, you must download and install the latest version of R at https://www.r-project.org/, and the latest version of the RStudio integrated development environment (IDE) at https://posit.co/download/rstudio-desktop/. You can then open the script (multispecies_psa.R) in RStudio.

#### Importing your data
The first step is setting your working directory. This directory is where the input data must be placed, and where the results will appear. This is done in this section (line 6):

```
  setwd('your wd')
```
Where 'your wd' must be replaced with a directory path. An example of a working directory path in Windows is:
```
setwd('/Users/alexandre/Documents/multispecies_psa')
```
To better understand working directories in R, a comprehensive guide can be found at https://intro2r.com/work-d.html.

This routine requires a simple data frame with species as rows and attributes as columns. Columns must be named after the column names in the test data frame (test_psa_data.csv). Columns are: 

*species* - the name of the species/stocks being evaluated;
*r* - Intrinsic rate of population growth;
*tmax* - Maximum age;
*lmax* - Maxium length;
*k* - Von Bertalanffy Growth Coefficient;
*fec* - Measured fecundity;
*breed* - Winemiller's index (breeding strategy quantificaton);
*rec* - Frequency of recruitment;
*tmat* - Age at maturity;
*troph* - Mean trophic level;
*area_over* - Areal overlap;
*geog_conc* - Geographical concentration;
*vert_over* - Vertical overlap;
*seas_migr* - Seasonal migrations affecting capture;
*school* - Schooling (or similar behaviors) affecting capture;
*morph* - Morphology affecting capture;
*desire* - Desirability of the species/stock;
*mng_strat* - Management strategy;
*f_over_m* - Fishing mortality in relation to natural mortalilty;
*b_over_b0* - Stock biomass in relation to virgin biomass;
*surv_prob* - Survival probability;
*hab_impact* - Habitat impact of the fishery.

More information on this set of attributes can be found in the resources.

Data frame importing is done in this section (line 10):
```
df = read.csv('test_psa_data.csv') 
```
Where 'test_psa_data.csv' is a .csv (comma-separated values) file with species as rows and attributes as columns. If needed, you can name this file after your own data frame, such as:
```
df = read.csv('species_attributes_gillnet_pernambuco.csv')
```
Categorical columns must be filled according to the respective functions in the script (i. e. cat_morph requires categories 'high_selec', 'mod_selec' & 'low_selec'); examples are available in the test data. If you use software such as Microsoft Excel to buid the data frame, remember to always convert it to a .csv file before importing to R.

And done! This is all it is needed to run the rountine. After setting the working directory and uploading your data, press Ctrl + A to select all the script and then Ctrl + Enter to run it. All the necessary packages will be automatically installed and/or loaded. Results will appear in your working directory in the form of a .csv file (psa_result.csv) and a image (plots_psa.png) with a susceptibility by productivity plot and a histogram. If you want to modify the PSA settings further, see below.

### Further modifications
Several modifications can be made throughout the script to adapt the PSA to the specific desired conditions. Some key adjustments can be found below. 
#### Modifying attribute thresholds
To modify attribute thresholds (what is defined as "low", "moderate" and "high" producitivity or susceptibility), the functions to define those thresholds can be modified (starting in line 24). For example, the Von Bertalanffy growth rate (*K*) thresholds are set to 0.15 between low and moderate, and 0.25 between moderate and high: 

```
cat_k = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 0.25, 'high', 
                ifelse(x >= 0.15 & x <= 0.25, 'mod', 'low')))
}
```
If you wish to modify the *K* thresholds to to 0.3 and 0.5 (instead of the standard 0.15 and 0.25), for example, you can modify the values accordingly in the conditionals: 

```
cat_k = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x > 0.5, 'high',       # now if K > 0.5, it will be set to "high" productivity 
                ifelse(x >= 0.3 & x <= 0.5, 'mod', 'low'))) # if K is between 0.3  and 0.5, it will be set to "moderate",                                                                        # otherwise it will be "low"  
}
```
Categorical attributes can also be modified this way. Here is an example with management strategy, in which default categories are 'no_strat', 'reactive' and 'proactive': 

```
cat_mng_strat = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x == 'no_strat', 'high', 
                ifelse(x == 'reactive', 'mod',
                       ifelse(x == 'proactive', 'low', NA))))
}
```
If you wish to change it to 'presence_catch_quota' for low susceptibility, 'fishing_ban' for moderate, and 'none' for high, for example, this can be done by modifying the default category names: 

```
cat_mng_strat = function(x) {
  ifelse(is.na(x), NA, 
         ifelse(x == 'none', 'high', 
                ifelse(x == 'fishing_ban', 'mod',
                       ifelse(x == 'presence_catch_quota', 'low', NA))))
}
```
#### Modifying attribute weights
In this routine, all attribute weights are set to 1 as a default, except for intrinsic growth rate (*r*), which is set to 2. This is done in this section (line 229):

```
 for (species_name in names(species_list)) {
 species_df = species_list[[species_name]]
  
 species_df$weight[species_df$attribute == 'r'] = 2
  
 species_list[[species_name]] = species_df
 }
```
If you wish to modify the weight of *r* to 4, for example, you can modify the number:

```
 for (species_name in names(species_list)) {
 species_df = species_list[[species_name]]
  
 species_df$weight[species_df$attribute == 'r'] = 4 #now r is set to weight 4
  
 species_list[[species_name]] = species_df
 }
```
This can be done to all attributes. If you wish to modify the weight of areal overlap as well, for example, the same code above can be used; only change 'r' to 'area_over' and specify the desired weight: 

```
 for (species_name in names(species_list)) {
 species_df = species_list[[species_name]]
  
species_df$weight[species_df$attribute == 'r'] = 4 #now r is set to weight 4
species_df$weight[species_df$attribute == 'area_over'] = 2 # and areal overlap is set to weight 2

 species_list[[species_name]] = species_df
 }
```
#### Assigning probabilities as attribute scores
One of the main features of the NOAA FIT PSA is the possibility of probabilistic scoring. Instead of an attribute being assigned a single definitive category ("low", "moderate" or "high"), probabilities can be assigned to each category, for each attribute and each species. For example, if we assume that the *r* for the species *Acanthurus bahianus* does not definitively place it in a specific category, but has a 50% probability of being assigned "moderate" productivity and 50% probability of being assigned "high" productivity, this can be done in this section (line 239): 

```
species_list[['Acanthurus bahianus']]['high'][species_list[['Acanthurus bahianus']]['attribute'] == 'weight', ] = 2
# weight can also be modified here for a single species if wished

species_list[['Acanthurus bahianus']]['mod'][species_list[['Acanthurus bahianus']]['attribute'] == 'r', ] = 0.5
# setting a probability of 0.5 of Acanthurus bahianus being in the 'mod' category according to 'r'

species_list[['Acanthurus bahianus']]['high'][species_list[['Acanthurus bahianus']]['attribute'] == 'r', ] = 0.5
# setting a probability of 0.5 of Acanthurus bahianus being in the 'high' category according to 'r'

```
Like attribute weights, this can be done to all species/attributes by using the same code and specifying the desired species/attributes. For example, if the same probabilistic scoring for *r* is done for areal overlap as well:

```
species_list[['Acanthurus bahianus']]['mod'][species_list[['Acanthurus bahianus']]['attribute'] == 'r', ] = 0.5

species_list[['Acanthurus bahianus']]['high'][species_list[['Acanthurus bahianus']]['attribute'] == 'r', ] = 0.5

species_list[['Acanthurus bahianus']]['mod'][species_list[['Acanthurus bahianus']]['attribute'] == 'area_over', ] = 0.5
# now, areal overlap also has a 0.5 probability of placing the species in the 'mod' category

species_list[['Acanthurus bahianus']]['high'][species_list[['Acanthurus bahianus']]['attribute'] == 'area_over', ] = 0.5
# # and a 0.5 probability of placing the species in the 'high' category

```
This probabilistic scoring will result in a bootstrapping-based confidende interval to productivity, susceptibility and vulnerability scores, which can be observed in the resulting plot and .csv file. 

#### Modifying vulnerability thresholds

In this routine, the default vulnerability thresholds are 1.8 between low and moderate vulnerability, and 2.2 between moderate and high vulnerability. This is done in this section (line 353):

```
vul_class = function(x) {
  ifelse(x >= 2.2, 'High', 
         ifelse(x > 1.8 & x < 2.2, 'Moderate', 'Low')) 
}
```
If, for example, you wish to modify these thresholds to 1.5 and 2.5, this can be done by changing the values in the conditionals:

```
vul_class = function(x) {
  ifelse(x >= 2.5, 'High',     # now, if vulnerability is equal or higher tan 2.5, it will be assigned as "high"
         ifelse(x > 1.5 & x < 2.5, 'Moderate', 'Low'))  # and if vulnerability is between 1.5 and 2.5, it will be "moderate",
# otherwise it will be "low"
}
```
### Resources

Main reference used in this PSA approach: *Patrick, W. S., Spencer, P., Ormseth, O. A., Cope, J. M., Field, J. C., Kobayashi, D. R., ... & Lawson, A. (2009). Use of productivity and susceptibility indices to determine stock vulnerability, with example applications to six US fisheries* 
 
NOAA FIT (Fisheries Integrated Toolbox) PSA Web Application: https://nmfs-ost.github.io/noaa-fit/PSA.

PSA code & probabilistic scoring sourced from: https://github.com/nathanvaughan1/PSA
