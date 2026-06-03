## Productivity-Susceptiblity Analysis (PSA) for multiple species/stocks at once in batch mode
This is an R shiny app for rapidly running Productivity-Susceptibility Analysis (PSA) for multiple species/stocks at once in a simple batch processing loop. The code for the PSA itself is mostly sourced from the [PSA Web Application at the Fisheries Integrated Toolbox (FIT)](https://nmfs-ost.github.io/noaa-fit/PSA) by the U. S. National Oceanic and Atmospheric Administration (NOAA), developed by Dr. Nathan Vaughan & Dr. Jason Cope. Like in the NOAA PSA Web Application, this routine allows for probabilistic attribute scoring and generates bootstrapping-based confidence intervals if scored using probabilities. This app also adds the possibility of directly retrieveing meta-analytical ([FishLife](https://github.com/James-Thorson-NOAA/FishLife)) attributes for probabilistic socring based on prediction densities. To learn more about FishLife, see [here](https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/eap.1606) and [here](https://onlinelibrary.wiley.com/doi/abs/10.1111/faf.12427).

### Instructions
#### Running the app
This app is built in the R language and runs locally. To use it, you must download and install the latest version of [R](https://www.r-project.org/). You can then download the [R script file](https://github.com/adossantos-jr/psa-batch-mode/blob/main/shiny_psa_batch_mode.R) and run it. 

Using an Integrated Development Environment (IDE) such as [RStudio](https://posit.co/download/rstudio-desktop/) is recommended. If you have the script open in RStudio or a similar IDE, press Ctrl+A to select the whole script and then Ctrl+Enter to run it. The app window should appear then. 

All required packages should be installed/loaded automatically. When you first run this app, the automatic package installation may take up to several minutes depending on your internet connection and what packages you already have installed. After that, it should take a few seconds.

#### Using the app

This routine requires a simple data frame with species as rows and attributes as columns. Columns must be named after the column names in the [test data](https://github.com/adossantos-jr/psa-batch-mode/blob/main/test_psa_data.csv). Columns must be: 

- `species`: The name of the species/stocks being evaluated;
 - `r`: Intrinsic rate of population growth;
- `tmax`: Maximum age;
 - `lmax`: Maxium length;
- `k`: von Bertalanffy growth coefficient;
-  `m`: Natural mortality rate; 
- `fec`: Measured fecundity;
- `breed`: Winemiller's index (breeding strategy quantificaton);
- `rec`: Frequency of recruitment;
- `tmat`: Age at maturity;
- `troph`: Mean trophic level;
- `area_over`: Areal overlap;
- `geog_conc`: Geographical concentration;
- `vert_over`: Vertical overlap;
- `seas_migr`: Seasonal migrations affecting capture;
- `school`: Schooling (or similar behaviors) affecting capture;
- `morph`: Morphology affecting capture;
- `desire`: Desirability of the species/stock;
- `mng_strat`: Management strategy;
- `f_over_m`: Fishing mortality in relation to natural mortalilty;
- `b_over_b0`: Stock biomass in relation to "virgin" biomass;
- `surv_prob`: Survival probability;
- `hab_impact`: Habitat impact of the fishery.

These attributes follow [Patrick et al.(2009)](https://media.fisheries.noaa.gov/dam-migration/ns1-patrick-et-al-2010.pdf); you do not need to score all of them to run a PSA. Due to their qualitative nature, some of the columns are categorical and must be filled according to the respective functions in the script (i. e. cat_morph requires categories 'high_selec', 'mod_selec' & 'low_selec'); examples on are available in the [test data](https://github.com/adossantos-jr/psa-batch-mode/blob/main/test_psa_data.csv). If you leave an attribute column empty, the script will assign the weight of that attribute to 0 and the attribute will not be included in the analysis. 

Data frame importing is done using the 'Browse" button (if you use software such as MS Excel to buid the data frame, remember to always convert it to a .csv file before importing to R. When you upload your data, attributes will be automatically scored based on the default thresholds. 

<img width="1091" height="690" alt="ui_psa" src="https://github.com/user-attachments/assets/23915d0e-c38d-4c5a-b4f7-0193040b3bc4" />


You can modify attribute and vulnerability thresholds if you wish in the left menu. If you want to use standard attribute and vulnerability thresholds, just scroll down and click the Run PSA  button.

<img width="1087" height="690" alt="ui_psa2" src="https://github.com/user-attachments/assets/daf0db2f-9a34-4767-9f11-74286cdb63de" />


And done! This is all needed to run a batch PSA with these standard thresholds and no probabilistic scoring. If you wish to modify the attribute thresholds, you can select the species, the attributes and assign the probababilities. You can batch autoscore productivity attributes with FishLife probabilistic densities for all species at one, or you can select attributes and species to autoscore with FishLife before running the PSA. Attributes that can be retrieved from FishLife to score productivity are `r`, `tmax`, `k`, `m` and `tmat`. 

Some graphical outputs should look like this:

<img width="525" height="750" alt="PSA_Plot_2026-06-02" src="https://github.com/user-attachments/assets/d3b22be2-f44b-4c1d-8762-d639c07d4ac8" />
<img width="750" height="375" alt="Proportions_Plot_2026-06-02" src="https://github.com/user-attachments/assets/22389277-433d-4d1e-b464-06debdee3c70" />


### Limitations
As of right now, this PSA shiny lacks the data quality index based on the number of attributes used. For now, when using this app, you should either use an equal number of attributes for all species, use the taxonomic level of FishLife estimates, or manually assign the data quality index.

### Resources

Main reference used in this PSA approach: [*Patrick, W. S., Spencer, P., Ormseth, O. A., Cope, J. M., Field, J. C., Kobayashi, D. R., ... & Lawson, A. (2009). Use of productivity and susceptibility indices to determine stock vulnerability, with example applications to six US fisheries*](https://media.fisheries.noaa.gov/dam-migration/ns1-patrick-et-al-2010.pdf) 
 
[NOAA FIT (Fisheries Integrated Toolbox) PSA Web Application](https://nmfs-ost.github.io/noaa-fit/PSA)

[FishLife tool](https://github.com/James-Thorson-NOAA/FishLife)



[PSA code & probabilistic scoring source](https://github.com/nathanvaughan1/PSA)
