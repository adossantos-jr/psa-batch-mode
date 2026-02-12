## Productivity-Susceptiblity Analysis (PSA) for multiple species/stocks at once in batch mode
This is an R shiny app for rapidly performing Productivity-Susceptibility Analysis (PSA) for multiple species/stocks at once in a simple batch processing loop. The code for the PSA itself is mostly sourced from the [PSA Web Application at the Fisheries Integrated Toolbox (FIT)](https://nmfs-ost.github.io/noaa-fit/PSA) by the U. S. National Oceanic and Atmospheric Administration (NOAA), developed by Dr. Nathan Vaughan & Dr. Jason Cope. Like in the NOAA PSA Web Application, this routine allows for probabilistic attribute scoring and generates bootstrapping-based confidence intervals if scored using probabilities. 

### Instructions
#### Running the app
This app is built in the R language and runs on your local R server. To use it, you must download and install the latest version of [R](https://www.r-project.org/). You can then download the [R script file](https://github.com/adossantos-jr/psa-batch-mode/blob/main/shiny_psa_batch_mode.R) and run it on your terminal. 

Using an Integrated Development Environment (IDE) such as [RStudio](https://posit.co/download/rstudio-desktop/) is recommended. If you have the script open in RStudio or a similar IDE, press Ctrl+A to select the whole script and the Ctrl+Enter to run it. The app window should appear then. 

All required packages should be installed/loaded automatically. When you first run this app, the automatic package installation may take up to several minutes depending on your internet connection and what packages you already have installed. After that, it should take a few seconds.

#### Using the app

This routine requires a simple data frame with species as rows and attributes as columns. Columns must be named after the column names in the [test data](https://github.com/adossantos-jr/psa-batch-mode/blob/main/test_psa_data.csv). Columns must be: 

- `species`: The name of the species/stocks being evaluated;
 - `r`: Intrinsic rate of population growth;
- `tmax`: Maximum age;
 - `lmax`: Maxium length;
- `k`: Von Bertalanffy Growth Coefficient;
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

Data frame importing is done using the 'Browse" button (if you use software such as MS Excel to buid the data frame, remember to always convert it to a .csv file before importing to R.
A preview of your data frame in the app should appear like this:

<img width="868" height="585" alt="ui_psa" src="https://github.com/user-attachments/assets/f0dc3e84-6555-4ce7-aeb8-395d4c394b99" />


You can modify attribute and vulnerability thresholds if you wish in the left menu. If you want to use standard attribute and vulnerability thresholds, then just scroll down and click the Run Analysis button.

And done! This is all needed to run a batch PSA with these standard thresholds and no probabilistic scoring. A pop-window will appear with probabilistic scoring, but you can skip it if you wish. If you wish to modify the attribute thresholds, you can select the species, the attributes and assign the probababilities. 

- Remember, probability scoring should always sum to 1;
- Remember to always click on "Assign Probability" after each scoring.

The graphical output should look like this:

<img width="300" height="500" alt="plots_psa" src="https://github.com/user-attachments/assets/dd92d597-2319-4a4e-8fec-cd7f4bcca557" />

### Limitations
As of right now, this PSA shiny lacks the data quality index based on the number of attributes used. For now, when using this app, you should either use an equal number of attributes for all species, or manually assign the data quality index. The data quality index will be implemented in the future.

### Resources

Main reference used in this PSA approach: [*Patrick, W. S., Spencer, P., Ormseth, O. A., Cope, J. M., Field, J. C., Kobayashi, D. R., ... & Lawson, A. (2009). Use of productivity and susceptibility indices to determine stock vulnerability, with example applications to six US fisheries*](https://media.fisheries.noaa.gov/dam-migration/ns1-patrick-et-al-2010.pdf) 
 
[NOAA FIT (Fisheries Integrated Toolbox) PSA Web Application](https://nmfs-ost.github.io/noaa-fit/PSA)

[PSA code & probabilistic scoring source](https://github.com/nathanvaughan1/PSA)
