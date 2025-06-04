## Productivity-Susceptiblity Analysis (PSA) for multiple species at once

This is an R routine for rapidly performing Productivity-Susceptibility Analysis (PSA) for multiple species/stocks at once in a simple way. The code for the PSA itself is mostly sourced from the U. S. National Oceanic and Atmospheric Administration (NOAA) Web Application for PSA (hosted at https://github.com/nathanvaughan1/PSA) developed by Dr. Nathan Vaughan & Dr. Jason Cope. Like in the NOAA PSA Web Application, this routine allows for probabilistic attribute scoring and generates a bootstrap-based confidence interval. 
### Instructions
#### Uploading your data
The routine requires a simple data frame with species as columns and attributes as rows. The first step is setting your working directory. This is done in this section of the routine:

```
  setwd('your wd')
```
Where 'your wd' must be replaced with a directory path. This directory is where the input data must be placed, and where the results will appear. An example of a working directory path in Windows is:
```
setwd('/Users/alexandre/Documents/multispecies_psa')
```
To better understand working directories in R, a comprehensive guide can be found at https://intro2r.com/work-d.html.

Columns must be named after the column names in the test data frame (test_psa_data.csv). 


Modifications can be made in all steps of the PSA. However, for a standard approach, only the input data is needed.

### Resources
Patrick, W. S., Spencer, P., Ormseth, O. A., Cope, J. M., Field, J. C., Kobayashi, D. R., ... & Lawson, A. (2009). Use of productivity and susceptibility indices to determine stock vulnerability, with example applications to six US fisheries.

PSA web application at NOAA FIT (Fisheries Integrated Toolbox): https://nmfs-ost.github.io/noaa-fit/PSA


