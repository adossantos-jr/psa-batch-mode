## Multi-Species Productivity-Susceptiblity Analysis 

This is an R routine for rapidly performing Productivity-Susceptibility Analysis (PSA) for multiple species/stocks at once, adapted by Alexandre dos Santos Jr in collaboration with Dr Jason Cope. The code is mostly sourced from the U. S. National Oceanic and Atmospheric Administration (NOAA) Web Application for PSA (hosted at https://github.com/nathanvaughan1/PSA) by Dr Nathan Vaughan & Dr Jason Cope. 

### How to use

The routine requires a simple data frame with species as columns and attributes as rows. Columns must be named after the column names in the test data frame (test_psa_data.csv). It is OK to leave attribute values empty; those will have an assigned weight of 0 and not be accounted for in the analysis.

Modifications can be made in all steps of the PSA (some useful may be attribute category thresholds; vulnerability threshholds; attribute weighting; probabilistic attribute scoring). However, for a standard approach, only the input data is needed.

### Resources
Patrick, W. S., Spencer, P., Ormseth, O. A., Cope, J. M., Field, J. C., Kobayashi, D. R., ... & Lawson, A. (2009). Use of productivity and susceptibility indices to determine stock vulnerability, with example applications to six US fisheries.

PSA web application at NOAA FIT (Fisheries Integrated Toolbox): https://nmfs-ost.github.io/noaa-fit/PSA


For questions, suggestions and bug reports, please contact Alexandre dos Santos Jr at alexandre.ricardosantos@ufpe.br

