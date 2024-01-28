The file "RWCR.R" contains the R function "RWCR" for the variable selection of clustered data using the RWCR procedure. The file "Example.R" contains an example code to illustrate the usage of the RWCR function.

The RWCR function takes the following arguments: 

  #size: number of WCR data sets
  
  #y: response variable, can be either continuous or binary variable.
  
  #X: covariate matrix
  
  #id: cluster id
  
  #type: type of penalty function, either "lasso", "SCAD, or "MCP"
  
  #fm: error distribution, either "binomial" or "gaussian"
  
  #lambda: initial set of tuning parameter values
  
  #SIS: logical value indicating whether the SIS step is used
  
  #pi: candidate values of the threshold value of the selection probability
  
  #K: number of folds for cross-validation
