## --------------------------------------------------------------- ##
# CAGED Beta Dispersion Calculation Function
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

# Purpose
## Calculate beta dispersion for provided (long-format) community data
## Returns dataframe with number of replicates as well as beta dispersion value


# Define function
calc_betadisp <- function(df = NULL, floor = 4,
                          taxa_col = NULL, abun_col = NULL,
                          dist_method = "bray", result_prefix = "result"){
  ## `df` = long-format dataframe of community comp.
  ## `floor` = minimum number (inclusive) of replicates for which to calculate beta dispersion
  ## `taxa_col` = name of column in 'df' containing taxonomic ID
  ### (e.g., species, functional group, etc.)
  ## `abun_col` = name of column in 'df' containing abundance values
  ## `dist_method` = distance method shorthand accepted by `vegan::vegdist`
  ## `result_prefix` = result columns' prefix
  ### (used to informatively add outputs to input data object)
  
  # Error if missing data or df is not provided as a dataframe-like object
  if(is.null(df) || "data.frame" %in% class(df) != T)
    stop("'df' must be provided as a dataframe-like object")
  
  # Error if floor is missing and/or not numeric and/or more than one value is provided
  if(is.null(floor) || is.numeric(floor) != T || length(floor) != 1)
    stop("'floor' must be provided as a single number")
  
  # Error if taxon/abundance columns are not found in the data
  if(taxa_col %in% names(df) != T)
    stop("'taxa_col' must exactly match a column name in 'df'")
  if(abun_col %in% names(df) != T || is.numeric(df[[abun_col]]) != T)
    stop("'abun_col' must exactly match a column name in 'df' and be numeric")
  
  # Identify all column names other than the taxon/abundance columns
  misc_cols <- setdiff(x = names(df), y = c(taxa_col, abun_col))
  
  # Create the first bit of the output data object
  beta_out <- df %>% 
    # Keep all non-vital columns
    dplyr::select(dplyr::all_of(misc_cols)) %>% 
    # Drop non-unique rows (likely as many as there were species IDs)
    dplyr::distinct()
  
  # Get a wide format variant of the community data
  comm_wide <- df %>% 
    tidyr::pivot_wider(names_from = {{taxa_col}},
                       values_from = {{abun_col}},
                       values_fill = 0) %>% 
    dplyr::select(-dplyr::all_of(misc_cols))
  
  # Identify number of replicates
  reps <- nrow(comm_wide)
  
  # Calculate beta dispersion _if_ there are enough reps
  if(reps >= floor){
    
    # Get distance/dissimilarity matrix
    dist_val <- vegan::vegdist(x = comm_wide, method = "bray")
    
    # Calculate beta dispersion
    beta_val <- vegan::betadisper(d = dist_val, group = as.factor(rep(x = "x", times = reps)),
                              type = "centroid", bias.adjust = F, 
                              sqrt.dist = F, add = F)
    
    
    # Otherwise, give back null values
  } else {
    dist_val <- NULL
    beta_val <- NULL
  }
  
  # Add replicate number & beta dispersion value to output
  beta_out[[paste0(result_prefix, ".n")]] <- reps
  beta_out[[paste0(result_prefix, ".betadisp")]] <- ifelse(length(dist_val) > 0,
                                                           yes = beta_val$distances,
                                                           no = NA_real_)
  
  # Return that object
  return(beta_out)
  
} # Close function
