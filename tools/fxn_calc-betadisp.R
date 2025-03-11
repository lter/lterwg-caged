#' @title Calculate Beta Dispersion
#' 
#' @description Calculates beta dispersion for provided (long-format) community data.
#' 
#' @param df (dataframe-like) long-format dataframe of community comp.
#' @param floor (numeric) minimum number of replicates for which to calculate beta dispersion. Inclusive of provided value (e.g., `floor = 4` will calculate beta dispersion for n = 4 data)
#' @param taxa_col (character) name of column in 'df' containing taxonomic ID
#' @param abun_col (character) name of column in 'df' containing numeric abundance values. (e.g., species, functional group, etc.)
#' @param dist_method (character) distance method shorthand accepted by `vegan::vegdist`
#' @param result_prefix (character) prefix to use when creating results columns. This argument is used to define the start of new columns containing results (end of columns determined inside of function)
#' 
#' @return (dataframe-like) data object without taxa/abundance information but with added columns for sample size and beta dispersion.
#' 
calc_betadisp <- function(df = NULL, floor = 4,
                          taxa_col = NULL, abun_col = NULL,
                          dist_method = "bray", result_prefix = "result"){
  
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
    dist_val <- vegan::vegdist(x = comm_wide, method = dist_method)
    
    # Calculate beta dispersion
    beta_val <- vegan::betadisper(d = dist_val, group = as.factor(rep(x = "x", times = reps)),
                              type = "median", bias.adjust = F, 
                              sqrt.dist = F, add = F)
    
    # Identify median & community-specific distances
    beta_median <- beta_val$group.distances
    beta_dists <- beta_val$distances
    
    # Otherwise, give back null values
  } else {
    dist_val <- NULL
    beta_val <- NULL
    beta_median <- NA
    beta_dists <- NA
  }
  
  # Add critical information to output
  ## Number of replicates
  beta_out[[paste0(result_prefix, ".n")]] <- reps
  ## Median beta dispersion
  beta_out[[paste0(result_prefix, ".betadisp.median")]] <- beta_median
  ## Per-community beta dispersion distance from median
  beta_out[[paste0(result_prefix, ".betadisp.site.dist")]] <- beta_dists
  
  # Return that object
  return(beta_out)
  
} # Close function
