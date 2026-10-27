#' @title Calculate Beta Dispersion
#' 
#' @description Calculates beta dispersion for provided (long-format) community data.
#' 
#' @param df (dataframe-like) long-format dataframe of community comp.
#' @param floor (numeric) minimum number of replicates for which to calculate beta dispersion. Inclusive of provided value (e.g., `floor = 4` will calculate beta dispersion for n = 4 data)
#' @param group_col (character) name of column in 'df' containing groups (likely treatments)
#' @param taxa_col (character) name of column in 'df' containing taxonomic ID
#' @param abun_col (character) name of column in 'df' containing numeric abundance values. (e.g., species, functional group, etc.)
#' @param dist_method (character) distance method shorthand accepted by `vegan::vegdist`
#' @param result_prefix (character) prefix to use when creating results columns. This argument is used to define the start of new columns containing results (end of columns determined inside of function)
#' 
#' @return (dataframe-like) data object without taxa/abundance information but with added columns for sample size and beta dispersion.
#' 
calc_betadisp <- function(df = NULL, floor = 4, 
  group_col = NULL, taxa_col = NULL, abun_col = NULL,
  dist_method = "bray", result_prefix = "result"){
  
  # Error if missing data or df is not provided as a dataframe-like object
  if(is.null(df) || "data.frame" %in% class(df) != TRUE)
    stop("'df' must be provided as a dataframe-like object")
  
  # Error if floor is missing and/or not numeric and/or more than one value is provided
  if(is.null(floor) || is.numeric(floor) != TRUE || length(floor) != 1)
    stop("'floor' must be provided as a single number")

  # Error if needed columns are not found in the data
  if(group_col %in% names(df) != TRUE)
    stop("'group_col' must exactly match a column name in 'df'")
  if(taxa_col %in% names(df) != TRUE)
    stop("'taxa_col' must exactly match a column name in 'df'")
  if(abun_col %in% names(df) != TRUE || is.numeric(df[[abun_col]]) != TRUE)
    stop("'abun_col' must exactly match a column name in 'df' and be numeric")
  
  # Identify all column names other than the specifically needed columns
  misc_cols <- setdiff(x = names(df), y = c(group_col, taxa_col, abun_col))
  
  # Create the first bit of the output data object
  beta_out <- df %>% 
    # Keep grouping column + all non-vital columns
    dplyr::select(dplyr::all_of(c(group_col, misc_cols))) %>% 
    # Drop non-unique rows (likely as many as there were species IDs)
    dplyr::distinct()
  
  # Get a wide format variant of the community data
  df_wide <- tidyr::pivot_wider(df, names_from = {{taxa_col}}, 
    values_from = {{abun_col}}, values_fill = 0)
  
  # And a 'community only' variant
  comm_wide <- dplyr::select(df_wide, -dplyr::all_of(c(group_col, misc_cols)))
  
  # Identify number of replicates
  reps <- df_wide %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of({{group_col}}))) %>% 
    dplyr::summarize(ct = dplyr::n(),
      .groups = "drop")
  
  # Calculate beta dispersion _if_ there are enough reps
  if(nrow(comm_wide) >= floor){
    
    # Get distance/dissimilarity matrix
    dist_val <- vegan::vegdist(x = comm_wide, method = dist_method)
    
    # Calculate beta dispersion
    beta_val <- vegan::betadisper(d = dist_val, group = as.factor(df_wide[[group_col]]),
      type = "median", bias.adjust = FALSE, sqrt.dist = FALSE, add = FALSE)
    
    # Identify median & community-specific distances
    beta_median <- data.frame("groups" = names(beta_val$group.distances),
      "median" = as.numeric(beta_val$group.distances))
    beta_dists <- beta_val$distances
    
    # Otherwise, give back null values
  } else {
    dist_val <- NULL
    beta_val <- NULL
    beta_median <- data.frame("groups" = NA, "median" = NA)
    beta_dists <- NA
  }

  # Assemble some needed column names
  median_col <- paste0(result_prefix, ".betadisp.median")
  n_col <- paste0(result_prefix, ".n")

  # Median beta dispersion requires extra tweaks
  beta_median <- supportR::safe_rename(data = beta_median,
    bad_names = c("groups", "median"), good_names = c(group_col, median_col))
  
  # So does replicate counting
  beta_reps <- supportR::safe_rename(data = reps,
    bad_names = "ct", good_names = n_col)
  
  # Add critical information to output
  ## Number of replicates
  beta_out <- dplyr::left_join(x = beta_out, y = beta_reps,
    by = dplyr::join_by({{group_col}}))
  ## Per-community beta dispersion distance from median
  beta_out[[paste0(result_prefix, ".betadisp.site.dist")]] <- beta_dists
  ## Median beta dispersion
  beta_out <- dplyr::left_join(x = beta_out, y = beta_median,
    by = dplyr::join_by({{group_col}}))
  
  # Replace values below the relevant number of replicates with NA
  beta_final <- beta_out %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::contains("betadisp"),
      .fns = ~ ifelse({{n_col}} < floor, yes = NA, no = .)))
  
  # Return that object
  return(beta_final)
  
} # Close function
