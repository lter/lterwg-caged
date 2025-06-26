#' @title Tidy Beta Dispersion Data Table
#' 
#' @description Tidies beta dispersion calculated for a community by `calc_betadisp` custom function. Assumes several specific characteristics of data structure so _should only be used on data returned by `calc_betadisp`_
#' 
#' @param beta (dataframe-like) beta dispersion data calculated by `calc_betadisp`
#' @param result_prefix (character) prefix corresponding to beta dispersion calculation result columns
#' 
#' @return (dataframe-like) tidied beta dispersion data with intuitively-named columns
#' 
tidy_betadisp <- function(beta = NULL, result_prefix = NULL){
  
  # Error if missing data or df is not provided as a dataframe-like object
  if(is.null(beta) || "data.frame" %in% class(beta) != T)
    stop("'beta' must be provided as a dataframe-like object")
  
  # Error if replicate column is not found in the data
  if(result_prefix %in% names(beta) != T)
    stop("'result_prefix' must exactly match a column name in 'beta'")
  
  tidy_beta <- beta %>% 
    # Pivot sample size into long format
    tidyr::pivot_longer(cols = dplyr::all_of(paste0(result_prefix, ".n")),
                        names_to = "betadisp.design.level",
                        values_to = "betadisp.sample.size") %>% 
    # Tidy contents of the new design level column
    dplyr::mutate(betadisp.design.level = ifelse(betadisp.design.level != "exp.name.n",
                                                 yes = gsub("\\.n", "", x = betadisp.design.level),
                                                 no = stringr::str_sub(betadisp.design.level,
                                                                       start = 1, end = 8))) %>% 
    # Rename median and community distance columns
    supportR::safe_rename(data = ., 
                          bad_names = c(paste0(result_prefix, ".betadisp.median"),
                                        paste0(result_prefix, ".betadisp.site.dist")),
                          good_names = c("betadisp.median", "betadisp.comm.dist")) %>% 
    # Keep only unique columns
    dplyr::distinct() %>% 
    # Reorder columns slightly
    dplyr::relocate(betadisp.median, betadisp.comm.dist,
                    .after = betadisp.sample.size)
  
  # Return that
  return(tidy_beta)
  
} # Close function

# End ----
