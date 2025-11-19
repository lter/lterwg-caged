#' @title Calculate Beta Deviation
#' 
#' @description Calculates beta deviation for provided community data. Adapted from Vannette 
#' & Fukami 2017.

calc_beta.dev <- function(compo = NULL, null.matrices = NULL){

  # Errors for 'compo'
  if(is.null(compo))
    stop("'compo' must be provided")
  
  # Errors for 'null.matrices'
  if(is.null(null.matrices))
    stop("'null.matrices' must be provided")

  ## Makes sure 'compo' is in the right format
  dim.compo <- length(dim(compo))
  if(dim.compo==0) 
    stop("'compo' does not have more than 1 dimension")
  if(dim.compo>2) 
    stop("This function does not know what to do when 'compo' has more than 2 dimensions")
  if(dim.compo==2) 
  {
    if(is.null(rownames(compo))) 
      rownames(compo) <- paste("site_", 1:nrow(compo), sep="")
    if(is.null(colnames(compo))) 
      colnames(compo) <- paste("sp_", 1:nrow(compo), sep="")
  } 
  
  ## Checks that values in the cells are all positive integers
  if(sum((compo - round(compo, 0)) != 0) > 0) 
    stop("This function cannot use species abundances that are not integers")
  if(sum(compo<0) > 0)    
    stop("This function cannot use negative species abundances")
  if(sum(is.na(compo)) > 0)    
    stop("This function cannot use NAs")

    
  fit <- vegdist(compo, method="bray")

emp.bdisp.dist <- betadisper(fit, group=rep(1, times=nrow(compo)),type="centroid")
emp.dist <- emp.bdisp.dist$distances

  ## Calculates density of individuals per site
  site.densities <- rowSums(compo)
  if(min(site.densities)==0) 
    warning("Some empirical sites seem to be empty of individuals")
  
  ## Calculates regional abundances per species - the regional SAD
  spp.abund <- colSums(compo)
  if(min(spp.abund)<=0) 
    warning("Some species have abundances of less than or equal to zero")
    
  ## Calculates species richness across all sites
  regional.richness <- length(which(spp.abund>0))
  
  ## Calculates species richness at each site
  site.richness <- rowSums(compo>0)
  mean.site.richness <- mean(site.richness, na.rm=TRUE)
  
  
  ## Calculates the number of null matrices to use
  null.N <- length(null.matrices)


  ## Creates empty objects to hold results from the null matrices
# need a matrix length (empirical) x width(# of null communities)
  rand.distances <- as.data.frame(matrix(NA, nrow=nrow(compo), ncol=null.N))
  rownames(rand.distances) <- rownames(compo)
  colnames(rand.distances) <- paste("null_", 1:null.N, sep="")


  for (i in 1:null.N)
  {

  spp.abund <- colSums(compo)

    ## Defines the focal null composition matrix
    null.compo <- as.matrix(null.matrices[[i]])

    ## Calculates null regional abundances per species - the regional SAD
    null.spp.abund <- colSums(null.compo)
    if(identical(null.spp.abund, spp.abund)==FALSE) 
      warning("Simulated and observed species total abundances are not identical")
    
    ## Calculates species richness across all sites
    null.regional.richness <- length(which(null.spp.abund>0))

    ## Calculates species richness at each site
    null.site.richness <- rowSums(null.compo>0)
    null.mean.site.richness <- mean(null.site.richness)

    ## Calculates null composition distances among all possible pairs of sites

      null.dist.details <- vegdist(null.compo, method="bray")   
    rand.bdisper <- betadisper(null.dist.details, group=rep(1, times=nrow(null.compo)),type="centroid")
 rand.distances[,i] <- rand.bdisper$distances
 
  }


# now use this to calculate the ses for each value 

ES <- as.data.frame(matrix(NA, nrow=nrow(compo)))
SES <- as.data.frame(matrix(NA, nrow=nrow(compo)), ncol=1)

ES <- (emp.dist-rowMeans(rand.distances))

for(i in 1:length(ES)){
  SES[i,1]<- ES[i]/sd(rand.distances[i,])
}

SES
}

# Note ----
# Adapting code below piece by piece.
## Can ignore 'basement' while this is in process

# Likely problems
## "vegetarian" (a dependency of the below function) is removed from CRAN as of 2021 and thus unavailable.


# BASEMENT ----


## DESCRIPTION: This is a modified version of the scripts used in Tello et al 2015 PlosONE, which also depends on functions defined by Tello et al. 2015
## Functions defined below are intended to generate a standardize effect size for beta diversity, based on empirical values compared to 
## values generated from null communities based on empirical data. Distance to the centroid values are calculated for the null communities and compared to 
## values calculated from the empirical community. 
##
## dependencies found here: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0121458#sec014

########################################
# code for the function beta_ses #######
########################################

calc_beta.dev <- function(compo, null.matrices){
  ## Opens required packages
  require(vegetarian)
  require(vegan)

  ## Makes sure 'compo' is in the right format
  dim.compo <- length(dim(compo))
  if(dim.compo==0) 
    stop("'compo' does not have more than 1 dimension")
  if(dim.compo>2) 
    stop("This function does not know what to do when 'compo' has more than 2 dimensions")
  if(dim.compo==2) 
  {
    if(is.null(rownames(compo))) 
      rownames(compo) <- paste("site_", 1:nrow(compo), sep="")
    if(is.null(colnames(compo))) 
      colnames(compo) <- paste("sp_", 1:nrow(compo), sep="")
  } 
  
  ## Checks that values in the cells are all positive integers
  if(sum((compo - round(compo, 0)) != 0) > 0) 
    stop("This function cannot use species abundances that are not integers")
  if(sum(compo<0) > 0)    
    stop("This function cannot use negative species abundances")
  if(sum(is.na(compo)) > 0)    
    stop("This function cannot use NAs")

    
  fit <- vegdist(compo, method="bray")

emp.bdisp.dist <- betadisper(fit, group=rep(1, times=nrow(compo)),type="centroid")
emp.dist <- emp.bdisp.dist$distances

  ## Calculates density of individuals per site
  site.densities <- rowSums(compo)
  if(min(site.densities)==0) 
    warning("Some empirical sites seem to be empty of individuals")
  
  ## Calculates regional abundances per species - the regional SAD
  spp.abund <- colSums(compo)
  if(min(spp.abund)<=0) 
    warning("Some species have abundances of less than or equal to zero")
    
  ## Calculates species richness across all sites
  regional.richness <- length(which(spp.abund>0))
  
  ## Calculates species richness at each site
  site.richness <- rowSums(compo>0)
  mean.site.richness <- mean(site.richness, na.rm=TRUE)
  
  
  ## Calculates the number of null matrices to use
  null.N <- length(null.matrices)


  ## Creates empty objects to hold results from the null matrices
# need a matrix length (empirical) x width(# of null communities)
  rand.distances <- as.data.frame(matrix(NA, nrow=nrow(compo), ncol=null.N))
  rownames(rand.distances) <- rownames(compo)
  colnames(rand.distances) <- paste("null_", 1:null.N, sep="")


  for (i in 1:null.N)
  {

  spp.abund <- colSums(compo)

    ## Defines the focal null composition matrix
    null.compo <- as.matrix(null.matrices[[i]])

    ## Calculates null regional abundances per species - the regional SAD
    null.spp.abund <- colSums(null.compo)
    if(identical(null.spp.abund, spp.abund)==FALSE) 
      warning("Simulated and observed species total abundances are not identical")
    
    ## Calculates species richness across all sites
    null.regional.richness <- length(which(null.spp.abund>0))

    ## Calculates species richness at each site
    null.site.richness <- rowSums(null.compo>0)
    null.mean.site.richness <- mean(null.site.richness)

    ## Calculates null composition distances among all possible pairs of sites

      null.dist.details <- vegdist(null.compo, method="bray")   
    rand.bdisper <- betadisper(null.dist.details, group=rep(1, times=nrow(null.compo)),type="centroid")
 rand.distances[,i] <- rand.bdisper$distances
 
  }


# now use this to calculate the ses for each value 

ES <- as.data.frame(matrix(NA, nrow=nrow(compo)))
SES <- as.data.frame(matrix(NA, nrow=nrow(compo)), ncol=1)

ES <- (emp.dist-rowMeans(rand.distances))

for(i in 1:length(ES)){
  SES[i,1]<- ES[i]/sd(rand.distances[i,])
}

SES
}





##################################################################
########### Example usage of beta ses function  ##################
##################################################################

##   dat - a matrix or data frame where rows are sites and columns species. Values within the 
##   matrix represent abundances as integers. The function will not work with presence/absence 
##   matrices or measures of abundance other than number of individuals.
## 
##   In Vannette & Fukami "Dispersal enhances beta diversity in nectar microbes", submitted, separate null communities were calculated for each dispersal treatment 
##   separately (e.g. bagged, caged, and exposed). Resulting SES values were compared, using the SES for each sample as a replicate point. 


# define list to populate below
# beta.list <- list()

#   rand.results <- assemblages.from.pool.randA(compo=t(otu_table(dat)), fix.local.abund=TRUE, fix.rSAD=TRUE, 
#   rand.N=N, save.output=FALSE)
#   null.compos <- rand.results$rand.datasets
#  null.matrices <- null.compos

# compo<- t(otu_table(dat))
# beta.list [[i]] <- beta.ses(compo, null.matrices)














