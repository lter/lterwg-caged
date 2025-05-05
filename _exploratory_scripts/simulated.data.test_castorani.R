# Simulate community data to explore whether zero-filling, joint zeroes, and the species pool is affecting Bray-Curtis dissimilarity and beta dispersion

# Max Castorani
# May 5, 2025

# Clear working directory
rm(list = ls())

# Load required packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(vegan)

# Set seed for reproducibility
set.seed(123)

#---------------------------------------------------------------------------------------------------------------------
# SIMULATION 1: BASELINE CASE WITH SAME SPECIES POOL BETWEEN TREATMENTS

# Parameters
num_species <- 10
num_plots_per_treatment <- 10
treatments <- c("control", "treatment")
zero_prob <- 0.3  # Probability of true zeros

# Define mean abundance per species for each treatment (to control average composition)
# Treatment has higher abundance for species 6-10 and lower for 1-5, compared to control
x1 <- 5
x2 <- 2
y1 <- 1
y2 <- 12

lambda_control <- c(rep(x1, num_plots_per_treatment/2), 
                    rep(x2, num_plots_per_treatment/2))
lambda_treatment <- c(rep(y1, num_plots_per_treatment/2), 
                      rep(y2, num_plots_per_treatment/2))

# Optionally add variation in *within-group* (among plots or replicates) variance: make treatment group more variable
treatment_variability_factor <- 3  # Increase variance by scaling lambda randomly

# Create plot metadata
plots <- expand.grid(
  plot = 1:num_plots_per_treatment,
  treatment = treatments
)
plots$plot_id <- paste(plots$treatment, plots$plot, sep = "_")

# Initialize abundance matrix
abundance_matrix <- matrix(NA, nrow = nrow(plots), ncol = num_species)

# Simulate data for each plot
for (i in 1:nrow(plots)) {
  trt <- plots$treatment[i]
  if (trt == "control") {
    lambda_vec <- lambda_control
  } else {
    # Add more within-treatment variation
    lambda_vec <- lambda_treatment * runif(num_species, 1, treatment_variability_factor)
  }
  
  # Draw from Poisson
  abundances <- rpois(num_species, lambda = lambda_vec)
  
  # Apply zero-inflation
  zero_mask <- runif(num_species) < zero_prob
  abundances[zero_mask] <- 0
  
  abundance_matrix[i, ] <- abundances
}

# Assign column names
colnames(abundance_matrix) <- paste0("species_", 1:num_species)

# Combine with metadata
community_data <- cbind(plots[, c("plot_id", "treatment")], abundance_matrix)

# Remove DIFFERENT species from each treatment
# Define the species to be excluded in each treatment
missing_species_control <- paste0("species_", 1:4)
missing_species_treat <- paste0("species_", 5:8)

##Alternative scenario
#missing_species_control <- paste0("species_", 1:2)
#missing_species_treat <- paste0("species_", 3:4)

# Extract the full species data
species_data <- community_data[, grep("^species_", names(community_data))]

# Modify control treatment: set missing species to zero
control_data <- community_data %>% filter(treatment == "control")
control_data[, missing_species_control] <- 0

# Modify treatment: set missing species to zero
treat_data <- community_data %>% filter(treatment == "treatment")
treat_data[, missing_species_treat] <- 0

# Recombine the data (control + treatment)
community_data<- rbind(control_data, treat_data)

# Check community dataframe
head(community_data)

# Reshape to long format
community_long <- community_data %>%
  pivot_longer(cols = starts_with("species_"),
               names_to = "species",
               values_to = "abundance")

# Plot histograms, faceted by treatment
ggplot(community_long, aes(x = abundance, fill = treatment)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~treatment, nrow = 2) +
  scale_fill_manual(values = c("control" = "steelblue", "treatment" = "darkorange")) +
  labs(title = "Histogram of Species Abundances by Treatment",
       x = "Abundance", y = "Frequency") +
  theme_minimal()

# Calculate Bray-Curtis dissimilarity and beta dispersion

# Extract just the species data
species_data <- dplyr::select(community_data, starts_with("species_"))

# Calculate Bray-Curtis dissimilarity matrix
bray_dist <- vegdist(species_data, method = "bray")

# Create treatment factor
treatment_factor <- community_data$treatment

# Calculate beta dispersion using calc_betadisp (same as betadisper)
beta_disp <- betadisper(bray_dist, group = treatment_factor)

# Extract scores for points and centroids
scores_df <- as.data.frame(scores(beta_disp, display = "sites"))
scores_df$treatment <- treatment_factor

# Ellipse plot
ggplot(scores_df, aes(x = PCoA1, y = PCoA2, color = treatment)) +
  stat_ellipse(type = "t", size = 1.2) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("control" = "steelblue", "treatment" = "darkorange")) +
  labs(title = "Beta Dispersion Ordination (Bray-Curtis)",
       x = "PCoA Axis 1", y = "PCoA Axis 2") +
  theme_minimal()

# Extract distances to centroid
distances_df <- data.frame(
  distance = beta_disp$distances,
  treatment = treatment_factor
)

ggplot(distances_df, aes(x = treatment, y = distance, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("control" = "steelblue", "treatment" = "darkorange")) +
  labs(title = "Distance to Centroid (Beta Dispersion)",
       y = "Distance", x = "Treatment") +
  theme_minimal()

# Test for differences in dispersion
anova(beta_disp)

# Permutation test 
perm <- permutest(beta_disp, permutations = 999)
print(perm)



#---------------------------------------------------------------------------------------------------------------------
# SIMULATION 2: CONSIDER SCENARIO WHERE ABSENT SPECIES ARE VS. ARE NOT INCLUDED IN THE DATA FRAME
# Test subtle but important data handling issue in community ecology: whether to include or exclude species that are absent from all plots in one treatment, and how that affects dissimilarity metrics and multivariate dispersion.

# Scenario 1 – Inclusive: All species in the pool (species_1 to species_20) are included as columns.
# Some species may be all-zero in a treatment, but they're still in the data frame.

# Scenario 2 – Treatment-specific pruning:
# Each treatment gets its own species matrix. 
# Species absent in all replicates within a treatment are excluded from that treatment’s dataset.
# Then the two treatment-specific matrices are recombined before analysis.

# Compare the Bray-Curtis dissimilarities and beta dispersion from each scenario.


# Scenario 1: We have already done this
species_all <- community_data[, grep("^species_", names(community_data))]
bray_all <- vegdist(species_all, method = "bray")
beta_all <- betadisper(bray_all, group = community_data$treatment)


# Scenario 2: Treatment-specific pruning (exclude species with all zeroes per group)
# Separate treatments

# Extract the full species data
species_data <- community_data[, grep("^species_", names(community_data))]

# Modify control treatment: set missing species to zero
control_data <- community_data %>% filter(treatment == "control")
control_data[, missing_species_control] <- 0

# Modify treatment: set missing species to zero
treat_data <- community_data %>% filter(treatment == "treatment")
treat_data[, missing_species_treat] <- 0

# Remove species that are all zero within each group
control_species <- control_data[, grep("^species_", names(control_data))]
control_nonzero <- control_species[, colSums(control_species) > 0]

treat_species <- treat_data[, grep("^species_", names(treat_data))]
treat_nonzero <- treat_species[, colSums(treat_species) > 0]

# Get union of column names (species present in either group)
all_species <- union(colnames(control_nonzero), colnames(treat_nonzero))

# Function to ensure all species are present (adding missing ones with zeros)
ensure_species <- function(df, all_species) {
  missing <- setdiff(all_species, colnames(df))
  if (length(missing) > 0) {
    for (s in missing) {
      df[[s]] <- 0
    }
  }
  # Reorder columns to match all_species
  df <- df[, all_species]
  return(df)
}

# Apply to both control and treatment
control_mat <- ensure_species(control_nonzero, all_species)
treat_mat <- ensure_species(treat_nonzero, all_species)

missing_in_control <- setdiff(all_species, colnames(control_nonzero))
control_mat[, missing_in_control] <- 0

missing_in_treat <- setdiff(all_species, colnames(treat_nonzero))
treat_mat[, missing_in_treat] <- 0

# Make sure column order is the same
control_mat <- control_mat[, sort(colnames(control_mat))]
treat_mat   <- treat_mat[, sort(colnames(treat_mat))]

# Combine back
species_pruned <- rbind(control_mat, treat_mat)
treatment_vector <- c(rep("control", nrow(control_mat)), rep("treatment", nrow(treat_mat)))

# Calculate distances and beta dispersion
bray_pruned <- vegdist(species_pruned, method = "bray")
beta_pruned <- betadisper(bray_pruned, group = treatment_vector)

# Compare mean distances to centroid
tapply(beta_all$distances, community_data$treatment, mean)
tapply(beta_pruned$distances, treatment_vector, mean)

#Compare beta dispersion statistically
anova(beta_disp) #This is the original beta dispersion
anova(beta_all)  #This is the same as the original beta dispersion
anova(beta_pruned) #This is the "pruned" beta dispersion

# Permutation test for Inclusive (All Species)
perm_all <- permutest(beta_all, permutations = 999)
print(perm_all)

# Permutation test for Pruned (No All-Zero Species)
perm_pruned <- permutest(beta_pruned, permutations = 999)
print(perm_pruned)


# Calculate ordination scores for both scenarios
scores_all <- as.data.frame(scores(beta_all, display = "sites"))
scores_all$treatment <- community_data$treatment
scores_all$scenario <- "Inclusive (All Species)"

scores_pruned <- as.data.frame(scores(beta_pruned, display = "sites"))
scores_pruned$treatment <- community_data$treatment
scores_pruned$scenario <- "Pruned (No All-Zero Species)"

# Combine for plotting
scores_combined <- rbind(scores_all, scores_pruned)

# Calculate distances to centroid for both scenarios
dist_all <- data.frame(
  distance = beta_all$distances,
  treatment = community_data$treatment,
  scenario = "Inclusive (All Species)"
)

dist_pruned <- data.frame(
  distance = beta_pruned$distances,
  treatment = community_data$treatment,
  scenario = "Pruned (No All-Zero Species)"
)

# Combine
distances_combined <- rbind(dist_all, dist_pruned)

ggplot(scores_combined, aes(x = PCoA1, y = PCoA2, color = treatment)) +
  stat_ellipse(type = "t", size = 1.1) +
  geom_point(size = 2.8, alpha = 0.8) +
  scale_color_manual(values = c("control" = "steelblue", "treatment" = "darkorange")) +
  facet_wrap(~ scenario) +
  labs(title = "Beta Dispersion by Ordination (Bray-Curtis)",
       x = "PCoA Axis 1", y = "PCoA Axis 2", color = "Treatment") +
  theme_minimal()

ggplot(distances_combined, aes(x = treatment, y = distance, fill = treatment)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("control" = "steelblue", "treatment" = "darkorange")) +
  facet_wrap(~ scenario) +
  labs(title = "Distance to Centroid (Beta Dispersion)",
       x = "Treatment", y = "Distance") +
  theme_minimal()

# CONCLUSION
# Beta dispersion may differ subtly depending on how species absences are handled—especially if certain species occur only in one treatment.

# Why These Might Be Similar:
  # 1. Bray-Curtis Ignores Joint Absences
  # 2. Bray-Curtis dissimilarity does not count shared zeroes between samples in its calculation. So, species that are entirely absent from one treatment (and set to zero across all plots) may not influence dissimilarity much because in the inclusive case, they're 0 everywhere in that treatment, and in the pruned case, they're simply gone. Either way, pairwise comparisons between plots don't change meaningfully.
  # 3. Dissimilarity is Driven by Present Species. Differences in Bray-Curtis are more influenced by abundance patterns among present species, not species that are absent everywhere in a treatment. 
  # 4. Beta Dispersion Focuses on Within-Group Spread. The betadisper() analysis is about the variability among plots within each treatment. If species with all-zero values in one treatment are simply noise (not affecting variance among replicates), excluding them won’t shift the group’s multivariate spread much.

# We could get stronger divergence between inclusive vs. pruned approaches if:
  # 1. The number of treatment-specific absent species is large relative to total species richness.
  # 2. These “missing” species were non-zero in the other treatment and drove differences in between-treatment composition.
  # 3. We are using a presence-absence metric like Jaccard, where zeros have more influence.


#---------------------------------------------------------------------------------------------------------------------
# SIMULATION 3: ADD SPECIES TO BOTH TREATMENTS AT ZERO ABUNDANCE

# Add 10 all-zero species
num_new_species <- 10
zero_species <- matrix(0, nrow = nrow(community_data), ncol = num_new_species)
colnames(zero_species) <- paste0("species_", 
                                 seq(num_species + 1, num_species + num_new_species))

# Combine with original data
community_data_extended <- cbind(community_data, zero_species)

# Extract new species data (species_1 to species_20)
species_data_extended <- community_data_extended[, grep("^species_", names(community_data_extended))]

# Recalculate Bray-Curtis distance
bray_dist_extended <- vegdist(species_data_extended, method = "bray")

# Beta dispersion
beta_disp_extended <- betadisper(bray_dist_extended, group = community_data_extended$treatment)

# Ordination scores
scores_df_ext <- as.data.frame(scores(beta_disp_extended, display = "sites"))
scores_df_ext$treatment <- community_data_extended$treatment

ggplot(scores_df_ext, aes(x = PCoA1, y = PCoA2, color = treatment)) +
  stat_ellipse(type = "t", size = 1.2) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("control" = "steelblue", "treatment" = "darkorange")) +
  labs(title = "Beta Dispersion with Extra Zero-Only Species",
       x = "PCoA Axis 1", y = "PCoA Axis 2") +
  theme_minimal()

# Distances to centroid
distances_df_ext <- data.frame(
  distance = beta_disp_extended$distances,
  treatment = community_data_extended$treatment
)

ggplot(distances_df_ext, aes(x = treatment, y = distance, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("control" = "steelblue", "treatment" = "darkorange")) +
  labs(title = "Distance to Centroid with Zero-Only Species",
       y = "Distance", x = "Treatment") +
  theme_minimal()

# CONCLUSION: Bray-Curtis ignores shared zeros, so adding zero-only species does NOT affect dissimilarity values or the beta dispersion — a useful check on the  analytical pipeline.

