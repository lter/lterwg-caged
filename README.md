# Consumer Absence Generates Ecological Dissimilarity (CAGED)

CAGED: A cross-ecosystem synthesis exploring the consequences of consumer loss on community variability

Principal Investigators:
- Jamie McDevitt-Irwin
- Kelly Speare
- Sally Koerner

## Script Explanations

- `000_group-member-shortcut` -- Download raw data, the data key, site-level metadata, or tidy outputs of core workflow scripts from the group's Shared Drive
    - Requires access to Drive & authentication with the `googledrive` R package
- `00_setup` -- Do all setup tasks used by 2 or more other scripts (e.g., create folders) 
- `01_harmonize` -- Standardize all raw data format using data key and combine into single output
    - See `?ltertools::standardize` for details
- `02_quality-control` -- Do general QC of data
    - E.g., standardize treatments/dates, ensure unique design level designations, identify study years
- `03_filter` -- Remove rows that are not useful for the current project scope
    - E.g., all but last year of sampling, no confounding treatments
- `04_zero-fill` -- Zero fill community data
    - Assumes any taxon identified in only one replicate of an experiment is truly absent from other reps of the same experiment
- `05-A_calc-beta` -- Calculate beta dispersion at various spatial/temporal scales
- `05-B_calc-gamma` -- Calculate gamma diversity within experiment across and within treatment
- `05-C_calc-alpha` -- Calculate alpha diversity (richness) at all design levels
- `05-D_calc-dominance` -- Calculate dominance (Berger Parker) at all design levels
- `06-A_compare-treat-mean-beta` -- Calculate mean difference and log response ratios (LRR) between caging treatment beta dispersions
- `06-B_compare-treat-gamma` -- Calculate difference & LRR for between caging treatment gamma richness
- `06-C_compare-treat-alpha` -- Calculate difference & LRR for between caging treatment alpha diversity (i.e., richness)
- `06-D_compare-treat-dominance` -- Calculate difference & LRR for between caging treatment dominance
- `07_tidy-metadata` -- Do QC on metadata file and check for mismatches with beta dispersion data
- `08_join-data` -- Join site-level metadata, beta dispersion (un-averaged), gamma richnes, alpha richness, dominance, and treament/experiment means of beta dispersion
- `08_prepare-for-stats` -- Calculate effect size and streamline data to only bits needed for current analysis

## Supplementary Resources

- LTER Scientific Computing Team [website](https://lter.github.io/scicomp/)
- NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)
- CAGED Working Group [Shared Drive](https://drive.google.com/drive/u/0/folders/0AFR2XIdw_sKbUk9PVA) 
