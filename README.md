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
- `06_calc-mean-beta-diff` -- Summarize beta dispersion within experiments and calculate difference between mean _caged_ beta dispersion and mean _uncaged_ beta dispersion
- `07_attach-metadata` -- Join site-level metadata with data as well as join outputs of all `05` scripts and script `06`
- `08_prepare-for-stats` -- Calculate effect size and streamline data to only bits needed for current analysis

## Supplementary Resources

- LTER Scientific Computing Team [website](https://lter.github.io/scicomp/)
- NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)
- CAGED Working Group [Shared Drive](https://drive.google.com/drive/u/0/folders/0AFR2XIdw_sKbUk9PVA) 
