# Consumer Absence Generates Ecological Dissimilarity (CAGED)

CAGED: A cross-ecosystem synthesis exploring the consequences of consumer loss on community variability

Principal Investigators:
- Jamie McDevitt-Irwin
- Kelly Speare
- Sally Koerner

## Script Explanations

Briefly describe the purpose of each script (or folder of scripts) here as you create them!

- `01_harmonize.R` -- Accepts all raw data files (from "data" folder in Google Drive) and uses a column key method (see `?ltertools::harmonize`) to combine them into a single, standardized data table
    - Performs some minor wrangling necessary for coalescing synonymous columns / etc.
- `02_quality-control.R` -- Performs more involved quality control (QC) and metric calculation
- `03_filter.R` -- Filters out rows/columns that made sense to harmonize & tidy but are likely not useful for (the current) analysis
- `04_calc-beta.R` -- Calculate beta dispersion at various spatial/temporal scales

## Supplementary Resources

LTER Scientific Computing Team [website](https://lter.github.io/scicomp/) & NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)

[Google Drive](https://drive.google.com/drive/u/1/folders/0AFR2XIdw_sKbUk9PVA) 

