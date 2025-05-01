# Exploratory Scripts

The script(s) in this folder exist for exploratory purposes. They are not critical to the core data processing workflow but may provide helpful diagnostics for parts of that workflow. Where possible, **exploratory scripts have the same zero-padded number as the _last_ core workflow script upon which they depend and a lowercase letter** (to differentiate the outputs of these scripts from those of the main workflow).

## Script Explanations

- `02a_per-dataset-abun-violins.R` - Creates violin plots of abundance against experiment name ("exp.name" column in QC'd data) for each dataset. File names are not date-stamped but a plot title is included in each graph that _is_ date-stamped.
- `05a_per-dataset-beta-boxplots.R` - Creates boxplots of beta dispersion against _standardized_ cage treatment (i.e., "caged", "partial", or "uncaged") for each dataset for which any beta dispersion was calculable. File names are not date-stamped but a plot title is included in each graph that _is_ date-stamped.
