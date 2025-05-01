# "Boatyard" Scripts

The script(s) in this folder exist to handle any problems that occur relatively rarely and/or don't need to be re-run frequently.

## Script Explanations

- `purgatory-pair.R` - Handles pernicious idiosyncracies of certain datasets. While most datasets are 'harmonize-able' via the column key method (see `?ltertools::harmonize`) some files are so strangely formatted in their archived forms that special pre-harmonization steps must be performed before they can be harmonized with the rest of the data. So, the workflow for these files is as follows:
    1. Data are identified as either acceptable (and placed in a "data" folder on Google Drive) or unacceptable (and placed in a "purgatory" folder)
    2. "Purgatory" data are downloaded and the bare minimum of needed tidying steps are performed (by the scripts in this folder)
    3. Repaired purgatory files are then re-uploaded to the Drive in the "data" folder
    4. Harmonization then uses all files in the "data" folder (whether they were placed there originally or were uploaded there after some amount of processing in the 'boatyard')
- `expand_key.R` - Identify "source" and "raw_name" columns for the data key for not-yet-harmonized data
    - Note this requires manual copy/pasting of these columns into the data key GoogleSheet but is otherwise automated
- `expand_metadata.R` - Identify data files that have been harmonized but are not included in the site-level metadata GoogleSheet
    - Note this requires manual copy/pasting of this script's output into the sitelevel metadata GoogleSheet but is otherwise automated
- `check_dropped-value.R` - For a given set of sequential data tables, identify where a particular value (from a column found in all tables) is dropped. Useful for QC purposes to ensure the workflow isn't randomly dropping certain values
    - Developed to check for lost datasets but is written to be generic to any value from any column
