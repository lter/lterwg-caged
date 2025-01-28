# "Boatyard" Scripts

The script(s) in this folder exist to handle pernicious idiosyncracies of certain datasets. While most datasets are 'harmonize-able' via the column key method (see `?ltertools::harmonize`) some files are so strangely formatted in their archived forms that special pre-harmonization steps must be performed before they can be harmonized with the rest of the data.

So, the workflow for these files is as follows:

1. Data are identified as either acceptable (and placed in a "data" folder on Google Drive) or unacceptable (and placed in a "purgatory" folder)
2. "Purgatory" data are downloaded and the bare minimum of needed tidying steps are performed
    - By the scripts in this folder
3. Repaired purgatory files are then re-uploaded to the Drive in the "data" folder
4. Harmonization then uses all files in the "data" folder (whether they were placed there originally or were uploaded there after some amount of processing in the 'boatyard')
