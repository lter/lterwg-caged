# Meta-Document

This document serves to identify macro-scale changes and judgement calls for easy revisiting / internal reminding.

## First Meeting - 2025/01/27-30
Overall Goal of this Meeting: 1) download data 2) create data key 3) harmonize data 4) create meta-data file  

We were able to start each of these three tasks (we didn't finish any of them) and so have a protocol for each.  

*Important Links*  
[Meeting agenda and notes](https://docs.google.com/document/d/1948uxU_8MAEoeU_nnuD_nZiY6FS9Zrgc2DkS0F-83xc/edit?tab=t.0)  
[Data Sources - Detailed](https://docs.google.com/spreadsheets/d/1Eg1mt-TPUgXqqPe8e8nhtAmKJ0NzWDbolfSVKmQf4o8/edit)  
[Harmonized Data Key](https://docs.google.com/spreadsheets/d/1SgdqsAl_yPArCaw8dCcP0KINL3Qp2qwN0ZJqWnIxGPk/edit?gid=0#gid=0)  
[Metadata](https://docs.google.com/spreadsheets/d/1vNMYj3-xO_tmIhniGyOuj1pG4DQt9fMRAuQipPE3hys/edit?gid=0#gid=0)    
[Metadata Instructions](https://docs.google.com/document/d/1gnFHVtSg-F6A3v3lRcJr1pGRxlhtWLQNRr4mCgxqMMQ/edit?tab=t.0)  

### Data Inclusion / Exclusion
We will include data that 1) excludes consumers and compares to a control with consumer access, 2) measures community structure and abundance, 3) has a minimum sample size of four, and 4) is conducted in field habitats.  


### Scientific / Domain Choices
- We will take the lowest level of taxonomic organization (e.g, if they provide functional groups and species, we will take species). We will take data at any level as long as there is community structure.(https://github.com/lter/lterwg-caged/issues/3)
- We will take the lowest level of spatial organization (even if its technically pseudoreplication) (e.g., plots within side a large exclosure that is typical of terrestrial systems)(https://github.com/lter/lterwg-caged/issues/6)
- We are considering sites/locations with separate experiments as separate datapoints in our project (i.e., exp.name) only if the authors are considering differences across site (e.g., jamie's chagos project with sites with different shark abundance, Tibet grassland experiments). Otherwise, we will include all sites within one datapoint (most papers are more like this).
- We are including data with multiple years, but not if there are multiple sampling time points within a year (we filter this out post-harmonization). We will likely only include the last year/sampling time point in our analysis, but have included all the data in case we decide to do some temporal comparison. (https://github.com/lter/lterwg-caged/issues/7)
- Many experiments have multiple measures of "abundance" (e.g., biomass, cover, density) and we will only take one to ensure they are independent (https://github.com/lter/lterwg-caged/issues/1)
- We will create a "unique.id" column after harmonization so we can create one datapoint for each exp.name. The exp.name in the harmonized file will just be the file.name if there are no exp.name (separate experiments/sites that need to be treated as independent).

unique.id = exp.design<sub>N</sub> + treatment<sub>N</sub> + year + exp.name  


### Code Workflow Notes  
1) Download Data
- When downloading data from our list of [data sources](https://docs.google.com/spreadsheets/d/1Eg1mt-TPUgXqqPe8e8nhtAmKJ0NzWDbolfSVKmQf4o8/edit), before uploading to [google drive data folder](https://drive.google.com/drive/u/1/folders/1EOSlNF3zz-ktBQwoIt1a30dv0azJ1g5M), we renamed the files using file.name  
file.name = organization_site_experimentname_yearssampled_excluded_measured
- If the file cannot be uploaded as is, put the file in purgatory and we will run boatyard scripts to clean it up (https://github.com/lter/lterwg-caged/issues/8)
  - pre-harmonization "boatyard" script (`purgatory-repair.R`) to handle pernicious issues in some datasets before they can be entered into the harmonization workflow
 
- when its uploaded, make sure to check Y in the [data sources](https://docs.google.com/spreadsheets/d/1Eg1mt-TPUgXqqPe8e8nhtAmKJ0NzWDbolfSVKmQf4o8/edit)
 
2) Create [Data-Key](https://docs.google.com/spreadsheets/d/1SgdqsAl_yPArCaw8dCcP0KINL3Qp2qwN0ZJqWnIxGPk/edit?gid=0#gid=0)   

- Then we will create the data-key using the expand-key.R in boatyard scripts folder (this will only add the new data rows)  
- Then we manually fill in the data key with our knowledge
- Column names for data key
    - exp.design.1-N: Block, transect, site, etc. each of those get a different number. These categories denote different aspects of the experimental design
    - exp.name: Unique name for the experiment. We use this if the sites are uniquely different from one another (this shouldn’t happen often) and is only if the authors have said there are unique differences across sites that make it interesting (and not part of exp.design)
    - sampling.point: Unique sampling time point. When experiments are sampled / data is recorded at multiple timepoints
    - orig.treat:  Denotes the original treatment of the experiment, however the author described them. Any time we put anything but orig.treat we have to put another line of code (Nick). Only use if there is only one treatment given. For example, if they concatenate all treatments into one variable- orig.treat_nutirents, orig.treat_fire, orig.treat_cage, etc.  

3) Harmonize Data  
- Then we run the harmonize script (01_harmonize) and hopefully! we end up with a beautiful happy dataset :)
  - harmonization workflow (`01_harmonize.R`) that uses column key-based method (see `?ltertools::harmonize`)  
 
4) Fill out [Metadata](https://docs.google.com/spreadsheets/d/1vNMYj3-xO_tmIhniGyOuj1pG4DQt9fMRAuQipPE3hys/edit?gid=0#gid=0)    

- lastly, we will fill out metadata for each exp.name
- follow our [instructions](https://docs.google.com/document/d/1gnFHVtSg-F6A3v3lRcJr1pGRxlhtWLQNRr4mCgxqMMQ/edit?tab=t.0)
- when its completed leadership team check Y in the [data sources](https://docs.google.com/spreadsheets/d/1Eg1mt-TPUgXqqPe8e8nhtAmKJ0NzWDbolfSVKmQf4o8/edit)  




