## --------------------------------------------------------------- ##
# CAGED Setup for Group Members
## --------------------------------------------------------------- ##
# Written by: Nick J Lyon, ...

# Purpose:
## Group members may want a "shortcut" so they don't need to run the full workflow
## This script generates needed local folders and downloads the outputs of core scripts from Drive

# Note:
## This script assumes you (the person running the script):
### 1. Have access to the group's Shared Drive
### 2. Have adopted GitHub (so that you have the right working directory)

## ------------------------------------------- ##
# Create Local Folders ----
## ------------------------------------------- ##

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "raw"), showWarnings = F)
dir.create(path = file.path("graphs"), showWarnings = F)

## ------------------------------------------- ##
# Download Core Outputs ----
## ------------------------------------------- ##

# Identify outputs
core_outs <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Acv2ybcpOd_8jEohzgVWcm5qRmgDb4Od"))

# Looks okay?
core_outs

# Download 'em all (overwriting your local versions)
purrr::walk2(.x = core_outs$id, .y = core_outs$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", .y)))

# End ----
