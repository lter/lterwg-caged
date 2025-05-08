#CAGED NCEAS working group
#Nico Matallana exploratory script


# Load Packages -----------------------------------------------------------
library(tidyverse)


# Attempt to connect with Github (failed) ---------------------------------
# Install the `usethis` and `gitcreds` packages
install.packages(c("usethis", "gitcreds"))
library(usethis)
library(gitcreds)
library(sp)


# Create a token (Note this will open a GitHub browser tab)
## See steps 6-10 in GitHub's PAT tutorial (link below)

#Already done:
#usethis::create_github_token()

#Why won't this work?
gitcreds::gitcreds_set()





# Import data -------------------------------------------------------------

#import metadata w/ lat/long
data1 = read.csv("C:/Users/Owner/OneDrive - Colostate/Documents/Grad School/Misc/CAGED NCEAS 2025/Datasets/06_caged_with-metadata - 06_caged_with-metadata.csv")




data2 = data1 %>% #Only have unique site identifiers & lat/long
  select(1:12) %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>%
  remove(is.na(is.na))

data3 = data2(pd.to_numeric(data))




spdf = SpatialPointsDataFrame(sp.coords, data2)                                                                                                                                                             "latitude"), class = "data.frame", row.names = c(NA, -8L))




