

#extract all unique taxa from column 'taxa'
taxa_unique = unique(taxa_list$taxa)

#save list of unique values in 'taxa'
write.csv(taxa_unique, file = "unique_taxa_06May2025.csv")

#create list of taxa to exclude from ALL studies
taxa_exclude = c("LITT",
                 "Bare",
                 "Dead Barnacle",
                 "Amphipod tube",
                 "bare",
                 "Mud Tube",
                 "Jingle shell",
                 "Sand tube",
                 "Little Black tubes",
                 "Mud tube",
                 "Branch",
                 "rock",
                 "BARE",
                 "Litter",
                 "Bareground",
                 "cactus__dead_",
                 "QUERCUS DOUGSEED",
                 "QUERCUS DOUGLASII_SEED",
                 "SEED2 SPECIES",
                 "SEED1 SPECIES",
                 "QUERCUS AGRIFOLIA_SEED",
                 "QUERCUS AG_SEED",
                 "ZZZZ general codes",
                 "#N/A",
                 "per.bare",
                 "litter",
                 "standing dead Betula nana",
                 "caribou feces",
                 "frost boil",
                 "animal litter",
                 "Squirrel feces",
                 "vole trail",
                 "vole litter",
                 "human trail",
                 "vole hole",
                 "vole trail",
                 "Mixed dead litter",
                 "Bare soil",
                 "Standing Dead Betula nana",
                 "Soil Frost boil",
                 "Standing Dead Salix pulchra",
                 "Ledum palustre-Dead",
                 "Miscellaneous litter",
                 "Pine needles",
                 "Radulations",
                 "Bare.cropped.substrate",
                 "Rubble",
                 "Sand",
                 "SOIL",
                 "sediment",
                 "substrate",
                 "Rock",
                 "Dung")

#For spiecker_newzealand_intertidalexclosure_2017-2018_herbivores_intertidal.csv, bleached corals need to be lumped with non-bleached:

"Bleached Crustose" --> "Crustose"
"Bleached Jointed Calcareous" --> "Jointed Calcareous"
"Bleached Sheet" --> "Sheet"
"Bleached Coarsely Branched" --> "Coarsely Branched"






