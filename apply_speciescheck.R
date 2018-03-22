library(dplyr)
library(stringr)
library(rfishbase)

load("data/ISDB.ISSPECIESCODES.RData")
taxa_table<-read.csv("data/taxonomy_reference_table.csv")

source("species_function.R")
clean_taxon(summary=TRUE)

write.csv(clean_codes, "updated_species_codes.csv")
write.csv(error_codes, "errors_species_codes.csv")
