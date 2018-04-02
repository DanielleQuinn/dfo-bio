# Creates the taxonomic reference tables used to assess ISDB Species Codes #
library(rfishbase)

FBtaxa<-load_taxa()
SLtaxa<-load_taxa(server = "https://fishbase.ropensci.org/sealifebase")

# Add TSN
fish_taxa<-FBtaxa%>%
  select(Class, Order, Family, SubFamily, Genus, Species, FBname)%>%
  mutate(Source="FishBase")%>%
  data.frame()

sea_taxa<-SLtaxa%>%
  select(Class, Order, Family, SubFamily, Genus, Species, FBname)%>%
  mutate(Source="SeaLifeBase")%>%
  data.frame()

taxa_table<-rbind(fish_taxa, sea_taxa)

# Convert to uppercase for ease of applying later functions
for(i in 1:ncol(taxa_table)) {taxa_table[,i]<-toupper(taxa_table[,i])}

write.csv(taxa_table, "data/taxonomy_reference_table.csv")
