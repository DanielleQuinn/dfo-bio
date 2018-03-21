# Creates the taxonomic reference tables used to assess ISDB Species Codes #
library(rfishbase)

FBtaxa<-load_taxa()
SLtaxa<-load_taxa(server = "https://fishbase.ropensci.org/sealifebase")


head(FBtaxa, 1)
head(SLtaxa,1)

fish_taxa<-FBtaxa%>%
  select(Class, Order, Family, SubFamily, Genus, Species, FBname)%>%
  mutate(Source="FishBase")%>%
  data.frame()

sea_taxa<-SLtaxa%>%
  select(Class, Order, Family, SubFamily, Genus, Species, FBname)%>%
  mutate(Source="SeaLifeBase")%>%
  data.frame()

taxa<-rbind(fish_taxa, sea_taxa)
write.csv(taxa, "data/taxonomy_reference_table.csv")
