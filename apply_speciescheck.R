# Scope out workflow for QAQC of fish species being captured
load("data/ISDB.ISSPECIESCODES.RData")
taxa<-read.csv("data/taxonomy_reference_table.csv")
taxa<-taxa%>%mutate(SCIENTIFIC=toupper(paste(Genus, Species)))%>%select(-X)

library(dplyr)
library(stringr)
library(rfishbase)

ISSPECIESCODES$SCIENTIFIC<-trimws(ISSPECIESCODES$SCIENTIFIC, which=c("both"))

# Remove Reserved Species Codes
reserved<-ISSPECIESCODES%>%
  filter(SCIENTIFIC=="RESERVED")

remains<-ISSPECIESCODES%>%
  filter(!SPECCD_ID %in% reserved$SPECCD_ID)

# Level 1: Scientific Names Match Reference Table
done<-ISSPECIESCODES%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  left_join(taxa)%>%
  filter(!is.na(Genus))

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)

# Level 2: Scientific Names End With " SP."
taxa_genus<-taxa%>%
  select(-Species, -FBname, -SCIENTIFIC)%>%
  distinct()%>%
  mutate(Genus=toupper(Genus))

done<-remains%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-2, nchar(SCIENTIFIC)))%>%
  mutate(Genus=ifelse(temp %in% c(" SP", "SPP"),str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-3),NA))%>%
  mutate(Genus=str_replace(Genus," ",""), Species=NA, FBname=NA)%>%
  left_join(taxa_genus)%>%
  filter(Genus %in% toupper(taxa_genus$Genus))%>%
  select(names(done))%>%
  rbind(done)

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)

# Level 3: Scientific Names End With " F." (Family)
taxa_family<-taxa%>%
  select(-Genus, -Species, -FBname, -SCIENTIFIC, -SubFamily)%>%
  distinct()%>%
  mutate(Family=toupper(Family))

done<-remains%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-1, nchar(SCIENTIFIC)))%>%
  mutate(Family=ifelse(temp == " F", str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-2),NA))%>%
  mutate(Family=str_replace(Family," ",""), SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
  left_join(taxa_family)%>%
  filter(Family %in% toupper(taxa_family$Family))%>%
  select(names(done))%>%
  rbind(done)

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)

# Level 4: Scientific Names End With " O." (Order)
taxa_order<-taxa%>%
  select(-Family, -Genus, -Species, -FBname, -SCIENTIFIC, -SubFamily)%>%
  distinct()%>%
  mutate(Order=toupper(Order))

done<-remains%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-1, nchar(SCIENTIFIC)))%>%
  mutate(Order=ifelse(temp == " O", str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-2),NA))%>%
  mutate(Order=str_replace(Order," ",""), Family=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
  left_join(taxa_order)%>%
  filter(Order %in% toupper(taxa_order$Order))%>%
  select(names(done))%>%
  rbind(done)

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)

# Level 5: Scientific Names End With " C." (Class)
taxa_class<-taxa%>%
  select(-Family, -Genus, -Species, -FBname, -Order, -SCIENTIFIC, -SubFamily)%>%
  distinct()%>%
  mutate(Class=toupper(Class))

done<-remains%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-1, nchar(SCIENTIFIC)))%>%
  mutate(Class=ifelse(temp == " C", str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-2),NA))%>%
  mutate(Class=str_replace(Class," ",""), Family=NA, Order=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
  left_join(taxa_class)%>%
  filter(Class %in% toupper(taxa_class$Class))%>%
  select(names(done))%>%
  rbind(done)

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)

# Level 6: Scientific Names are a Recognized Family
done<-remains%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  mutate(Family=str_replace(SCIENTIFIC," ",""), SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
  left_join(taxa_family)%>%
  filter(Family %in% toupper(taxa_family$Family))%>%
  select(names(done))%>%
  rbind(done)

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)

# Level 7: Scientific Names are a Recognized Order
done<-remains%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  mutate(Order=str_replace(SCIENTIFIC," ",""), Family=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
  left_join(taxa_order)%>%
  filter(Order %in% toupper(taxa_order$Order))%>%
  select(names(done))%>%
  rbind(done)

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)

# Level 8: Scientific Names are a Recognized Class
done<-remains%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "[[:punct:]]",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "\t",""))%>%
  mutate(SCIENTIFIC=str_replace(SCIENTIFIC, "  "," "))%>%
  mutate(Class=str_replace(SCIENTIFIC," ",""), Order=NA, Family=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
  left_join(taxa_class)%>%
  filter(Class %in% toupper(taxa_class$Class))%>%
  select(names(done))%>%
  rbind(done)

remains<-remains%>%
  filter(!SPECCD_ID %in% done$SPECCD_ID)
