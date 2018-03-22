# ---- Function: clean_taxon ----
clean_taxon<-function(data=ISSPECIESCODES, taxadata=taxa_table, summary=FALSE)
{
  taxa<-taxadata%>%mutate(SCIENTIFIC=paste(Genus, Species))%>%select(-X)
  
  # Trim leading and trailing whitespace
  data$SCIENTIFIC<-trimws(data$SCIENTIFIC, which=c("both"))
  
  # Remove Reserved Species Codes
  reserved<-data%>%
    filter(SCIENTIFIC=="RESERVED")
  
  remains<-data%>%
    filter(!SPECCD_ID %in% reserved$SPECCD_ID)
  
  if(summary==TRUE)
  {
    assessment<-"R"
    codes_complete<-nrow(reserved)
    codes_remaining<-nrow(remains)
  }
  # Level 1: Scientific Names Match Reference Table
  done<-data%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    left_join(taxa)%>%
    filter(!is.na(Genus))
  
  remains<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)
  
  if(summary==TRUE)
  {
    assessment<-c(assessment,"L1")
    codes_complete<-c(codes_complete,nrow(done))
    codes_remaining<-c(codes_remaining,nrow(remains))
  }
  
  # Level 2: Scientific Names End With " SP."
  taxa_genus<-taxa%>%
    select(-Species, -FBname, -SCIENTIFIC)%>%
    distinct()
  
  done<-remains%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-2, nchar(SCIENTIFIC)))%>%
    mutate(Genus=ifelse(temp %in% c(" SP", "SPP"),str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-3),NA))%>%
    mutate(Genus=str_replace_all(Genus," ",""), Species=NA, FBname=NA)%>%
    left_join(taxa_genus)%>%
    filter(Genus %in% taxa_genus$Genus)%>%
    select(names(done))%>%
    rbind(done)
  
  remains<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)

  if(summary==TRUE)
  {
    assessment<-c(assessment,"L2")
    codes_complete<-c(codes_complete,nrow(done)-sum(codes_complete[2:length(codes_complete)]))
    codes_remaining<-c(codes_remaining,nrow(remains))
  }
  
  # Level 3: Scientific Names End With " F." (Family)
  taxa_family<-taxa%>%
    select(-Genus, -Species, -FBname, -SCIENTIFIC, -SubFamily)%>%
    distinct()
  
  done<-remains%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-1, nchar(SCIENTIFIC)))%>%
    mutate(Family=ifelse(temp == " F", str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-2),NA))%>%
    mutate(Family=str_replace_all(Family," ",""), SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
    left_join(taxa_family)%>%
    filter(Family %in% taxa_family$Family)%>%
    select(names(done))%>%
    rbind(done)
  
  remains<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)
  
  if(summary==TRUE)
  {
    assessment<-c(assessment,"L3")
    codes_complete<-c(codes_complete,nrow(done)-sum(codes_complete[2:length(codes_complete)]))
    codes_remaining<-c(codes_remaining,nrow(remains))
  }
  # Level 4: Scientific Names End With " O." (Order)
  taxa_order<-taxa%>%
    select(-Family, -Genus, -Species, -FBname, -SCIENTIFIC, -SubFamily)%>%
    distinct()
  
  done<-remains%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-1, nchar(SCIENTIFIC)))%>%
    mutate(Order=ifelse(temp == " O", str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-2),NA))%>%
    mutate(Order=str_replace_all(Order," ",""), Family=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
    left_join(taxa_order)%>%
    filter(Order %in% taxa_order$Order)%>%
    select(names(done))%>%
    rbind(done)
  
  remains<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)
  
  if(summary==TRUE)
  {
    assessment<-c(assessment,"L4")
    codes_complete<-c(codes_complete,nrow(done)-sum(codes_complete[2:length(codes_complete)]))
    codes_remaining<-c(codes_remaining,nrow(remains))
  }
  
  # Level 5: Scientific Names End With " C." (Class)
  taxa_class<-taxa%>%
    select(-Family, -Genus, -Species, -FBname, -Order, -SCIENTIFIC, -SubFamily)%>%
    distinct()
  
  done<-remains%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    mutate(temp=str_sub(SCIENTIFIC, nchar(SCIENTIFIC)-1, nchar(SCIENTIFIC)))%>%
    mutate(Class=ifelse(temp == " C", str_sub(SCIENTIFIC, 1, nchar(SCIENTIFIC)-2),NA))%>%
    mutate(Class=str_replace_all(Class," ",""), Family=NA, Order=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
    left_join(taxa_class)%>%
    filter(Class %in% taxa_class$Class)%>%
    select(names(done))%>%
    rbind(done)
  
  remains<<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)
  
  if(summary==TRUE)
  {
    assessment<-c(assessment,"L5")
    codes_complete<-c(codes_complete,nrow(done)-sum(codes_complete[2:length(codes_complete)]))
    codes_remaining<-c(codes_remaining,nrow(remains))
  }
  
  # Level 6: Scientific Names are a Recognized Family
  done<-remains%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    mutate(Family=str_replace_all(SCIENTIFIC," ",""), SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
    left_join(taxa_family)%>%
    filter(Family %in% taxa_family$Family)%>%
    select(names(done))%>%
    rbind(done)
  
  remains<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)
  
  if(summary==TRUE)
  {
    assessment<-c(assessment,"L6")
    codes_complete<-c(codes_complete,nrow(done)-sum(codes_complete[2:length(codes_complete)]))
    codes_remaining<-c(codes_remaining,nrow(remains))
  }
  
  # Level 7: Scientific Names are a Recognized Order
  done<-remains%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    mutate(Order=str_replace_all(SCIENTIFIC," ",""), Family=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
    left_join(taxa_order)%>%
    filter(Order %in% taxa_order$Order)%>%
    select(names(done))%>%
    rbind(done)
  
  remains<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)
  
  if(summary==TRUE)
  {
    assessment<-c(assessment,"L7")
    codes_complete<-c(codes_complete,nrow(done)-sum(codes_complete[2:length(codes_complete)]))
    codes_remaining<-c(codes_remaining,nrow(remains))
  }
  
  # Level 8: Scientific Names are a Recognized Class
  done<-remains%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, c("[[:punct:]]|\t"),""))%>%
    mutate(SCIENTIFIC=str_replace_all(SCIENTIFIC, "  "," "))%>%
    mutate(Class=str_replace_all(SCIENTIFIC," ",""), Order=NA, Family=NA, SubFamily=NA, Genus=NA, Species=NA, FBname=NA)%>%
    left_join(taxa_class)%>%
    filter(Class %in% taxa_class$Class)%>%
    select(names(done))%>%
    rbind(done)
  
  remains<-remains%>%
    filter(!SPECCD_ID %in% done$SPECCD_ID)

  clean_codes<<-done
  error_codes<<-remains
  
  if(summary==TRUE)
  {
    assessment<-c(assessment,"L8")
    codes_complete<-c(codes_complete,nrow(done)-sum(codes_complete[2:length(codes_complete)]))
    codes_remaining<-c(codes_remaining,nrow(remains))
    
    clean_taxa_summary<<-data.frame(assessment, clean=codes_complete, codes_remaining)
    return(clean_taxa_summary)
  }
  
}

