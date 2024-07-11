## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(flextable)


mkdir("output")

# Load data ---------------------------------------------------------------

# input data

load("data/input.RData") 



# info surveys ----
info.data.pela$survey<-"PELAGO"
info.data.ecocadiz$survey<-"ECOCADIZ"
data.ecoR2$survey<-"ECOCADIZ-RECLUTAS"

info_survey<-rbind(info.data.pela[,c(1,2,6)],info.data.ecocadiz[,c(1,2,6)],data.ecoR2[,c(1,2,6)])
info_survey<-dcast(info_survey, year ~ survey,value.var = "month")

# biomass surveys ----
dist_si_pelago$survey<-"PELAGO"
dist_si_ecocadiz$survey<-"ECOCADIZ"
dist_si_ECOREC$survey<-"ECOCADIZ-RECLUTAS"

indices_a<-rbind(dist_si_pelago[,c(1,4,5)],dist_si_ecocadiz[,c(1,4,5)],dist_si_ECOREC[,c(1,4,5)])
indices<-dcast(indices_a, year ~ survey,value.var = "weight")

# age composition surveys ----
adist_pelago$survey<-"PELAGO"
adist_ecocadiz$survey<-"ECOCADIZ"
adist_ecocadizR$survey<-"ECOCADIZ-RECLUTAS"


adist_surveys<-rbind(adist_pelago,adist_ecocadiz,adist_ecocadizR)





write.taf(list(info_survey=info_survey,surveys = indices),dir="./data")

# Write inputData.RData 
save(info_survey, indices,indices_a,adist_surveys,file="./data/inputData.RData")
