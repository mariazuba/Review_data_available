## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(openxlsx)
library(readxl)
require(dplyr)
require(tidyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(viridisLite)
library(viridis)
library(icesTAF)
library(fs)
library(httr)
library(knitr)
library(kableExtra)
library(magrittr)
library(purrr)
library(ggridges)


mkdir("data")



path_origin<-getwd()
# DATA for model
path.data<-"boot/data" 
list.files(path.data)
# survey data since 1999-2016
file_path <- file.path(path.data, "Acústica_en_9aS_TallasyEdades.xlsx")

# PELAGO DATA ----

## 1. DATOS *PELAGO* ----

### a) acustic biomass ----

#- Años 1999, 2001:2003, 2005:2009 and Años 2010, 2013 y 2014
#- From 1999 to 2014, data is read from the file "Acústica en 9aS_TallasyEdades.xls".

#### Information files
year.pela0<-c(1999,2001:2003,2005:2009)
year.pela1<-c(2010,2013,2014)
year.pela<-c(year.pela0,year.pela1)
nyears<-length(year.pela)

mestrimPel1<-c(rep(3,3),2,rep(4,8))
# Convertir los valores numéricos a nombres de meses
mesPel <- month.name[mestrimPel1]

path.pela1<-file.path(path.data)
file.pela1<-rep("Acústica_en_9aS_TallasyEdades.xlsx",nyears)
sheet.pela1<-paste0(year.pela, "Port")

#col and row 
#'*biomass*
row.pela1<-rep(2,nyears)
col.pela1<-c(rep(11,length(year.pela0)),
             rep(9,length(year.pela1)))
col_letters1 <- LETTERS[col.pela1]
range_bio1<-paste0(col_letters1,row.pela1)

# Create the dataframe
data.files<-data.frame(year=year.pela,
                       month=mesPel,
                       name_file=file.pela1,
                       name_sheet=sheet.pela1,
                       range_bio=range_bio1)


#- Data from 2015 

# Define the files
year.pela2<-seq(2015,2024,1)
nyears2<-length(year.pela2)
mestrimPel2<-c(rep(4,5),3,3,3,3,3)
# Convertir los valores numéricos a nombres de meses
mesPel2 <- month.name[mestrimPel2]

file.pela2<-c("ANE_PELAGO15_Results.xlsx",
              "PELAGO16_ANE_RESULTS_FINAL_sg.xlsx",
              "results-ane-pel17_fv.xlsx",
              "results-ANE-PEL18.xlsx",
              "results-ANE-PEL19.xlsx",
              "results-ANE-PEL20_final.xlsx",
              "results-ANE-PEL21_final.xlsx",
              "results-ANE-PEL22_final.xlsx",
              "results-ANE-PEL23_final.xlsx",
              "results-ANE-PEL24.xlsx")
sheet.pela2<-c("9a S",
               "AbundanceBiomassANE_FINAL",
               "CAD",
               "algarve+cadiz",
               "algarve+cadiz",
               "algarve+cadiz",
               "algarve+cadiz",
               "algarve+cadiz",
               "algarve+cadiz",
               "algarve+cadiz")

# col and row 
#'*biomass*
row.pela2<-c(45,109,44,44,44,44,44,44,44,44)
col.pela2<-c(15,3,5,3,3,3,3,3,3,3)
col_letters2 <- LETTERS[col.pela2]
range_bio2<-paste0(col_letters2,row.pela2)

# Create the dataframe
data.files2<-data.frame(year=year.pela2,
                        month=mesPel2,
                        name_file=file.pela2,
                        name_sheet=sheet.pela2,
                        range_bio=range_bio2)

info.data.pela<-rbind(data.files,data.files2)

##### Biomass 
total_years<-nyears+nyears2
dist_si_pelago<-data.frame(year=rep(NA,total_years), 
                           step=rep(NA,total_years),
                           area=rep(NA,total_years),
                           weight=rep(NA,total_years))

for (i in 1:total_years){
  if(i<=nyears){
    bioPel1<-read.xlsx(file.path(path.data,file.pela1[i]), 
                       sheet = sheet.pela1[i], 
                       cols=col.pela1[i],
                       rows=row.pela1[i],
                       colNames = F)*0.001 #Para que quede en toneladas (está en kilos)
    dist_si_pelago[i,]<-c(year.pela[i],mestrimPel1[i],"IXa",bioPel1)
  } else {
    index2<-i-nyears
    bioPel2<-read.xlsx(file.path(path.data,file.pela2[index2]), 
                       sheet = sheet.pela2[index2],
                       cols = col.pela2[index2], 
                       rows = row.pela2[index2],
                       colNames = F)
dist_si_pelago[i,]<-c(year.pela2[index2],mestrimPel2[index2],"IXa",bioPel2)
  }
}

# Para gadget
landings_pelago<-data.frame(year= dist_si_pelago$year,
                            step= dist_si_pelago$step,
                            area= dist_si_pelago$area,
                            weight=rep(1,total_years))

### b) Length composition ----
#'*---------------------------------------------------------*
### Años 1999, 2001:2003, 2005:2009, 2010, 2013 y 2014 
#'*---------------------------------------------------------*
#'#Composicioón talla/edad
col_1<-c(13,13,13,13,13,13,13,13,13,11,11,11)
col_2<-c(17,17,17,17,17,17,17,17,17,15,15,15)
row_1<-c(6,6,6,6,6,6,6,6,6,6,6,6)
row_2<-c(37,37,37,37,37,37,37,37,37,37,37,37)
#Pesos medios
colP_1<-c(11,11,11,11,11,11,11,11,11,9,9,9)
colP_2<-c(14,14,14,14,14,14,14,14,14,12,12,12)
rowP_1<-c(80,80,80,80,80,80,80,80,80,80,80,80)
#Talla media a la edad
colL_1<-c(2,2,2,2,2,2,2,2,2,2,2,2)
colL_2<-c(5,5,5,5,5,5,5,5,5,5,5,5)
rowL_1<-c(80,80,80,80,80,80,80,80,80,80,80,80)

#'------------------------------------------
mybiglist0c<- list()
mybiglist0b<- list()
mybiglist0 <- list()
for (i in 1:nyears){
  name<-paste("Pelago",year.pela[i],sep="")
  a<-read.xlsx(file.path(path.data,file.pela1[i]), 
               sheet = sheet.pela1[i], 
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  # a<-data.frame(cbind(b$X1,0,b$X2,b$X3,b$X4))
  names(a)<-c("length","0","1", "2", "3")
  a$length<-a$length-0.25
  a$year<-year.pela[i]
  a$step<-mestrimPel1[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist0[[name]] <- a
  #Pesos medios
  b<-read.xlsx(file.path(path.data,file.pela1[i]), 
               sheet = sheet.pela1[i], 
               cols = colP_1[i]:colP_2[i],
               rows = rowP_1[i],
               colNames = F)
  names(b)<-c("0","1", "2", "3")
  b$year<-year.pela[i]
  b$step<-mestrimPel1[i]
  b$area<-"IXa"
  b[is.na(b)] <- 0
  mybiglist0b[[name]] <- b
  #Talla media
  c<-read.xlsx(file.path(path.data,file.pela1[i]), 
               sheet = sheet.pela1[i], 
               cols = colL_1[i]:colL_2[i],
               rows = rowL_1[i],
               colNames = F)
  names(c)<-c("0","1", "2", "3")
  c$year<-year.pela[i]
  c$step<-mestrimPel1[i]
  c$area<-"IXa"
  c[is.na(c)] <- 0
  mybiglist0c[[name]] <- c
}
dfpela0<-plyr::ldply(mybiglist0,data.frame)
dPpela0<-plyr::ldply(mybiglist0b,data.frame)
dLpela0<-plyr::ldply(mybiglist0c,data.frame)

#'*---------------------------------------------------------*
### Años 2015 al 2024 
#'*---------------------------------------------------------*
#Composicioón talla/edad
col_1<-c(38,29,36,18,18,18,18,18,18,18)
col_2<-c(41,32,39,21,21,21,21,21,21,21)
row_1<-c(6,68,7,9,4,3,3,3,3,3)
row_2<-c(32,89,27,29,24,18,29,29,29,29)
#Pesos medios
colP_1<-c(39,50,44,38,26,26,26,26,26,26,26)
colP_2<-c(41,52,46,40,28,28,28,28,28,28,28)
rowP_1<-c(82,91,59,31,26,42,42,42,42,42,42)
#Talla media a la edad
colL_1<-c(28,43,37,31,19,19,19,19,19,19,19)
colL_2<-c(30,45,39,33,21,21,21,21,21,21,21)
rowL_1<-c(82,91,59,31,26,42,42,42,42,42,42)

mybiglist1d <- list()
mybiglist1c <- list()
mybiglist1 <- list()
for(i in 1:nyears2){
  name<-paste("Pelago",year.pela2[i],sep="")
  b<-read.xlsx(file.path(path.data,file.pela2[i]), 
               sheet = sheet.pela2[i],
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  a<-data.frame(cbind(b$X1,0,b$X2,b$X3,b$X4))
  names(a)<-c("length","0","1", "2", "3")
  a$length<-a$length
  a$year<-year.pela2[i]
  a$step<-mestrimPel2[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist1[[name]] <- a
  
  # Pesos medios
  c<-read.xlsx(file.path(path.data,file.pela2[i]), 
               sheet = sheet.pela2[i],
               cols = colP_1[i]:colP_2[i],
               rows = rowP_1[i],
               colNames = F)
  d<-data.frame(cbind(0,c$X1,c$X2,c$X3))
  names(d)<-c("0","1", "2", "3")
  d$length<-d$length
  d$year<-year.pela2[i]
  d$step<-mestrimPel2[i]
  d$area<-"IXa"
  d[is.na(d)] <- 0
  mybiglist1c[[name]] <- d
  
  # Talla media
  d<-read.xlsx(file.path(path.data,file.pela2[i]),
               sheet = sheet.pela2[i],
               cols = colL_1[i]:colL_2[i],
               rows = rowL_1[i],
               colNames = F)
  e<-data.frame(cbind(0,d$X1,d$X2,d$X3))
  names(e)<-c("0","1", "2", "3")
  e$length<-e$length
  e$year<-year.pela2[i]
  e$step<-mestrimPel2[i]
  e$area<-"IXa"
  e[is.na(e)] <- 0
  mybiglist1d[[name]] <- e
  
}
dfpela1<-plyr::ldply(mybiglist1,data.frame)
dPpela1<-plyr::ldply(mybiglist1c,data.frame)
dLpela1<-plyr::ldply(mybiglist1d,data.frame)


dfPela<-rbind(dfpela0,dfpela1)
dPPela<-rbind(dPpela0,dPpela1)
dLPela<-rbind(dLpela0,dLpela1)

# length composition
ldist_pelago <- dfPela %>%
  mutate(number = X0 + X1 + X2 + X3)%>%
  select(year, step, area, length, number)

dfpelago <- dfPela %>%
  select(-length) %>%
  mutate(sum_X = rowSums(select(., starts_with("X")))) %>%
  group_by(year, step) %>%
  summarize(across(.cols = everything()))

### c) age composition ----
adist_pelago <-melt(dfpelago,id=c(".id","sum_X","year","step","area"))

adist_pelago <- adist_pelago %>%
  select(-c(.id,sum_X)) %>%
  rename(age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))

### d) age proportion ----
# Calcular el número total de peces para cada año y paso
dfpelab <- dfpelago %>%
  mutate(X0 = ifelse(sum_X != 0, X0 / sum_X, 0),
         X1 = ifelse(sum_X != 0, X1 / sum_X, 0),
         X2 = ifelse(sum_X != 0, X2 / sum_X, 0),
         X3 = ifelse(sum_X != 0, X3 / sum_X, 0)) %>%
  select(-sum_X)

apropdist_pelago <-melt(dfpelab,id=c(".id","year","step","area"))

apropdist_pelago <- apropdist_pelago %>%
  select(-c(.id)) %>%
  rename(age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))

### e) Conditional age-at-length composition ----
#'*---------------------------------------------------------*
### Años 1999, 2001:2003, 2005:2009, 2010, 2013 y 2014 
#'*---------------------------------------------------------*
col_1<-c(1,1,1,1,1,1,1,1,1,1,1,1)
col_2<-c(5,5,5,5,5,5,5,5,5,5,5,5)
row_1<-c(6,6,6,6,6,6,6,6,6,6,6,6)
row_2<-c(37,37,37,37,37,37,37,37,37,37,37,37)

mybiglist0pel <- list()
for (i in 1:nyears){
  name<-paste("Pelago",year.pela[i],sep="")
  a<-read.xlsx(file.path(path.data,file.pela1[i]), 
               sheet = sheet.pela1[i], 
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  names(a)<-c("length","0","1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.pela[i]
  a$step<-mestrimPel1[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist0pel[[name]] <- a
}
dfpela0a<-plyr::ldply(mybiglist0pel,data.frame)
#'*---------------------------------------------------------*
### Años 2015 al 2024 
#'*---------------------------------------------------------*
col_1<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
col_2<-c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
row_1<-c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6)
row_2<-c(30,27,26,26,26,26,26,28,31,26)

mybiglist1apel <- list()
for(i in 1:nyears2){
  name<-paste("Pelago",year.pela2[i],sep="")
  a<-read.xlsx(file.path(path.data,paste0("Pelago",year.pela2[i],".xlsx")), 
               sheet = "Sheet1",
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  names(a)<-c("length","0","1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.pela2[i]
  a$step<-mestrimPel2[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist1apel[[name]] <- a
}
dfpela1a<-plyr::ldply(mybiglist1apel,data.frame)
dfpela1a2<-rbind(dfpela0a,dfpela1a)

names(dfpela1a2)<-c(".id", "length", "0", "1","2","3","year","step","area")
pela_alk_ald<-dfpela1a2[2:9]
pela_defalk_ald<-melt(pela_alk_ald,id=c("length","year","step","area" )) 

aldist_pelago<- pela_defalk_ald[, c(2,3,4,5,1,6)]
names(aldist_pelago)<-c("year","step","area","age","length","number")

### f) Watage ----

names(dPPela)<-c(".id", "0", "1","2","3","year","step","area")
pela_wage<-dPPela[2:8]

wagePela<-data.frame(Yr=pela_wage$year,	
                      Seas=pela_wage$step,	
                      Sex=1,	
                      Bio_Pattern=1,	
                      BirthSeas=1,	
                      Fleet=3)	#'*Revisar nºflota pelago!!!*
wagePela<-cbind(wagePela,pela_wage[1:4])

wage_pela<-melt(pela_wage,id=c("year","step","area" )) 
names(wage_pela)<-c("year","step","area","age","weight")

### f) Lmed_at_age ----

names(dLPela)<-c(".id", "0", "1","2","3","year","step","area")
pela_lage<-dLPela[2:8]

lagePela<-data.frame(Yr=pela_lage$year,	
                     Seas=pela_lage$step,	
                     Fleet=2)	#'*Revisar nºflota pelago!!!*
lagePela<-cbind(lagePela,pela_lage[1:4])
# for plot
lage_pela<-melt(pela_lage,id=c("year","step","area" )) 
names(lage_pela)<-c("year","step","area","age","weight")

## 2. DATA *ECOCADIZ* ----

### a) Biomasa acústica ----

# Las biomasas consideran todas las edades, desde edad 0 a la 3+
#   
# cel1: Años 2004,2006,2007,2009 y 2010 
# cel2: Años 2013,2014,2015

year.eco0<-c(2004,2006,2007,2009,2010 )
year.eco1<-c(2013,2014,2015)
year.eco<-c(year.eco0,year.eco1)
nyearsEco<-length(year.eco)
mestrimEco1<-c(6,6,7,7,7,8,7,7)

path.eco1<-file.path(path.data)
file.eco1<-rep("Acústica_en_9aS_TallasyEdades.xlsx",nyearsEco)
sheet.eco1<-paste0(year.eco, "Esp")

#col and row 
#'*biomass*
row.eco1<-rep(2,nyearsEco)
col.eco1<-c(rep(11,length(year.eco0)),rep(9,length(year.eco1)))

# Create the dataframe
col_letters <- LETTERS[col.eco1]
range_bio<-paste0(col_letters,row.eco1)
mesEco <- month.name[mestrimEco1]# Convertir los valores numéricos a nombres de meses
data.eco1<-data.frame(year=year.eco,
                      month=mesEco,
                      name_file=file.eco1,
                      name_sheet=sheet.eco1,
                      range_bio=range_bio)

path.eco2<-path.data

# Define the files
year.eco2<-c(2016,2017,2018,2019, 2020,2023)
nyearsEco2<-length(year.eco2)
mestrimEco2<-c(7,7,7,7,8,7)

file.eco2<-c("ALK_ECOCADIZ_2016.xlsx",
             "ALK_ECOCADIZ_2017.xlsx",
             "ALK_ECOCADIZ_2018.xlsx",
             "ALK_ECOCADIZ_2019.xlsx",
             "ALK_ECOCADIZ_2020.xlsx",
             "ALK_ECOCADIZ_2023.xlsx")
sheet.eco2<-c("ALK_GENERAL_BOQUERON",
              "ALK_GENERAL_BOQUERON",
              "ALK_GENERAL_BOQUERON",
              "ALK_GENERAL_BOQUERON",
              "ALK_GENERAL_BOQUERON",
              "ALK_GENERAL_BOQUERON")

# col and row 
#'*biomass*
row.eco2<-rep(3,nyearsEco2)
col.eco2<-rep(9,nyearsEco2)


# Create the dataframe
col_letters2 <- LETTERS[col.eco2]
range_eco2<-paste0(col_letters2,row.eco2)
mesEco2 <- month.name[mestrimEco2]# Convertir los valores numéricos a nombres de meses
data.eco2<-data.frame(year=year.eco2,
                      month=mesEco2,
                      name_file=file.eco2,
                      name_sheet=sheet.eco2,
                      range_bio=range_eco2)

info.data.ecocadiz<-rbind(data.eco1,data.eco2)

total_years<-nyearsEco+nyearsEco2
dist_si_ecocadiz<-data.frame(year=rep(NA,total_years), 
                             step=rep(NA,total_years),
                             area=rep(NA,total_years),
                             weight=rep(NA,total_years))

for (i in 1:total_years){
  if(i<=nyearsEco){
    bio1<-read.xlsx(file.path(path.eco1,file.eco1[i]), 
                    sheet = sheet.eco1[i], 
                    cols=col.eco1[i],
                    rows=row.eco1[i],
                    colNames = F)*0.001 #Para que quede en toneladas (está en kilos)
    dist_si_ecocadiz[i,]<-c(year.eco[i],mestrimEco1[i],"IXa",bio1)
  } else {
    index2<-i-nyearsEco
    bio2<-read.xlsx(file.path(path.eco2,file.eco2[index2]), 
                    sheet = sheet.eco2[index2],
                    cols = col.eco2[index2], 
                    rows = row.eco2[index2],
                    colNames = F)*0.001 #Para que quede en toneladas (está en kilos)
    dist_si_ecocadiz[i,]<-c(year.eco2[index2],mestrimEco2[index2],"IXa",bio2)
  }
}

# Para gadget
landings_ecocadiz<-data.frame(year=dist_si_ecocadiz$year,
                              step=dist_si_ecocadiz$step,
                              area=dist_si_ecocadiz$area,
                              weight=rep(1,total_years))

### b) Length composition ----

#'*----------------------------------------------------*
#### Años 2004,2006,2007,2009, 2010, 2013,2014,2015 
#'*----------------------------------------------------*
col_1a<-c(13,13,13,13,13,11,11,11)
col_2a<-c(17,17,17,17,17,15,15,15)
row_1a<-c(6,6,6,6,6,6,6,6)
row_2a<-c(37,37,37,37,37,37,37,37)
#Pesos medios
colP_1<-c(11,11,11,11,11,9,9,9)
colP_2<-c(14,14,14,14,14,12,12,12)
rowP_1<-c(80,80,80,80,80,90,90,90)
#Talla media a la edad
colL_1<-c(2,2,2,2,2,2,2,2)
colL_2<-c(5,5,5,5,5,5,5,5)
rowL_1<-c(80,80,80,80,80,90,90,90)

#'*--------------------------------*
mybiglist0b <- list()
mybiglist0 <- list()
for (i in 1:nyearsEco){
  name<-paste("Ecocadiz",year.eco[i],sep="")
  a<-read.xlsx(file.path(path.eco1,file.eco1[i]), 
               sheet = sheet.eco1[i],
               cols = col_1a[i]:col_2a[i],
               rows = row_1a[i]:row_2a[i],
               colNames = F)
  names(a)<-c("length","0", "1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.eco[i]
  a$step<-mestrimEco1[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist0[[name]] <- a
#Weight mean  
  b<-read.xlsx(file.path(path.eco1,file.eco1[i]), 
               sheet = sheet.eco1[i],
               cols = colP_1[i]:colP_2[i],
               rows = rowP_1[i],
               colNames = F)
  names(b)<-c("0", "1", "2", "3")
  b$year<-year.eco[i]
  b$step<-mestrimEco1[i]
  b$area<-"IXa"
  b[is.na(b)] <- 0
  mybiglist0b[[name]] <- b
}
dfeco0<-plyr::ldply(mybiglist0,data.frame)
dPeco0<-plyr::ldply(mybiglist0b,data.frame)
#'*----------------------------------------------------*
#### Años 2016,2017,2018,2019,2020 y 2023 
#'*----------------------------------------------------*
col_1<-c(11,11,11,11,11,11)
col_2<-c(15,15,15,15,15,15)
row_1<-c(7,7,7,7,7,7)
row_2<-c(38,38,38,38,38,38)
#Pesos medios
colP_1<-c(9,9,9,9,9,9,9)
colP_2<-c(12,12,12,12,12,12)
rowP_1<-c(91,91,91,91,91,97)
#Talla media a la edad
colL_1<-c(2,2,2,2,2,2)
colL_2<-c(5,5,5,5,5,5)
rowL_1<-c(91,91,91,91,91,97)

mybiglist1b<- list()
mybiglist1 <- list()
for(i in 1:nyearsEco2){
  name<-paste("Ecocadiz",year.eco2[i],sep="")
  a<-read.xlsx(file.path(path.eco2,file.eco2[i]), 
               sheet = sheet.eco2[i],
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  names(a)<-c("length","0", "1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.eco2[i]
  a$step<-mestrimEco2[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist1[[name]] <- a
  #Weight mean  
  b<-read.xlsx(file.path(path.eco2,file.eco2[i]), 
               sheet = sheet.eco2[i],
               cols = colP_1[i]:colP_2[i],
               rows = rowP_1[i],
               colNames = F)
  names(b)<-c("0", "1", "2", "3")
  b$year<-year.eco2[i]
  b$step<-mestrimEco2[i]
  b$area<-"IXa"
  b[is.na(b)] <- 0
  mybiglist1b[[name]] <- b
  
}

dfeco1<-plyr::ldply(mybiglist1,data.frame)
dPeco1<-plyr::ldply(mybiglist1b,data.frame)

dfEco<-rbind(dfeco0,dfeco1)
dPEco<-rbind(dPeco0,dPeco1)


# length composition
ldist_ecocadiz <- dfEco %>%
  mutate(number = X0 + X1 + X2 + X3)%>%
  select(year, step, area, length, number)

dfeco <- dfEco %>%
  select(-length) %>%
  mutate(sum_X = rowSums(select(., starts_with("X")))) %>%
  group_by(year, step) %>%
  summarize(across(.cols = everything()))

### c) age composition ----
adist_ecocadiz <-melt(dfeco,id=c(".id","sum_X","year","step","area"))

adist_ecocadiz <- adist_ecocadiz %>%
  select(-c(.id,sum_X)) %>%
  rename(age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))

### d) age proportion ----
# Calcular el número total de peces para cada año y paso
dfecob <- dfeco %>%
  mutate(X0 = ifelse(sum_X != 0, X0 / sum_X, 0),
         X1 = ifelse(sum_X != 0, X1 / sum_X, 0),
         X2 = ifelse(sum_X != 0, X2 / sum_X, 0),
         X3 = ifelse(sum_X != 0, X3 / sum_X, 0)) %>%
  select(-sum_X)

apropdist_ecocadiz <-melt(dfecob,id=c(".id","year","step","area"))

apropdist_ecocadiz <- apropdist_ecocadiz %>%
  select(-c(.id)) %>%
  rename(age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))


### e) Conditional age-at-length composition ----

year.eco<-c(year.eco0,year.eco1)
nyearsEco<-length(year.eco)

#'*---------------------------------------------------------*
### Años 2004 2006 2007 2009 2010 2013 2014 2015 ----
#'*---------------------------------------------------------*
col_1<-c(1,1,1,1,1,1,1,1,1,1,1,1)
col_2<-c(5,5,5,5,5,5,5,5,5,5,5,5)
row_1<-c(6,6,6,6,6,6,6,6,6,6,6,6)
row_2<-c(37,37,37,37,37,37,37,37,37,37,37,37)

mybiglist0eco <- list()
for (i in 1:nyearsEco){
  name<-paste("Ecocadiz",year.eco[i],sep="")
  a<-read.xlsx(file.path(path.eco1,file.eco1[i]), 
               sheet = sheet.eco1[i],
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  names(a)<-c("length","0","1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.eco[i]
  a$step<-mestrimEco1[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist0eco[[name]] <- a
}
dfeco0a<-plyr::ldply(mybiglist0eco,data.frame)

#### Años 2016,2017,2018,2019,2020 y 2023 ----
col_1<-c(1,1,1,1,1,1)
col_2<-c(5,5,5,5,5,5)
row_1<-c(7,7,7,7,7,7)
row_2<-c(38,38,38,38,38,38)

mybiglist1aeco <- list()
for(i in 1:nyearsEco2){
  name<-paste("Ecocadiz",year.eco2[i],sep="")
  a<-read.xlsx(file.path(path.eco2,file.eco2[i]), 
               sheet = sheet.eco2[i],
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  names(a)<-c("length","0", "1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.eco2[i]
  a$step<-mestrimEco2[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist1aeco[[name]] <- a
}
dfeco1a<-plyr::ldply(mybiglist1aeco,data.frame)
dfeco1a2<-rbind(dfeco0a,dfeco1a)

names(dfeco1a2)<-c(".id", "length", "0", "1","2","3","year","step","area")
eco_alk_ald<-dfeco1a2[2:9]
eco_defalk_ald<-melt(eco_alk_ald,id=c("length","year","step","area" )) 

aldist_ecocadiz<- eco_defalk_ald[, c(2,3,4,5,1,6)]
names(aldist_ecocadiz)<-c("year","step","area","age","length","number")

### f) Watage ----

names(dPEco)<-c(".id", "0", "1","2","3","year","step","area")
eco_wage<-dPEco[2:8]

wageEco<-data.frame(Yr=eco_wage$year,	
                     Seas=eco_wage$step,	
                     Sex=1,	
                     Bio_Pattern=1,	
                     BirthSeas=1,	
                     Fleet=2)	#'*Revisar nºflota pelago!!!*
wageEco<-cbind(wageEco,eco_wage[1:4])

wage_eco<-melt(eco_wage,id=c("year","step","area" )) 
names(wage_eco)<-c("year","step","area","age","weight")

## 3. DATOS *ECOCADIZ-RECLUTAS* ----

### a) Biomasa acústica ----

path.ecoR2<-path.data
year.ecoR2<-c(2012,2014,2015,2016,2018,2019,2020,2021,2022,2023)
nyearsEcoR2<-length(year.ecoR2)
mestrimEcoR2<-c(11,rep(10,9))

file.ecoR2<-c("ALK_ECO-R_2012.xlsx",
              "ALK_ECO-R_2014.xlsx",
              "ALK_ECO-R_2015.xlsx",
              "ALK_ECO-R_2016.xlsx",
              "DATOS_INFORME_ECOR2018_-_ALKS_BOQUERON.xlsx",
              "DATOS_INFORME_ECOR2019_-_ALKS_BOQUERON.xlsx",
              "ALK_ECO-R_2020.xlsx",
              "ALK_ECO-R_2021.xlsx",
              "ALK_ECO-R_2022.xlsx", 
              "ALK_ECO-R_2023.xlsx")
sheet.ecoR2<-c("ALK_GENERAL_BOQUERON",
               "ALK_GENERAL_BOQUERON",
               "ALK_GENERAL_BOQUERON",
               "ALK_GENERAL_BOQUERON",
               "GULF OF CADIZ",
               "GULF OF CADIZ",
               "ALK_GENERAL_BOQUERON",
               "ALK_GENERAL_BOQUERON",
               "ALK_GENERAL_BOQUERON",
               "ALK_GENERAL_BOQUERON")

# col and row 
#'*biomass*
row.ecoR2<-c(3,3,3,3,2,2,3,3,3,3)
col.ecoR2<-c(9,9,9,9,9,9,9,9,9,9)

# Create the dataframe
col_lettersR2 <- LETTERS[col.ecoR2]
range_ecoR2<-paste0(col_lettersR2,row.ecoR2)
mesEcoR2 <- month.name[mestrimEcoR2]# Convertir los valores numéricos a nombres de meses
data.ecoR2<-data.frame(year=year.ecoR2,
                       month=mesEcoR2,
                       name_file=file.ecoR2,
                       name_sheet=sheet.ecoR2,
                       range_bio=range_ecoR2)

total_years<-nyearsEcoR2
dist_si_ECOREC<-data.frame(year=rep(NA,total_years), 
                           step=rep(NA,total_years),
                           area=rep(NA,total_years),
                           weight=rep(NA,total_years))

for (i in 1:total_years){
  bio2<-read.xlsx(file.path(path.ecoR2,file.ecoR2[i]), 
                  sheet = sheet.ecoR2[i],
                  cols = col.ecoR2[i], 
                  rows = row.ecoR2[i],
                  colNames = F)*0.001 #Para que quede en toneladas (está en kilos)
  dist_si_ECOREC[i,]<-c(year.ecoR2[i],mestrimEcoR2[i],"IXa",bio2)
}

landings_ECOREC<-data.frame(year=dist_si_ECOREC$year,
                            step=dist_si_ECOREC$step,
                            area=dist_si_ECOREC$area,
                            weight=rep(1,total_years)) 

### b) Length composition ----

#### Años 2012,2014,2015,2016,2018,2019,2020,2021,2022,2023 
col_1<-c(11,11,11,11,11,11,11,11,11,11)
col_2<-c(15,15,15,15,15,15,15,15,15,15)
row_1<-c(7,7,7,7,6,6,7,7,7,7)
row_2<-c(43,43,43,43,42,42,43,46,46,46)-4
#Pesos medios
colP_1<-c(9,9,9,9,9,9,9,9,9,9)
colP_2<-c(12,12,12,12,12,12,12,12,12,12)
rowP_1<-c(91,91,91,91,90,90,91,97,97,97)
#Talla media a la edad
colL_1<-c(2,2,2,2,2,2,2,2,2,2)
colL_2<-c(5,5,5,5,5,5,5,5,5,5)
rowL_1<-c(91,91,91,91,90,90,91,97,97,97)

mybiglist1d<- list()
mybiglist1 <- list()
for(i in 1:nyearsEcoR2){
  name<-paste("Ecocadiz-Rec",year.ecoR2[i],sep="")
  a<-read.xlsx(file.path(path.ecoR2,file.ecoR2[i]), 
               sheet = sheet.ecoR2[i],
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  names(a)<-c("length","0", "1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.ecoR2[i]
  a$step<-mestrimEcoR2[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist1[[name]] <- a
  #Weight mean  
  b<-read.xlsx(file.path(path.ecoR2,file.ecoR2[i]), 
               sheet = sheet.ecoR2[i],
               cols = colP_1[i]:colP_2[i],
               rows = rowP_1[i],
               colNames = F)
  names(b)<-c("0", "1", "2", "3")
  b$year<-year.ecoR2[i]
  b$step<-mestrimEcoR2[i]
  b$area<-"IXa"
  b[is.na(b)] <- 0
  mybiglist1d[[name]] <- b
  
}

dfecoR1<-plyr::ldply(mybiglist1,data.frame)
dPecoR1<-plyr::ldply(mybiglist1d,data.frame)

# length composition
ldist_ECOREC <- dfecoR1 %>%
  mutate(number = X0 + X1 + X2 + X3)%>%
  select(year, step, area, length, number)

dfecoR <- dfecoR1 %>%
  select(-length) %>%
  mutate(sum_X = rowSums(select(., starts_with("X")))) %>%
  group_by(year, step) %>%
  summarize(across(.cols = everything()))

### c) age composition ----
adist_ecocadizR <-melt(dfecoR,id=c(".id","sum_X","year","step","area"))

adist_ecocadizR <- adist_ecocadizR %>%
  select(-c(.id,sum_X)) %>%
  rename(age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))

### d) age proportion ----
# Calcular el número total de peces para cada año y paso
dfecoRb <- dfecoR %>%
  mutate(X0 = ifelse(sum_X != 0, X0 / sum_X, 0),
         X1 = ifelse(sum_X != 0, X1 / sum_X, 0),
         X2 = ifelse(sum_X != 0, X2 / sum_X, 0),
         X3 = ifelse(sum_X != 0, X3 / sum_X, 0)) %>%
  select(-sum_X)

apropdist_ecocadizR <-melt(dfecoRb,id=c(".id","year","step","area"))

apropdist_ecocadizR <- apropdist_ecocadizR %>%
  select(-c(.id)) %>%
  rename(age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))

### d) Conditional age-at-length composition ----

#### Años 2012,2014,2015,2016,2018,2019,2020,2021,2022,2023 ----
col_1<-c(1,1,1,1,1,1,1,1,1,1)
col_2<-c(5,5,5,5,5,5,5,5,5,5)
row_1<-c(7,7,7,7,6,6,7,7,7,7)
row_2<-c(43,43,43,43,42,42,43,46,46,46)


mybiglist1 <- list()
for(i in 1:nyearsEcoR2){
  name<-paste("Ecocadiz-Rec",year.ecoR2[i],sep="")
  a<-read.xlsx(file.path(path.ecoR2,file.ecoR2[i]), 
               sheet = sheet.ecoR2[i],
               cols = col_1[i]:col_2[i],
               rows = row_1[i]:row_2[i],
               colNames = F)
  names(a)<-c("length","0", "1", "2", "3")
  a$length<-a$length-.25
  a$year<-year.ecoR2[i]
  a$step<-mestrimEcoR2[i]
  a$area<-"IXa"
  a[is.na(a)] <- 0
  mybiglist1[[name]] <- a
}

dfecoR1<-plyr::ldply(mybiglist1,data.frame)

names(dfecoR1)<-c(".id", "length", "0", "1","2","3","year","step","area")
ecoR_alk_ald<-dfecoR1[2:9]
ecoR_defalk_ald<-melt(ecoR_alk_ald,id=c("length","year","step","area" )) 

aldist_ECOREC<- ecoR_defalk_ald[, c(2,3,4,5,1,6)]
names(aldist_ECOREC)<-c("year","step","area","age","length","number")

### f) Watage ----

names(dPecoR1)<-c(".id", "0", "1","2","3","year","step","area")
ecoR_wage<-dPecoR1[2:8]

wageEcoR<-data.frame(Yr=ecoR_wage$year,	
                    Seas=ecoR_wage$step,	
                    Sex=1,	
                    Bio_Pattern=1,	
                    BirthSeas=1,	
                    Fleet=4)	#'*Revisar nºflota ecocadizReclutas!!!*
wageEcoR<-cbind(wageEcoR,ecoR_wage[1:4])

wage_ecoR<-melt(ecoR_wage,id=c("year","step","area" )) 
names(wage_ecoR)<-c("year","step","area","age","weight")

## 4. DATOS *SEINE* ----

### a) Catches ----

# step de capturas están en trimestre
# 
# desde 1989 hasta 2013 trimestrales
# se leen las sheet Q1, Q2, Q3 y Q4 de cada file.seine1
# 
# Primero se extraen las capturas correspondientes a la flota española, y luego a la flota portuguesa correspondiente a las capturas de Algarve.
# 
# No he considerado el descarte en estos cálculos

path.seine1<-path.data
path.seine2<-path.data

year.seine1<-1989:2023
nyearsSeine1<-length(year.seine1)
trim<-1:4
mestrim<-c(2,5,8,11)

file.seine1<-rep(NA,nyearsSeine1)
for(i in 1:nyearsSeine1){
  file.seine1[i]<-paste0("BOQUERON_ALK_",year.seine1[i],".xlsx")}

row.seine1<-rep(2,nyearsSeine1)
col.seine1<-rep(9,nyearsSeine1)

# Create the dataframe
col_letters <- LETTERS[col.seine1]
range_seine<-paste0(col_letters,row.seine1)
mestrimseine <- month.name[mestrim]# Convertir los valores numéricos a nombres de meses
data.seine1<-data.frame(year=year.seine1,
                        name_file=file.seine1,
                        range_bio=range_seine)

### Spain catches ----
landings_seine<-data.frame(year=rep(year.seine1,each=max(trim)),
                           step=rep(year.seine1,each=max(trim)),
                           area=rep(year.seine1,each=max(trim)),
                           weight=rep(year.seine1,each=max(trim)))
a<-1                           
for (i in 1:length(year.seine1)){
  for (j in 1:length(trim)){
    bioq<-read.xlsx(file.path(path.seine1,file.seine1[i]), 
                    sheet = paste(trim[j],"Q",sep=""), 
                    cols = col.seine1[i], 
                    rows = row.seine1[i],
                    colNames = F)*0.001 #Para que quede en toneladas (está en kilos)
    landings_seine[a,]<-c(year.seine1[i],trim[j],"IXa",bioq)
    a<-a+1
  }
}

### Portugal catches ----
Catches_Ptg = read.xlsx(file.path(path.data,"ANE_1989_2016_WGPELA_ENVIO_CORRIGIDO.xlsx"))

Catches_Algarve<- Catches_Ptg %>% 
  filter(AREATYPE=="27.9.a.s.a") %>%  
  group_by(YEAR,SEASON)%>% 
  summarise(totalton=sum(CATON))%>% 
  ungroup() %>% 
  complete(YEAR, SEASON, fill = list(totalton = 0))

Catches_Algarve<-add_row(Catches_Algarve, 
                         YEAR=2017, 
                         SEASON=1:4, 
                         totalton=c(10079.6,1953.5,12871,1206.6)*0.001)
Catches_Algarve<-add_row(Catches_Algarve, 
                         YEAR=2018, 
                         SEASON=1:4, 
                         totalton=c(1431.3,11785.3,52035.7,83.6)*0.001)  
Catches_Algarve<-add_row(Catches_Algarve, 
                         YEAR=2019, 
                         SEASON=1:4, 
                         totalton=c(0,0,4,109))  
Catches_Algarve<-add_row(Catches_Algarve, 
                         YEAR=2020, 
                         SEASON=1:4, 
                         totalton=c(1.6059,67.2002,	73.5084,12.23))
Catches_Algarve<-add_row(Catches_Algarve,
                         YEAR=2021, 
                         SEASON=1:4, 
                         totalton=c(1669, 1204.8,106140,110)*0.001)
#sacado de /home/marga/GADGET/Assessment/Assessment_/Datos_algarve_capturas
Catches_Algarve<-add_row(Catches_Algarve, 
                         YEAR=2022, 
                         SEASON=1:4, 
                         totalton=c(0, 0,0.09,0))
#Catches_Algarve<-add_row(Catches_Algarve, YEAR=2023, SEASON=1:4, totalton=c(5.50, 119.23,29.08,0))
Catches_Algarve<-add_row(Catches_Algarve, 
                         YEAR=2023, 
                         SEASON=1:4,
                         totalton=c(0,17.325, 136.81,0.4))# totalton=c(5.50, 119.23,29.08,0)) correegido el 28 de mayo

# Cambiar los nombres de las columnas para que coincidan con landings_seine
Catches_Algarve <- Catches_Algarve %>%
  rename(year = YEAR, step = SEASON, weight = totalton)

# Agregar la columna 'area' después de 'step'
Catches_Algarve <- Catches_Algarve %>%
  add_column(area ="IXa", .after = "step")

### Total catches ----

landings_seine$weight<-landings_seine$weight+Catches_Algarve$weight

### b) Length compositions ----

year<-1989:2023
mybiglist <- list()
for(i in 1:length(year)){
  for(j in 1:length(trim)){
    name<-paste("fleet",year[i],trim[j],sep="")
    a<-read.xlsx(file.path(path.data,
                           paste("BOQUERON_ALK_",year[i],".xlsx",sep="")), 
                 sheet = paste(trim[j],"Q",sep=""), 
                 cols = 11:15,
                 rows = 6:37,
                 colNames = F)
    names(a)<-c("length","0", "1", "2", "3")
    a$length<-a$length-.25
    a$year<-year[i]
    a$month<-trim[j]
    a$type<-"IXa"
    a[is.na(a)] <- 0
    mybiglist[[name]] <- a
    
  }
}

#year	step	area	length	number
df1<-plyr::ldply(mybiglist,data.frame)

# length composition
ldist_seine <- df1 %>%
  mutate(number = X0 + X1 + X2 + X3)%>%
  select(year, step = month, area = type, length, number)

### c) age composition ----
df2 <- df1 %>%
  select(-length) %>%
  mutate(sum_X = rowSums(select(., starts_with("X")))) %>%
  group_by(year, month) %>%
  summarize(across(.cols = everything()))

adist_seine <-melt(df2,id=c(".id","sum_X","year","month","type"))

adist_seine <- adist_seine %>%
  select(-c(.id,sum_X)) %>%
  rename(year = year, step = month, area = type, age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))

# se eliminan del análisis edad 0 del año 1992 y 2000 y Q2(month=5), 
# en este trimestre no deberían registrarse edades 0
adist_seine$number[adist_seine$age==0&adist_seine$year==1992&adist_seine$step==2] <- 0
adist_seine$number[adist_seine$age==0&adist_seine$year==2000&adist_seine$step==2] <- 0
# # # se elimina del análisis edad 3 del año 2017 y Q4(month=11), la talla media es muy baja, raro
adist_seine$number[adist_seine$age==3&adist_seine$year==2017&adist_seine$step==4] <- 0



df2b <- df2 %>%
  mutate(X0 = ifelse(sum_X != 0, X0 / sum_X, 0),
         X1 = ifelse(sum_X != 0, X1 / sum_X, 0),
         X2 = ifelse(sum_X != 0, X2 / sum_X, 0),
         X3 = ifelse(sum_X != 0, X3 / sum_X, 0)) %>%
  select(-sum_X)

apropdist_seine <-melt(df2b,id=c(".id","year","month","type"))

apropdist_seine <- apropdist_seine %>%
  select(-c(.id)) %>%
  rename(year = year, step = month, area = type, age = variable, number = value) %>%
  mutate(age = case_when(
    age == "X0" ~ 0,
    age == "X1" ~ 1,
    age == "X2" ~ 2,
    age == "X3" ~ 3
  ))

# se eliminan del análisis edad 0 del año 1992 y 2000 y Q2(month=5), 
# en este trimestre no deberían registrarse edades 0
apropdist_seine$number[apropdist_seine$age==0&apropdist_seine$year==1992&apropdist_seine$step==2] <- 0
apropdist_seine$number[apropdist_seine$age==0&apropdist_seine$year==2000&apropdist_seine$step==2] <- 0
# # # se elimina del análisis edad 3 del año 2017 y Q4(month=11), la talla media es muy baja, raro
apropdist_seine$number[apropdist_seine$age==3&apropdist_seine$year==2017&apropdist_seine$step==4] <- 0

### d) Conditional age-at-length compositions ----

mybiglist2 <- list()
#'*--------------------------------*
year<-1989:2023
#'*--------------------------------*
for(i in 1:length(year)){
  for(j in 1:length(trim)){
    name<-paste("fleet",year[i],trim[j],sep="")
    a<-read.xlsx(file.path(path.data,
                      paste("BOQUERON_ALK_",year[i],".xlsx",sep="")), 
                 sheet = paste(trim[j],"Q",sep=""), 
                 cols = 1:5,
                 rows = 5:37,
                 colNames = F)
    names(a)<-c("length","0", "1", "2", "3")
    a<-a[-1,]
    a$year<-year[i]
    a$step<-trim[j]
    a$area<-"IXa"
    a[is.na(a)] <- 0
    mybiglist2[[name]] <- a
    
  }
}


df2<-plyr::ldply(mybiglist2,data.frame)
df2$length<-as.numeric(df2$length)-.25
names(df2)<-c(".id", "length", "0", "1","2","3","year","step","area")
fleet_alk_ald<-df2[2:9]
fleet_defalk_ald<-melt(fleet_alk_ald,id=c("length","year","step","area" )) 

aldist_seine<- fleet_defalk_ald[, c(2,3,4,5,1,6)]
names(aldist_seine)<-c("year","step","area","age","length","number")

# se eliminan del análisis edad 0 del año 1992 y 2000 y Q2(month=5), 
# en este trimestre no deberían registrarse edades 0
aldist_seine$number[aldist_seine$age==0&aldist_seine$year==1992&aldist_seine$step==2] <- 0
aldist_seine$number[aldist_seine$age==0&aldist_seine$year==2000&aldist_seine$step==2] <- 0
# # # se elimina del análisis edad 3 del año 2017 y Q4(month=11), la talla media es muy baja, raro
aldist_seine$number[aldist_seine$age==3&aldist_seine$year==2017&aldist_seine$step==4] <- 0

### e) Pesos medios por edad ----
mybiglist3 <- list()
#'*--------------------------------*
year<-1989:2023
#'*--------------------------------*
for(i in 1:length(year)){
  for(j in 1:length(trim)){
    name<-paste("fleet",year[i],trim[j],sep="")
    if(year[i]>2013){
    row<-90}
    else{
      row<-80
    }
    a<-read.xlsx(file.path(path.data,
                               paste("BOQUERON_ALK_",year[i],".xlsx",sep="")),
                     sheet = paste(trim[j],"Q",sep=""),
                     cols = 9:12,
                     rows = row,
                     colNames = F)
    names(a)<-c("0", "1", "2", "3")
    a$year<-year[i]
    a$step<-trim[j]
    a$area<-"IXa"
    a[is.na(a)] <- 0
    mybiglist3[[name]] <- a
  }}

df3<-plyr::ldply(mybiglist3,data.frame)

names(df3)<-c(".id", "0", "1","2","3","year","step","area")
fleet_wage<-df3[2:8]

wageSeine<-data.frame(Yr=fleet_wage$year,	
                      Seas=fleet_wage$step,	
                      Sex=1,	
                      Bio_Pattern=1,	
                      BirthSeas=1,	
                      Fleet=1)	
wageSeine<-cbind(wageSeine,fleet_wage[1:4])

wage_seine<-melt(fleet_wage,id=c("year","step","area" )) 
names(wage_seine)<-c("year","step","area","age","weight")

### f) Talla media por edad ----
mybiglist4 <- list()
#'*--------------------------------*
year<-1989:2023
#'*--------------------------------*
for(i in 1:length(year)){
  for(j in 1:length(trim)){
    name<-paste("fleet",year[i],trim[j],sep="")
    if(year[i]>2013){
      row<-90}
    else{
      row<-80
    }
    a<-read.xlsx(file.path(path.data,
                           paste("BOQUERON_ALK_",year[i],".xlsx",sep="")),
                 sheet = paste(trim[j],"Q",sep=""),
                 cols = 2:5,
                 rows = row,
                 colNames = F)
    names(a)<-c("0", "1", "2", "3")
    a$year<-year[i]
    a$step<-trim[j]
    a$area<-"IXa"
    a[is.na(a)] <- 0
    mybiglist4[[name]] <- a
  }}

df4<-plyr::ldply(mybiglist4,data.frame)

names(df4)<-c(".id", "0", "1","2","3","year","step","area")
fleet_Lage<-df4[2:8]
Lage_seine<-melt(fleet_Lage,id=c("year","step","area" )) 
names(Lage_seine)<-c("year","step","area","age","weight")


# Formato para ModelWizar  ----
time<-data.frame(steps=4,year_min=1989,year_max=2024)
area<-"IXa"
stock<-data.frame(renewal_step=0,	
                  lg_min=2.5,	
                  lg_max=22,	
                  lg_size=0.5,	
                  age_min=0,	
                  age_max=3,	
                  name="anch")
fleet<-data.frame(step_active=c(0,2,1,3),
                  landings=c(rep("weight",4)),
                  ldist=c(rep("number",4)),
                  aldist=c(rep("number",4)),
                  year_min=c(1989,2004,1998,2012),
                  year_max=c(2024,2023,2024,2023),
                  name=c("SEINE","ECOCADIZ","PELAGO","ECORECLUTAS"))
abund<-data.frame(step_active=c(2,1,3),
                  dist=c(rep("number",3)),
                  ldist=c(rep("none",3)),
                  aldist=c(rep("none",3)),
                  year_min=c(2004,1998,2012),
                  year_max=c(2023,2024,2023),
                  name=c("si_ECOCADIZ","si_PELAGO","si_ECORECLUTAS"))

#########################################################
# Write inputData.RData ----
save(info.data.pela,info.data.ecocadiz,data.ecoR2,
     dist_si_pelago, dist_si_ecocadiz,dist_si_ECOREC,
     ldist_pelago,ldist_ecocadiz,ldist_ECOREC,ldist_seine,
     adist_pelago,adist_ecocadiz,adist_ecocadizR,adist_seine,
     aldist_pelago,aldist_ecocadiz,aldist_ECOREC,aldist_seine,
     wage_seine,
     time,area,stock,fleet,abund,
     file="./data/input.RData")


# Formato para ModelWizar  ----
wb <- createWorkbook()
#'*-------------------------------------------------------------*
addWorksheet(wb, "time")
writeData(wb, sheet = "time", x = time,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "area")
writeData(wb, sheet = "area", x = area,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "stock")
writeData(wb, sheet = "stock", x = stock,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "fleet")
writeData(wb, sheet = "fleet", x = fleet,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "abund")
writeData(wb, sheet = "abund", x = abund,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "dist_si_ecocadiz")
writeData(wb, sheet = "dist_si_ecocadiz", x = dist_si_ecocadiz,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "dist_si_pelago")
writeData(wb, sheet = "dist_si_pelago", x = dist_si_pelago,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "dist_si_ECOREC")
writeData(wb, sheet = "dist_si_ECOREC", x = dist_si_ECOREC,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "landings_seine")
writeData(wb, sheet = "landings_seine", x = landings_seine,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "ldist_seine")
writeData(wb, sheet = "ldist_seine", x = ldist_seine,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "aldist_seine")
writeData(wb, sheet = "aldist_seine", x = aldist_seine,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "landings_ecocadiz")
writeData(wb, sheet = "landings_ecocadiz", x = landings_ecocadiz,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "ldist_ecocadiz")
writeData(wb, sheet = "ldist_ecocadiz", x = ldist_ecocadiz,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "aldist_ecocadiz")
writeData(wb, sheet = "aldist_ecocadiz", x =aldist_ecocadiz,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "landings_pelago")
writeData(wb, sheet = "landings_pelago", x = landings_pelago,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "ldist_pelago")
writeData(wb, sheet = "ldist_pelago", x = ldist_pelago,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "aldist_pelago")
writeData(wb, sheet = "aldist_pelago", x = aldist_pelago,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "landings_ECOREC")
writeData(wb, sheet = "landings_ECOREC", x = landings_ECOREC,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "ldist_ECOREC")
writeData(wb, sheet = "ldist_ECOREC", x = ldist_ECOREC,rowNames = F)
#'*-------------------------------------------------------------*
addWorksheet(wb, "aldist_ECOREC")
writeData(wb, sheet = "aldist_ECOREC", x = aldist_ECOREC,rowNames = F)

#'*-------------------------------------------------------------*
addWorksheet(wb, "wageSeine")
writeData(wb, sheet = "wageSeine", x = wageSeine,rowNames = F)
addWorksheet(wb, "wageEco")
writeData(wb, sheet = "wageEco", x = wageEco,rowNames = F)
addWorksheet(wb, "wagePela")
writeData(wb, sheet = "wagePela", x = wagePela,rowNames = F)
addWorksheet(wb, "wageEcoR")
writeData(wb, sheet = "wageEcoR", x = wageEcoR,rowNames = F)

#'*-------------------------------------------------------------*
#'
saveWorkbook(wb, "data/anch2024.xlsx",overwrite = TRUE)





