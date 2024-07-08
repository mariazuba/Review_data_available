

library("icesTAF")


# create initial directories and R scripts for a new TAF analysis
# ¦--bootstrap   
# ¦   --initial 
# ¦       --data
# ¦--data.R      
# ¦--model.R     
# ¦--output.R    
# °--report.R    
#taf.skeleton()

# crea DATA.bib
# SS3 scenarios
draft.data(
  originator = "SS3",
  year = 2024,
  title = "ane27.9a",
  period = "1989-2024",file=TRUE)
taf.bootstrap() # se obtiene los datos de la carpeta boot/data

# crea SOFTWARE.bib
draft.software('boot/initial/software/ss3',file=TRUE)
taf.bootstrap() # se obtiene los datos de la carpeta boot/data, 


