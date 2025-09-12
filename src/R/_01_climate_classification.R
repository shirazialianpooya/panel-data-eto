
# Remove All Objects From Environment And Clear Console:
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/clearWorkspace.R")
clearWorkspace()

# Loading (Or Install) Required R Packages:
source("https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/installPackages.R")
installPackages('tidyverse','ClimClass', 'lubridate')

# Load Monthly Historical Data for 15 Razavi Khorasan Province Meteorological Stations:
data <- readRDS(file = 'Articles/A03/data/data_monthly_final.rds') %>% 
    mutate(year = year(date),
           month = month(date)) %>% 
    select(station, year, month, tn, tx, tm, pr) %>% 
    na.omit()

# Convert to List:
data.list <- split(x = data[,c('year', 'month', 'tn', 'tx', 'tm', 'pr')], f = data$station)

# Creates climate mean monthly values from a monthly series of temperature and precipitation:
Climate.Normals <- lapply(X = lapply(X = data.list,
                                     FUN = setNames, 
                                     c('year', 'month', 'Tn', 'Tx', 'Tm', 'P')),
                          FUN = ClimClass::climate,
                          max.perc.missing = 50)

# Koeppen - Geiger’s climate classification
KG.Climate.Classification <- do.call(what = rbind.data.frame,
                                     args = lapply(X = Climate.Normals,
                                                   FUN = ClimClass::koeppen_geiger,
                                                   A_B_C_special_sub.classes = FALSE,
                                                   clim.resume_verbose = TRUE,
                                                   class.nr = FALSE))

KGCC <- data.frame(station = rownames(KG.Climate.Classification),
                   class = KG.Climate.Classification[,'class'])

# Sorting Class
KGCC <- KGCC[order(KGCC$class),]

# Export Koeppen - Geiger’s climate classification
write.csv(x = KGCC,
          file = "Articles/A01/result/_03_KGCC.csv",
          row.names = FALSE)

