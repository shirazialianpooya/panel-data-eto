
# Remove All Objects From Environment And Clear Console:
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/clearWorkspace.R")
clearWorkspace()

# Loading (Or Install) Required R Packages:
source("https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/installPackages.R")
installPackages('SPEI','dplyr')

# Load Monthly Historical Data for 15 Razavi Khorasan Province Meteorological Stations:
data <- read.csv(file = "Articles/A01/data/data-01.csv")

# Load Koeppen - Geiger’s climate Classification of Monthly Historical Data for 15 Razavi Khorasan Province Meteorological Stations:
KGCC <- read.csv(file = "Articles/A01/result/r01-KGCC.csv")

# Convert Sunshine From Monthly Sum To Monthly Mean:
data$sh <- data$sh / c(31,28,31,30,31,30,31,31,30,31,30,31)

# Convert Wind Speed From knot To m/s:
data$ws <- data$ws / 1.94384

# Reorder data Base On Station Name:
data <- data[order(data$station),]

# Select 'BSk' Class of KGCC Stations:
selected.data <- data %>%
    dplyr::filter(station %in% KGCC[KGCC$class == 'BSk',][[1]]) %>% 
    droplevels()

# Convert 'selected.data' to List:
selected.data.list <- split(x = selected.data,
                            f = selected.data$station)

# Create Function To Calculate 'penman' ET:
myfun_penman <- function(x) {
    mutate(.data = x,
           et0.p = as.numeric(SPEI::penman(Tmin = x[, 'tn'],
                                           Tmax = x[, 'tx'],
                                           U2 = x[, 'ws'],
                                           tsun = x[, 'sh'],
                                           RH = x[, 'rh'],
                                           lat = x[1, 'latitude'],
                                           z = x[1, 'altitude'],
                                           na.rm = TRUE)))
}

# Create Function To Calculate 'hargreaves' ET:
myfun_hargreaves <- function(x) {
    mutate(.data = x,
           et0.h = as.numeric(SPEI::hargreaves(Tmin = x[, 'tn'],
                                               Tmax = x[, 'tx'],
                                               Pre = x[, 'pr'],
                                               lat = x[1, 'latitude'],
                                               na.rm = TRUE)))
}

# Calculate 'penman' ET:
selected.data.list <- lapply(X = selected.data.list,
                             FUN = myfun_penman)

# Calculate 'hargreaves' ET:
selected.data.list <- lapply(X = selected.data.list,
                             FUN = myfun_hargreaves)

# Convert data List To Dataframe:
data.main <- do.call(what = rbind.data.frame,
                     args = selected.data.list)

# Save Data:
write.csv(x = data.main,
          file = "Articles/A01/data/data-02.csv",
          row.names = FALSE)
