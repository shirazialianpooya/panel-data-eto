# TOPIC:    PANEL DATA MODELS
# AUTHOR:   POOYA SHIRAZI
# DATE:     JANUARY 02 2019

# Initial Setting -------------------------------------------------------------------------------------------------

# Remove All Objects From Environment And Clear Console:
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/clearWorkspace.R")
clearWorkspace()

# Loading Required R Packages:
library(package = dplyr)        # dplyr: A Fast, Consistent Tool For Working With Data Frame Like Objects, Both In Memory And Out Of Memory.
library(package = plm)          # plm: A Set Of Estimators And Tests For Panel Data Econometrics.
library(package = lubridate)    # lubridate: Make Dealing with Dates a Little Easier.

# Load And Modified Data ------------------------------------------------------------------------------------------

# Load Required Data: Monthly Historical Data for 9 Razavi Khorasan Province Meteorological Stations (KGCC: 'BSk' Class) + ET (penman and hargreaves).
raw_data <- utils::read.csv(file = "Articles/A01/data/data-02.csv", header = TRUE) %>%
    mutate(date = ymd(paste0(year, '-', month, '-', '15'))) %>%
    mutate(tid = year * 100 + month) %>% 
    mutate(sid = as.numeric(x = station)) %>% 
    filter(year >= 1971)

pdata <- raw_data[!is.na(raw_data$et0.p),] %>% 
    pdata.frame(index = c('sid','tid'))

# Panel Data Models -----------------------------------------------------------------------------------------------

# Estimat Method: Pooled OLS
ols <- plm::plm(formula = et0.p ~ tm + ws,
                data = pdata,
                model = 'pooling')

# Estimat Method: Fixed Effects
fixed_effects <- plm::plm(formula = et0.p ~ tm + ws,
                          data = pdata,
                          effect = 'twoways',
                          model = 'within')

# Estimat Method: Random Effects
random_effects <- plm::plm(formula = et0.p ~ tm + ws,
                           data = pdata,
                           effect = 'twoways',
                           model = 'random')

# Panel Data Tests ------------------------------------------------------------------------------------------------

# Testing for unit roots/stationarity
tseries::adf.test(x =  pdata$et0.p, k = 2)
plm::purtest()

# Multicollinearity
car::vif(mod = ols)

# Lagrange Multiplier Tests For Panel Models:
# Test Of Individual And/Or Time Effects For Panel Models.
# These Lagrange Multiplier Tests Use Only The Residuals Of The Pooling Model.
plm::plmtest(x = ols)

# Fixed or OLS: F Test for Individual and/or Time Effects
# Test of individual and/or time effects based on the comparison of the within and the pooling model.
plm::pFtest(fixed_effects, ols)

# Fixed or Random: Hausman Test for Panel Models
plm::phtest(fixed_effects, random_effects)

# Tests of Cross-Section Dependence for Panel Models
plm::pcdtest(x = fixed_effects)

# Test of Serial Correlation for (the idiosyncratic component of) the Errors in Panel Models.
plm::pbgtest(x = fixed_effects)
plm::pdwtest(x = fixed_effects)

# Performs the Breusch-Pagan Test Against Heteroskedasticity.
lmtest::bptest(formula = et0.p ~ tm + ws + sid,
               data = pdata,
               studentize = FALSE)

# Controlling for Heteroskedasticity: Fixed Effects
lmtest::coeftest(x = within)
lmtest::coeftest(x = within, vcov = vcovHC)
lmtest::coeftest(x = within, vcov = vcovHC(x = within, method = "arellano"))
lmtest::coeftest(x = within, vcov = vcovHC(x = within, type = "HC3"))
