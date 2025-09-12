# TOPIC:    PANEL DATA MODELS
# AUTHOR:   POOYA SHIRAZI
# DATE:     JANUARY 02 2019

# Initial Setting -------------------------------------------------------------------------------------------------

# Remove All Objects From Environment And Clear Console:
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/clearWorkspace.R")
clearWorkspace()

source(file = 'Articles/A01/r/PD.R')

# Loading Required R Packages:
library(plm)
library(broom)
library(dplyr)
library(ggplot2)
library(lubridate)
library(hydroGOF)
library(openair)

# Load And Modified Data ------------------------------------------------------------------------------------------

# Monthly Data:
data <- readRDS(file = 'Articles/A01/data/data_monthly_final.rds') %>% 
    arrange(sid, tid)

# Model Development -----------------------------------------------------------------------------------------------
# Define Dependent Variable:
Y = 'et0.p'

# Define InDependent Variable:
X = list(year   = 'tm + ws',
         winter = 'tm + ws',
         spring = 'tm + ws',
         summer = 'tm + ws',
         autumn = 'tm + ws')

# X = list(year   = 'z850 + q1000 + u850 + tcc + npr',
#          winter = 'z850 + r850 + q850 + t850 + v1000 + msl + npr',
#          spring = 'z850 + pv500 + t500 + u500 + vo500 + tcc + npr',
#          summer = 'z1000 + v850 + w850 + msl + sp + npr',
#          autumn = 'z500 + q1000 + v500 + r500 + pv850 + npr')

result <- PD(data = data,
             Y = Y,
             X = X,
             SampleSize = 0.30,
             seed = 123)

# Extract GOF -----------------------------------------------------------------------------------------------------

for (t in names(result)[-3]) 
{
    i = 1
    re <- data.frame(season = NA, station = NA, 
                     MAE_pd = NA, MSE_pd = NA, RMSE_pd = NA, RSR_pd = NA, NSE_pd = NA, R2_pd = NA, PMARE_pd = NA, RE_pd = NA,
                     MAE_ols = NA, MSE_ols = NA, RMSE_ols = NA, RSR_ols = NA, NSE_ols = NA, R2_ols = NA, PMARE_ols = NA, RE_ols = NA)
    
    gof <- data.frame(para = c('MAE', 'MSE', 'RMSE', 'RSR', 'NSE', 'R2', 'PMARE', 'RE'))
    
    for (s in names(result[[t]]))
    {
        d <- result[[t]][[s]]
        
        for (st in c(as.character(unique(d$station)), 'all'))
        {
            if (st == 'all')
            {
                dd <- d
            } else {
                dd <- d %>% filter(station == st)
            }
            
            for (cn in colnames(dd)[5:length(colnames(dd))])
            {
                gof[[cn]] <- round(x = c(MAE = hydroGOF::mae(obs = as.numeric(dd[[Y]]),
                                                             sim = as.numeric(dd[[cn]])),
                                         MSE = hydroGOF::mse(obs = as.numeric(dd[[Y]]),
                                                             sim = as.numeric(dd[[cn]])),
                                         RMSE = hydroGOF::rmse(obs = as.numeric(dd[[Y]]),
                                                               sim = as.numeric(dd[[cn]])),
                                         RSR = hydroGOF::rsr(obs = as.numeric(dd[[Y]]),
                                                             sim = as.numeric(dd[[cn]])),
                                         NSE = hydroGOF::NSE(obs = as.numeric(dd[[Y]]),
                                                             sim = as.numeric(dd[[cn]])),
                                         R2 = (hydroGOF::rPearson(obs = as.numeric(dd[[Y]]),
                                                                  sim = as.numeric(dd[[cn]])))^2,
                                         PMARE = PMARE(obs = as.numeric(dd[[Y]]),
                                                       sim = as.numeric(dd[[cn]])),
                                         RE = RE(obs = as.numeric(dd[[Y]]),
                                                 sim = as.numeric(dd[[cn]]))),
                                   digits = 2)
            }
            re[i,] = c(s, st, gof[, 'pd_fe_tw'], gof[, 'ols'])
            i = i + 1
        }
    }
    re <- re %>% 
        select(season, station, MAE_pd, RMSE_pd, RE_pd, RSR_pd, MSE_pd, NSE_pd, R2_pd, PMARE_pd, 
               MAE_ols, RMSE_ols, RE_ols, RSR_ols, MSE_ols, NSE_ols, R2_ols, PMARE_ols)
    
    write.csv(x = re,
              file = paste0('Articles/A01/result/', Y, '/gof_', t, '_', Y, '.csv'),
              row.names = FALSE)
}

# Panel Data Tests ------------------------------------------------------------------------------------------------


# Lagrange Multiplier Tests For Panel Models:
# Test Of Individual And/Or Time Effects For Panel Models.
# These Lagrange Multiplier Tests Use Only The Residuals Of The Pooling Model.
plm::plmtest(x = result$PDTest$ols)

# Fixed or OLS: F Test for Individual and/or Time Effects
# Test of individual and/or time effects based on the comparison of the within and the pooling model.
plm::pFtest(result$PDTest$pd_fe_i, result$PDTest$ols)
plm::pFtest(result$PDTest$pd_fe_tw, result$PDTest$ols)

# Fixed or Random: Hausman Test for Panel Models
plm::phtest(result$PDTest$pd_fe_i, result$PDTest$pd_re_i)
plm::phtest(result$PDTest$pd_fe_tw, result$PDTest$pd_re_tw)

# # Testing for unit roots/stationarity
# tseries::adf.test(x = data_Y$tm, k = 2)

# Multicollinearity
car::vif(mod = result$PDTest$ols)

# Tests of Cross-Section Dependence for Panel Models
plm::pcdtest(x = result$PDTest$pd_fe_i)
plm::pcdtest(x = result$PDTest$pd_fe_tw)

# Test of Serial Correlation for (the idiosyncratic component of) the Errors in Panel Models.
plm::pbgtest(x = result$PDTest$pd_fe_tw)
plm::pdwtest(x = result$PDTest$pd_fe_tw)

# # Performs the Breusch-Pagan Test Against Heteroskedasticity.
# lmtest::bptest(formula = et0.p ~ tm + ws + sid,
#                data = pdata,
#                studentize = FALSE)

# Controlling for Heteroskedasticity: Fixed Effects
lmtest::coeftest(x = result$PDTest$pd_fe_tw)
lmtest::coeftest(x = result$PDTest$pd_fe_tw, vcov = vcovHC)
lmtest::coeftest(x = result$PDTest$pd_fe_tw, vcov = vcovHC(x = result$PDTest$pd_fe_tw, method = "arellano"))
lmtest::coeftest(x = result$PDTest$pd_fe_tw, vcov = vcovHC(x = result$PDTest$pd_fe_tw, type = "HC3"))



# Taylor Diagram
for (ss in names(x = result$Train))
{
    pdf(file = paste0('Articles/A03/result/', Y, '/TD_', ss, '.pdf'), width = 4, height = 4)
    openair::TaylorDiagram(mydata = data.frame('PenmanMonteithETo' = result$Train[[ss]]$et0.p,
                                               'HargreavesSamaniETo' = result$Train[[ss]]$et0.h,
                                               'PanelDataETo' = result$Train[[ss]]$pd_fe_tw,
                                               'OLSETo' = result$Train[[ss]]$ols) %>%
                               reshape2::melt(id = 'PenmanMonteithETo'),
                           obs = "PenmanMonteithETo",
                           mod = "value",
                           group = "variable",
                           cols = c('black','black','black'),
                           annotate = 'RMSE',
                           key.title = '',
                           key.pos = '',
                           pch = c(15,19,17),
                           cex = 1.5)
    dev.off()
}




# RMSE Train and Test --------------------------------------------------------------------------------------------------
D_Train <- read.csv(file = paste0('Articles/A01/result/', Y, '/gof_Train_', Y, '.csv'))
D_Test <- read.csv(file = paste0('Articles/A01/result/', Y, '/gof_Test_', Y, '.csv'))
D_Plot <- rbind(data.frame(season = D_Train$season, RMSE = D_Train$RMSE_pd, MODEL = 'Train'),
                data.frame(season = D_Test$season, RMSE = D_Test$RMSE_pd, MODEL = 'Test')) %>% 
    mutate(season = factor(x = season, levels = c('spring', 'summer', 'autumn', 'winter', 'year')))
m <- ggplot(data = D_Plot, mapping = aes(x = season, y = RMSE, fill = MODEL)) +
    theme_bw() +
    geom_boxplot() +
    xlab('') + 
    ylab('Root Mean Squared Error (mm)') +
    scale_fill_manual(values = c("grey75", "white")) +
    theme(text = element_text(size = 14),
          legend.position="none")
ggsave(filename = paste0('Articles/A01/result/', Y, '/RMSE_Plot_', Y, '.pdf'), plot = m, width = 6, height = 4)

# RMSE PD and OLS ------------------------------------------------------------------------------------------------------
for (rmse in c('Train', 'Test'))
{
    data_pd_ols <- read.csv(file = paste0('Articles/A01/result/', Y, '/gof_', rmse, '_', Y, '.csv'))
    
    m <- rbind(data.frame(season = data_pd_ols$season, RMSE = data_pd_ols$RMSE_pd, MODEL = 'PD'),
               data.frame(season = data_pd_ols$season, RMSE = data_pd_ols$RMSE_ols, MODEL = 'OLS')) %>% 
        mutate(season = factor(x = season, levels = c('spring', 'summer', 'autumn', 'winter', 'year'))) %>% 
        ggplot(mapping = aes(x = season, y = RMSE, fill = MODEL)) +
        theme_bw() +
        geom_boxplot() +
        xlab('') + 
        ylab('Root Mean Squared Error (mm)') +
        scale_fill_manual(values = c("grey75", "white")) +
        theme(text = element_text(size = 14),
              legend.position="none")
    ggsave(filename = paste0('Articles/A01/result/', Y, '/RMSE_pd_ols_', rmse, '_', Y, '.pdf'), plot = m, width = 6, height = 4)
}


# RMSE PD and OLS and HS ET0 for Each Stations-------------------------------------------------------------------------------------

data = data[!is.na(data[[Y]]), ]

for (rmse in c('Train', 'Test')) 
{
    for (st in unique(data$station))
    {
        d <- data %>% 
            filter(station == st) %>% 
            mutate(season = factor(x = season, levels = c('spring', 'summer', 'autumn', 'winter')))
        
        RMSE_et0.h <- d %>% 
            select(station, date, season, et0.p, et0.h) %>%
            group_by(season) %>% 
            summarise(RMSE = hydroGOF::rmse(sim = et0.h,
                                     obs = et0.p))
        
        RMSE <- read.csv(file = paste0('Articles/A01/result/', Y, '/gof_', rmse, '_', Y, '.csv')) %>% 
            select(season, station, RMSE_pd, RMSE_ols) %>% 
            filter(station == st) %>% 
            filter(!(season == 'year')) %>% 
            mutate(season = factor(x = season, levels = c('spring', 'summer', 'autumn', 'winter')))
        
        m <- ggplot(data = d, mapping = aes_string(x = 'season', y = Y)) +
            geom_boxplot(fill = "grey75") +
            geom_point(data = RMSE_et0.h, mapping = aes(x = season, y = RMSE*20, group = 1), size = 4, pch = 15) +
            geom_line(data = RMSE_et0.h, mapping = aes(x = season, y = RMSE*20, group = 1), size = 0.5, linetype = "dashed") +
            geom_point(data = RMSE, mapping = aes(x = season, y = RMSE_pd*20, group = 1), size = 4, pch = 19) +
            geom_line(data = RMSE, mapping = aes(x = season, y = RMSE_pd*20, group = 1), size = 0.5, linetype = "dashed") +
            geom_point(data = RMSE, mapping = aes(x = season, y = RMSE_ols*20, group = 1), size = 4, pch = 17) +
            geom_line(data = RMSE, mapping = aes(x = season, y = RMSE_ols*20, group = 1), size = 0.5, linetype = "dashed") +
            scale_y_continuous(sec.axis = sec_axis(~./20, name = 'Root Mean Squared Error (mm)')) +
            theme_bw() +
            xlab('') + 
            ylab('Reference Evapotranspiration (mm)') +
            theme(text = element_text(size = 14))
        
        ggsave(filename = paste0('Articles/A01/result/', Y, '/RMSE_OBS_', rmse, '_', st, '_', Y,'.pdf'), plot = m, width = 6, height = 4)
    }
}


