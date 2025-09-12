# Test:
# TOPIC:    PANEL DATA MODELS
# AUTHOR:   POOYA SHIRAZI
# DATE:     JANUARY 02 2019

# Initial Setting -------------------------------------------------------------------------------------------------

# Remove All Objects From Environment And Clear Console:
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/clearWorkspace.R")
clearWorkspace()

# Loading (Or Install) Required R Packages:
source("https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/installPackages.R")
installPackages('dplyr', 'plm', 'hydroGOF')

# Load And Modified Data ------------------------------------------------------------------------------------------

# Load Required Data: Monthly Historical Data for 9 Razavi Khorasan Province Meteorological Stations (KGCC: 'BSk' Class) + ET (penman and hargreaves).
raw_data <- utils::read.csv(file = "Articles/A01/data/data-02.csv", header = TRUE)

# Create Index:
raw_data$tid <- raw_data$year * 100 + raw_data$month
raw_data$sid <- as.numeric(x = raw_data$station)

# Remove NA From et0.p Column:
data <- raw_data[!is.na(raw_data$et0.p),]

# Create A List Of Data:
data_list <- list(Spring = subset(x = data, month >= 3 & month <= 5),
                  Summer = subset(x = data, month >= 6 & month <= 8),
                  Fall = subset(x = data, month >= 9 & month <= 11),
                  Winter = subset(x = data, month == 12 | month == 1 | month == 2),
                  Year = data)

rm(list = c('data', 'raw_data'))

y = 'et0.p'

fm <- et0.p ~ tm + ws

result <- data.frame(para = rownames(hydroGOF::gof(sim = rnorm(n = 10, mean = 0, sd = 1),
                                                   obs = rnorm(n = 10, mean = 1, sd = 2))))

for (i in names(data_list))
{
    pdata <- pdata.frame(x = data_list[[i]], index = c('sid','tid'))
    
    result[[paste0('P-H_', i)]] <- hydroGOF::gof(as.numeric(pdata$et0.p),
                                                 as.numeric(pdata$et0.h))
    
    model_01 <- plm::plm(formula = fm,
                         data = pdata,
                         model = 'pooling')
    result[[paste0('model_01_', i)]] <- hydroGOF::gof(as.numeric(model_01$model[[y]]),
                                                      as.numeric(model_01$model[[y]] - model_01$residuals))
    
    model_02 <- plm::plm(formula = fm,
                         data = pdata,
                         effect = 'individual',
                         model = 'within')
    result[[paste0('model_02_', i)]] <- hydroGOF::gof(as.numeric(model_02$model[[y]]),
                                                      as.numeric(model_02$model[[y]] - model_02$residuals))
    
    model_03 <- plm::plm(formula = fm,
                         data = pdata,
                         effect = 'time',
                         model = 'within')
    result[[paste0('model_03_', i)]] <- hydroGOF::gof(as.numeric(model_03$model[[y]]),
                                                      as.numeric(model_03$model[[y]] - model_03$residuals))
    
    model_04 <- plm::plm(formula = fm,
                         data = pdata,
                         effect = 'twoways',
                         model = 'within')
    result[[paste0('model_04_', i)]] <- hydroGOF::gof(as.numeric(model_04$model[[y]]),
                                                      as.numeric(model_04$model[[y]] - model_04$residuals))
    
    model_05 <- plm::pggls(formula = fm,
                           data = pdata,
                           effect = 'individual',
                           model = 'within')
    result[[paste0('model_05_', i)]] <- hydroGOF::gof(as.numeric(model_05$model[[y]]),
                                                      as.numeric(model_05$model[[y]] - model_05$residuals))
    
    model_06 <- plm::pggls(formula = fm,
                           data = pdata,
                           effect = 'time',
                           model = 'within')
    result[[paste0('model_06_', i)]] <- hydroGOF::gof(as.numeric(model_06$model[[y]]),
                                                      as.numeric(model_06$model[[y]] - model_06$residuals))
    
    model_07 <- plm::pvcm(formula = fm,
                          data = pdata,
                          effect = 'individual',
                          model = 'within')
    result[[paste0('model_07_', i)]] <- hydroGOF::gof(as.numeric(model_07$model[[y]]),
                                                      as.numeric(model_07$model[[y]] - model_07$residuals))
    
    model_08 <- plm::plm(formula = fm,
                         data = pdata,
                         effect = 'individual',
                         model = 'random')
    result[[paste0('model_08_', i)]] <- hydroGOF::gof(as.numeric(model_08$model[[y]]),
                                                      as.numeric(model_08$model[[y]] - model_08$residuals))
    
    model_09 <- plm::plm(formula = fm,
                         data = pdata,
                         effect = 'time',
                         model = 'random')
    result[[paste0('model_09_', i)]] <- hydroGOF::gof(as.numeric(model_09$model[[y]]),
                                                      as.numeric(model_09$model[[y]] - model_09$residuals))
    
    model_10 <- plm::plm(formula = fm,
                         data = pdata,
                         effect = 'twoways',
                         model = 'random')
    result[[paste0('model_10_', i)]] <- hydroGOF::gof(as.numeric(model_10$model[[y]]),
                                                      as.numeric(model_10$model[[y]] - model_10$residuals))
}
# 
# 
# a <- pgmm(formula = et0.p ~ lag(et0.p) + td + tm + ws | lag(et0.p, 2:99),
#           data = pdata, 
#           model = "onestep",
#           effect = "individual",
#           transformation = "ld")
