# TOPIC:    PANEL DATA MODELS
# AUTHOR:   POOYA SHIRAZI
# DATE:     JANUARY 02 2019

# Initial Setting -------------------------------------------------------------------------------------------------

# Remove All Objects From Environment And Clear Console:
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/clearWorkspace.R")
clearWorkspace()

# Random Number Generation
set.seed(123)

# Loading Required R Packages:
library(plm)
library(lubridate)
library(dplyr)
library(caTools)
library(ggplot2)
library(naniar)
library(ExPanDaR)
library(ggExtra)
library(hydroGOF)
library(plotrix)
library(openair)
library(broom)

# Load And Modified Data ------------------------------------------------------------------------------------------

# Load Required Data: Monthly Historical Data for 9 Razavi Khorasan Province Meteorological Stations (KGCC: 'BSk' Class) + ET (penman and hargreaves).
raw_data <- utils::read.csv(file = "Articles/A01/data/data-02.csv", header = TRUE) %>%
    mutate(date = ymd(paste0(year, '-', month, '-', '15'))) %>%
    mutate(tid = year * 100 + month) %>% 
    mutate(sid = as.numeric(x = station)) %>% 
    filter(year >= 1971)

# Create NA dataframe
na_pdata <- raw_data[is.na(raw_data$et0.p),]
na_pdata <- na_pdata[(!is.na(na_pdata$tm) & !is.na(na_pdata$ws)),] %>% 
    pdata.frame(index = c('sid','tid'))

# Create Panel Data
pdata <- raw_data[!is.na(raw_data$et0.p),] %>% 
    pdata.frame(index = c('sid','tid'))

# Split Data into Test and Train Set
sample = sample.split(Y = pdata, SplitRatio = 0.8)
pdata_train = subset(x = pdata, subset = sample == TRUE)
pdata_test  = subset(x = pdata, subset = sample == FALSE)

# Panel Data Models -----------------------------------------------------------------------------------------------
# Create Model with train data
model <- plm::plm(formula = et0.p ~ tm + ws,
                  data = pdata_train,
                  effect = 'twoways',
                  model = 'within')

model_ols <- plm::plm(formula = et0.p ~ tm + ws,
                      data = pdata_train,
                      model = 'pooling')

d_train_ols <- augment(x = model_ols, data = pdata_train)

d_train <- augment(x = model, data = pdata_train)

d_train <- d_train %>% 
    mutate(fixef = model$coefficients['tm'] * tm + model$coefficients['ws'] * ws +
               plm::fixef(object = model, effect = 'time')[as.character(tid)] + 
               plm::fixef(object = model, effect = 'individual')[as.character(sid)])

d_train$dif <- d_train$.fitted - d_train$fixef

# Numerical Goodness-of-fit measures: Train Data
result <- data.frame(para = rownames(hydroGOF::gof(sim = rnorm(n = 10, mean = 0, sd = 1),
                                                   obs = rnorm(n = 10, mean = 1, sd = 2))))
result[['P-H_train']] <- hydroGOF::gof(as.numeric(d_train$et0.p),
                                       as.numeric(d_train$et0.h))
result[['panel_data_train']] <- hydroGOF::gof(as.numeric(d_train$et0.p),
                                              as.numeric(d_train$.fitted))
result[['ols_train']] <- hydroGOF::gof(as.numeric(d_train_ols$et0.p),
                                       as.numeric(d_train_ols$.fitted))

# Taylor Diagram for Model Evaluation
mydata = data.frame('PenmanMonteithETo' = d_train$et0.p,
                    'HargreavesSamaniETo' = d_train$et0.h,
                    'PanelDataETo' = d_train$.fitted,
                    'OLSETo' = d_train_ols$.fitted) %>% 
    reshape2::melt(id = 'PenmanMonteithETo')

mydata$variable <- c(rep(x = 'Hargreaves Samani', nrow(mydata)/3), rep(x = 'Panel Data', nrow(mydata)/3), rep(x = 'OLS', nrow(mydata)/3))

openair::TaylorDiagram(mydata = mydata,
                       obs = "PenmanMonteithETo",
                       mod = "value",
                       group = "variable",
                       cols = c('red', 'green','black'),
                       annotate = 'RMSE',
                       key.title = '',
                       key.pos = 'top')

# Test Panel Data Model
model_coef <- coef(object = lm(formula = et0.p ~ tm + ws + sid + tid - 1,
                               data = pdata_train))

model_coef_ols <- coef(object = lm(formula = et0.p ~ tm + ws,
                               data = pdata_train))

pdata_test <- pdata_test %>% 
    mutate(et0.p_predict = model_coef['tm'] * tm + model_coef['ws'] * ws +
               model_coef[paste0('tid', as.character(tid))] + 
               model_coef[paste0('sid', as.character(sid))]) %>% 
    mutate(et0.p_ols = model_coef_ols['tm'] * tm + model_coef_ols['ws'] * ws +
               model_coef_ols['(Intercept)'])
    

# Numerical Goodness-of-fit measures: Test Data
result[['P-H_test']] <- hydroGOF::gof(as.numeric(pdata_test$et0.p),
                                      as.numeric(pdata_test$et0.h))
result[['panel_data_test']] <- hydroGOF::gof(as.numeric(pdata_test$et0.p),
                                             as.numeric(pdata_test$et0.p_predict))
result[['ols_test']] <- hydroGOF::gof(as.numeric(pdata_test$et0.p),
                                      as.numeric(pdata_test$et0.p_ols))

write.csv(x = result, file = "Articles/A01/result/GOF.csv")

# Taylor Diagram for Model Test
mydata = data.frame('PenmanMonteithETo' = pdata_test$et0.p,
                    'HargreavesSamaniETo' = pdata_test$et0.h,
                    'PanelDataETo' = pdata_test$et0.p_predict,
                    'OLSETo' = pdata_test$et0.p_ols) %>% 
    reshape2::melt(id = 'PenmanMonteithETo')

mydata$variable <- c(rep(x = 'Hargreaves Samani', nrow(mydata)/3), rep(x = 'Panel Data', nrow(mydata)/3), rep(x = 'OLS', nrow(mydata)/3))

openair::TaylorDiagram(mydata = mydata,
                       obs = "PenmanMonteithETo",
                       mod = "value",
                       group = "variable",
                       cols = c('red', 'green','black'),
                       annotate = 'RMSE',
                       key.title = '',
                       key.pos = 'top')

# Plot Hargreaves-Samani ETo vs Predict ETo
theme_set(new = theme_bw())
ggplot(data = pdata_test) +
    geom_point(aes(x = et0.p, y = et0.h, color = "Hargreaves-Samani"), size = 1) +
    geom_point(aes(x = et0.p, y = et0.p_predict, color = "Panel Data"), size = 1) +
    geom_point(aes(x = et0.p, y = et0.p_ols, color = "OLS"), size = 1) +
    scale_colour_manual(name = '', values = c('Hargreaves-Samani' = 'red', 'Panel Data' = 'black', 'OLS' = 'green')) +
    theme(text = element_text(size = 11), 
          aspect.ratio = 1,
          legend.justification = c(0,0),
          legend.position = c(0.05,0.8),
          legend.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab('Penman–Monteith ETo (mm/month)') +
    ylab('Hargreaves-Samani | Predict | OLS ETo (mm/month)') +
    coord_equal(xlim=c(0,400),ylim=c(0,400)) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.5)


# Predict NA Data -------------------------------------------------------------------------------------------------

# Create Model
model_coef_na <- coef(object = lm(formula = et0.p ~ tm + ws + sid + tid - 1,
                                  data = pdata))

# Predict NA Value Data
na_pdata <- na_pdata %>% 
    mutate(et0_predict = model_coef_na['tm'] * tm + model_coef_na['ws'] * ws +
               model_coef_na[paste0('tid', as.character(tid))] + 
               model_coef_na[paste0('sid', as.character(sid))])

# Plot Predict Data
c <- full_join(x = raw_data %>%
                   mutate(stid = sid * 1000000 + tid),
               y = na_pdata %>%
                   mutate(stid = as.numeric(as.character(sid)) * 1000000 + as.numeric(as.character(tid))) %>% 
                   select(c(et0_predict, stid)),
               by = 'stid')

ggplot(data = c, aes(x = date)) + 
    geom_line(aes(y = et0.p)) + 
    geom_line(data = c, aes(x = date, y = et0_predict), color = 'red') +
    facet_wrap(facets = ~station, labeller = labeller(station = c("dargaz" = "Dargaz",
                                                                  "golmakan" = "Golmakan",
                                                                  "kashmar" = "Kashmar",
                                                                  "mashhad" = "Mashhad",
                                                                  "neyshabur" = "Neyshabur",
                                                                  "sabzevar" = "Sabzevar",
                                                                  "sarakhs" = "Sarakhs",
                                                                  "torbate_heydarieh" = "Torbate Heydarieh",
                                                                  "torbate_jam" = "Torbate Jam"))) +
    theme(text = element_text(size = 22),
          axis.title.x = element_blank()) +
    ylab('ETo (mm/month)')
