
PMARE <- function(obs = NA, sim = NA)
{
    100 * (sum((abs(obs - sim) / obs), na.rm = T) / length(obs))
}

RE <- function(obs = NA, sim = NA)
{
    100 * mean(abs((obs - sim) / sim), na.rm = T)
}

PD <- function(data = data,
               Y = Y,
               X = X,
               SampleSize = 0.25,
               seed = 123)
{
    # Random Number Generation
    set.seed(seed = seed)
    
    # Load Library
    library(plm)
    library(dplyr)
    library(broom)
    library(hydroGOF)
    
    # Remove Y NAs
    data = data[!is.na(data[[Y]]), ]
    
    # List of Result
    result_train <- list()
    result_test <- list()
    
    for (s in names(X))
    {
        # Define Formula
        f <- as.formula(paste0(Y, ' ~ ', X[[s]]))
        
        # Select Data
        if (s == 'year')
        {
            # Data Test Model
            data_test <- data %>% 
                group_by(station, season) %>%
                sample_frac(size = SampleSize)
            
            # Data Train Model
            data_train <- data %>%
                anti_join(y = data_test, by = c('sid', 'tid'))
            
        } else {
            # Data Test Model
            data_test <- data %>% 
                filter(season == s) %>% 
                group_by(station) %>%
                sample_frac(size = SampleSize)
            
            # Data Train Model
            data_train <- data %>%
                filter(season == s) %>% 
                anti_join(y = data_test, by = c('sid', 'tid'))
        }
        
        # START Train Model
        # Define Result
        if (Y == 'et0.p')
        {
            result <- data_train %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', Y, 'et0.h')
        } else {
            result <- data_train %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', Y)
        }
        
        if (Y == 'pr_a')
        {
            result <- data_train %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', 'occ_pr', Y)
        } else {
            result <- data_train %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', Y)
        }
        
        # START Panel Data
        # 1. Panel Data - Fixed Effect - Individual
        pd_fe_i <- plm::plm(formula = f,
                            data = data_train,
                            effect = 'individual',
                            model = 'within',
                            index = c('sid','tid'))
        result <- left_join(x = result,
                            y = augment(x = pd_fe_i, data = data_train) %>% 
                                mutate_(pd_fe_i = '.fitted') %>% 
                                select_('sid', 'tid', 'pd_fe_i'),
                            by = c('sid', 'tid'))
        
        # 2. Panel Data - Fixed Effect - Twoways
        pd_fe_tw <- plm::plm(formula = f,
                             data = data_train,
                             effect = 'twoways',
                             model = 'within',
                             index = c('sid','tid'))
        result <- left_join(x = result,
                            y = augment(x = pd_fe_tw, data = data_train) %>% 
                                mutate_(pd_fe_tw = '.fitted') %>% 
                                select_('sid', 'tid', 'pd_fe_tw'),
                            by = c('sid', 'tid'))
        
        # 3. Panel Data - Random Effect - Individual
        pd_re_i <- plm::plm(formula = f,
                            data = data_train,
                            effect = 'individual',
                            model = 'random',
                            index = c('sid','tid'))
        result <- left_join(x = result,
                            y = augment(x = pd_re_i, data = data_train) %>% 
                                mutate_(pd_re_i = '.fitted') %>% 
                                select_('sid', 'tid', 'pd_re_i'),
                            by = c('sid', 'tid'))
        
        # 4. Panel Data - Random Effect - Twoways
        pd_re_tw <- plm::plm(formula = f,
                             data = data_train,
                             effect = 'twoways',
                             model = 'random',
                             index = c('sid','tid'))
        result <- left_join(x = result,
                            y = augment(x = pd_re_tw, data = data_train) %>% 
                                mutate_(pd_re_tw = '.fitted') %>% 
                                select_('sid', 'tid', 'pd_re_tw'),
                            by = c('sid', 'tid'))
        
        # 5. OLS
        ols <- lm(formula = f,
                  data = data_train)
        
        result <- left_join(x = result,
                            y = augment(x = ols, data = data_train) %>% 
                                mutate_(ols = '.fitted') %>% 
                                select_('sid', 'tid', 'ols'),
                            by = c('sid', 'tid'))
        
        # 6. Save Result
        result_train[[s]] <- result
        # END Panel Data
        # END Train Model
        
        # START Test Model
        # Define Result
        if (Y == 'et0.p') 
        {
            result <- data_test %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', Y, 'et0.h')
        } else {
            result <- data_test %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', Y)
        }
        
        if (Y == 'pr_a') 
        {
            result <- data_test %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', 'occ_pr', Y)
        } else {
            result <- data_test %>%
                select_('station', 'date', 'season', 'month', 'sid', 'tid', Y)
        }
        
        # 1. Panel Data - Fixed Effect - Individual
        pd_fe_i_lm <- lm(formula = as.formula(paste0(Y, ' ~ ', X, ' + sid - 1')),
                         data = data_train)
        
        pd_fe_i_lm$xlevels[["tid"]] <- union(pd_fe_i_lm$xlevels[["tid"]], levels(data_test$tid))
        pd_fe_i_lm$xlevels[["sid"]] <- union(pd_fe_i_lm$xlevels[["sid"]], levels(data_test$sid))
        
        result <- left_join(x = result,
                            y = augment(pd_fe_i_lm, data_train, data_test) %>% 
                                mutate_(pd_fe_i = '.fitted') %>% 
                                select_('sid', 'tid', 'pd_fe_i'),
                            by = c('sid', 'tid'))
        
        # 2. Panel Data - Fixed Effect - Twoways
        pd_fe_tw_lm <- lm(formula = as.formula(paste0(Y, ' ~ ', X, ' + sid + tid - 1')),
                          data = data_train)
        
        pd_fe_tw_lm$xlevels[["tid"]] <- union(pd_fe_tw_lm$xlevels[["tid"]], levels(data_test$tid))
        pd_fe_tw_lm$xlevels[["sid"]] <- union(pd_fe_tw_lm$xlevels[["sid"]], levels(data_test$sid))
        
        result <- left_join(x = result,
                            y = augment(pd_fe_tw_lm, data_train, data_test) %>% 
                                mutate_(pd_fe_tw = '.fitted') %>% 
                                select_('sid', 'tid', 'pd_fe_tw'),
                            by = c('sid', 'tid'))
        
        # 3. OLS
        result <- left_join(x = result,
                            y = augment(ols, data_train, data_test) %>% 
                                mutate_(ols = '.fitted') %>% 
                                select_('sid', 'tid', 'ols'),
                            by = c('sid', 'tid'))
        
        # 4. Save Result
        result_test[[s]] <- result
    }
    
    return(list(Train = result_train,
                Test = result_test,
                PDTest = list(pd_fe_i = pd_fe_i,
                              pd_fe_tw = pd_fe_tw,
                              pd_re_i = pd_re_i,
                              pd_re_tw = pd_re_tw,
                              ols = ols)))
}


# # 1. Panel Data - Fixed Effect - Individual
# predict.pd.fe.i <- function(model, newData) 
# {
#     Y <- all.vars(model$formula)[1]
#     
#     X <- all.vars(model$formula)[-1]
#     
#     X.coeff <- data.frame(predictor = names(model$coefficients),
#                           coeff = model$coefficients)
#     
#     S.coeff <- data.frame(station = as.factor(names(fixef(object = model))),
#                           station.intercept = fixef(object = model))
#     
#     data <- newData %>% select(one_of(c('station', 'tid', 'season', Y, X)))
#     
#     for (c in X)
#     {
#         data[c] <- data[c] * X.coeff[X.coeff$predictor == c, 2]
#     }
#     
#     data <- data %>% 
#         left_join(y = S.coeff,
#                   by = 'station')
#     
#     result <- as.data.frame(data) %>% 
#         mutate(pd_fe_i = rowSums(.[5:ncol(data)])) %>% 
#         select('station', 'tid', Y, 'pd_fe_i')
#     
#     return(result)
# }


# # 2. Panel Data - Fixed Effect - Twoways
# predict.pd.fe.tw <- function(model, newData) 
# {
#     Y <- all.vars(model$formula)[1]
#     
#     X <- all.vars(model$formula)[-1]
#     
#     X.coeff <- data.frame(predictor = names(model$coefficients),
#                           coeff = model$coefficients)
#     
#     S.coeff <- data.frame(station = as.factor(names(fixef(object = model))),
#                           station.intercept = fixef(object = model))
# 
#     T.coeff <- data.frame(tid = as.factor(names(fixef(object = model, effect = 'time'))),
#                           time.intercept = fixef(object = model, effect = 'time'))
#     
#     data <- newData %>% select(one_of(c('station', 'tid', 'season', Y, X)))
#     data$tid <- as.factor(data$tid)
#     
#     for (c in X)
#     {
#         data[c] <- data[c] * X.coeff[X.coeff$predictor == c, 2]
#     }
#     
#     data <- data %>% 
#         # left_join(y = S.coeff,
#         #           by = 'station') %>%
#         left_join(y = T.coeff,
#                   by = 'tid') %>% 
#         mutate(st = (station.intercept + time.intercept) / 2) %>% 
#         select(-station.intercept, -time.intercept)
#     
#     result <- data %>% 
#         mutate(pd_fe_tw = rowSums(.[5:ncol(data)])) %>% 
#         select('station', 'tid', Y, 'pd_fe_tw')
#     
#     return(result)
# }
# 
# gof(sim = result$pd_fe_tw, obs = result$sh, norm = 'maxmin')
# 
# gof(sim = result_train$year$pd_fe_i, obs = result_train$year$sh, norm = 'maxmin')
# 
# 
# result_train$year$pd_fe_tw -  result$pd_fe_tw
# 
# 
# a <- result$pd_fe_tw
# b <- result$pd_fe_tw
# c = (a + b) /2


PD.Predict <- function(data = data,
                       Y = Y,
                       X = X,
                       newData = newData,
                       station = 'sid',
                       date = 'tid')
{
    
    # Load Library
    library(plm)
    library(dplyr)
    library(broom)
    library(hydroGOF)
    
    # Remove Y NAs
    data = data[!is.na(data[[Y]]), ]
    
    # Define Formula
    f <- as.formula(paste0(Y, ' ~ ', X))
    
    # Panel Data - Fixed Effect - Twoways
    model <- lm(formula = as.formula(paste0(Y, ' ~ ', X, ' + ', ' sid + tid - 1')),
                data = data)
    
    model$xlevels[["tid"]] <- union(model$xlevels[["tid"]], levels(newData$tid))
    
    result <- augment(model, data, newData) %>% 
        mutate_(predict = '.fitted') %>% 
        select_('station', 'date', 'sid', 'tid', Y, 'predict')
    
    return(result)
}

OLS.Predict <- function(data = data,
                        Y = Y,
                        X = X,
                        newData = newData,
                        station = 'sid',
                        date = 'tid')
{
    
    # Load Library
    library(plm)
    library(dplyr)
    library(broom)
    library(hydroGOF)
    
    # Remove Y NAs
    data = data[!is.na(data[[Y]]), ]
    
    # Define Formula
    f <- as.formula(paste0(Y, ' ~ ', X))
    
    # Panel Data - Fixed Effect - Twoways
    model <- lm(formula = as.formula(paste0(Y, ' ~ ', X)),
                data = data)
    
    model$xlevels[["tid"]] <- union(model$xlevels[["tid"]], levels(newData$tid))
    
    result <- augment(model, data, newData) %>% 
        mutate_(predict = '.fitted') %>% 
        select_('station', 'date', 'sid', 'tid', Y, 'predict')
    
    return(result)
}
