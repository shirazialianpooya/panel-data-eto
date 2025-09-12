
rm(list = ls())

library(ExPanDaR)
library(naniar)

data <- readRDS(file = 'Articles/A03/data/data_monthly_final.rds') %>% 
    mutate(year = year(date),
           month = month(date)) %>% 
    select(station, year, month, tn, tx, tm, rh, pr, ws, sh, et0.p)

colnames(data) <- c('Station', 'Year', 'Month', 'Minimum Temperature', 'Maximum Temperature', 'Mean Temperature', 
                    'Relative Humidity', 'Precipitation', 'Wind Speed', 'Sunshine',
                    'Penman–Monteith ETo')

ExPanDaR::prepare_correlation_graph(df = data[,4:length(data)])
PerformanceAnalytics::chart.Correlation(R = data[,4:length(data)], histogram = TRUE, pch = 19)
GGally::ggpairs(data[,4:length(data)])

# Visualize Missing Values in R:
# facet Stations
naniar::gg_miss_var(x = data[,c(1, 4:length(data))],
                    show_pct = TRUE,
                    facet = Station) + 
    theme_bw() + 
    theme(text = element_text(size = 14),
          axis.title.y = element_blank())

# All Stations
naniar::gg_miss_var(x = data[,c(1, 4:length(data))],
                    show_pct = TRUE) + 
    theme_bw() + 
    theme(text = element_text(size = 20),
          axis.title.y = element_blank())

# method two
library(ggplot2)
theme_set(theme_bw())

num_na <- colSums(is.na(data[,c(4:length(data))]))
num_na <- data.frame(para = names(num_na), na = num_na / nrow(data))
rownames(num_na) <- NULL

num_na$na <- num_na$na * 100

num_na$para = factor(num_na$para,levels(num_na$para)[c(2,4,3,6,7,8,9,5,1)])

ggplot(data = num_na, aes(x = para, y = na)) + 
    geom_point(size = 3) + 
    geom_segment(aes(x = para, 
                     xend = para, 
                     y = 0, 
                     yend = na)) + 
    theme(axis.text.x = element_text(angle=90, vjust=0.6),
          text = element_text(size = 14),
          axis.title.x = element_blank()) +
    ylab('% Missing Data')


levels(num_na$para)








naniar::gg_miss_upset(data[,c(7:length(data))])

