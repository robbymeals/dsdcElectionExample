library(ggplot2)
library(reshape2)
library(lubridate)
library(stringr)
library(xtable)
library(plyr)
library(ggmap)
library(scales)

# General Bayesian Binom Prop Inference Functions
source('InferenceFunctions.R') 
# Functions specific to Polling Data Example
source('PollingExampleFunctions.R') 

#### 2012 Election ####
## 2012 Parameters
elecDay <- '2012-11-06'
cutOff <- '2012-07-01'
elecPassed = F
elecType <- 'President'
candidates <- list('d'='Obama','r'='Romney')
d_m = 0.5
r_m = 0.5
init_n = 2

# Run source code using above parameters
source('PollingExampleSource.R')

print(xtable(myPreds), type='html')
print(xtable(rcpPreds), type='html')

## Plot updating models
w = 5
h = 3
s = 1.2
## Trend plots
p <- postTrendPlot('NC',candidates)
ggsave('NCtrendPlot12.png', width=w, height=h, scale=s)

p <- postTrendPlot('NH',candidates)
ggsave('NHtrendPlot12.png', width=w, height=h, scale=s)

p <- postTrendPlot('MO',candidates)
ggsave('MOtrendPlot12.png', width=w, height=h, scale=s)

## Map plots
p <- mapPlotPolling(myPreds, "My Predictions")
ggsave('MyPredMap2012.png', width=w, height=h, scale=s)

p <- mapPlotPolling(rcpPreds, "RCP Predictions")
ggsave('RcpPredMap2012.png', width=w, height=h, scale=s)
