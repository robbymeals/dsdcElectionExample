library(ggplot2)
library(reshape2)
library(lubridate)
library(stringr)
library(xtable)
library(plyr)
library(scales)
library(ggmap)
library(gridExtra)

# General Bayesian Binom Prop Inference Functions
source('InferenceFunctions.R') 
# Functions specific to Polling Data Example
source('PollingExampleFunctions.R') 

#### 2008 Election ####
# 2008 Parameters
elecDay <- '2008-11-04'
cutOff <- '2008-08-01'
#cutOff <- NA
elecPassed = T
elecType <- 'President'
candidates <- list('d'='Obama','r'='McCain')
d_m = 0.5
r_m = 0.5
init_n = 2

# Run source code using above parameters
source('PollingExampleSource.R')

print(xtable(myPreds), type='html')
print(xtable(rcpPreds), type='html')

print(xtable(outcomes), type='html')
print(xtable(rmse), type='html')

## Plot updating models
w = 5
h = 3
s = 1.2
IN_01 <- plotModel(d_models[['IN']][['models_list']][[1]],
	"Initial Obama Model")
IN_10 <- plotModel(d_models[['IN']][['models_list']][[10]],
	"Obama Model #10")
IN_100 <- plotModel(d_models[['IN']][['models_list']][[22]],
	"Final Obama Model")

## Trend plots
p <- postTrendPlot('IN',candidates)
p

p <- postTrendPlot('NC',candidates)
p

p <- postTrendPlot('MO',candidates)
p

## Map plots
p <- mapPlotPolling(outcomes, "Actual Outcomes")
p

p <- mapPlotPolling(myPreds, "My Predictions")
p

p <- mapPlotPolling(rcpPreds, "RCP Predictions")
p
