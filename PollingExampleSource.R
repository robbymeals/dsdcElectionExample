##### Called In Driver
#library(ggplot2)
#library(reshape2)
#library(lubridate)
#library(stringr)
#library(xtable)
#library(plyr)
#library(scales)
#library(ggmap)
##library(gridExtra)

## General Bayesian Binom Prop Inference Functions
#source('InferenceFunctions.R') 
## Functions specific to Polling Data Example
#source('PollingExampleFunctions.R') 

##### 2008 Election ####
## 2008 Parameters
#elecDay <- '2008-11-04'
#cutOff <- '2008-08-01'
##cutOff <- NA
#elecPassed = T
#elecType <- 'President'
#candidates <- list('d'='Obama','r'='McCain')
#d_m = 0.5
#r_m = 0.5
#init_n = 2
#### Called In Driver

## read in datasets
elecYear <- year(as.Date(elecDay))
dataDir <- paste('./Datasets/',elecType,'_',elecYear,'/', sep='')
dSets <- list.files(dataDir)
states <- sub('.txt','',dSets)
raw_dfs <- list()
for (state in states){
	raw_dfs[[state]] <- read.table(paste(dataDir,state,'.txt',sep=''),sep='\t', stringsAsFactors=F,header=T)
}

## Proprocess data frames
poll_dfs <- list()
for(state in names(raw_dfs)){
	poll_dfs[[state]] <- ppDf_polling(raw_dfs[[state]], candidates, elecDay, elecPassed, cutOff=cutOff)
}

## default is same priors for each state
## if you want to specify priors for each state, just write to df
priors_d = matrix(nrow=length(states),ncol=2)
priors_r = matrix(nrow=length(states),ncol=2)
rownames(priors_d) <- states
rownames(priors_r) <- states
colnames(priors_d) <- c('m','n')
colnames(priors_r) <- c('m','n')
for (state in states){
	priors_d[state,] <- c(d_m,init_n)
	priors_r[state,] <- c(d_m,init_n)
}

########################################################################
################### Simple Dual Binomial/Beta Models ################### 
########################################################################

## Initial Beta Prior parameters
d_prior <- qplot(data=Prior(d_m,init_n),x=x,y=y,geom='line')
r_prior <- qplot(data=Prior(r_m,init_n),x=x,y=y,geom='line')

## Run updating models
d_models = list()
r_models = list()
for(state in names(poll_dfs)){
	d_models[[state]] <- runUpdatingModels('Y_d',poll_dfs[[state]],init_m=priors_d[state,'m'], init_n=priors_d[state,'n'])
	r_models[[state]] <- runUpdatingModels('Y_r',poll_dfs[[state]],init_m=priors_r[state,'m'], init_n=priors_r[state,'n'])
}

## Compare Predictions to real outcomes
preds = list()
for(state in names(poll_dfs)){
	preds[[state]] <- c(
				r_models[[state]][["models_df"]][nrow(r_models[[state]][["models_df"]]),'mean_posterior'],
				d_models[[state]][["models_df"]][nrow(d_models[[state]][["models_df"]]),'mean_posterior'])
}

## Get Real Clear Politics Predictions
rcpPreds <- ldply(raw_dfs, function(x) return(x[with(x,Poll=='RCP Average'),5:6]))
colnames(rcpPreds) <- sub(paste(candidates[['d']],'..D.',sep=''),'D',colnames(rcpPreds))
colnames(rcpPreds) <- sub(paste(candidates[['r']],'..R.',sep=''),'R',colnames(rcpPreds))
rownames(rcpPreds) <- rcpPreds[,1]
rcpPreds <- rcpPreds[,c('D','R')]
colnames(rcpPreds) <- c(candidates[['d']],candidates[['r']])
rcpPreds[,'Outcome'] <- rcpPreds[,candidates[['d']]] - rcpPreds[,candidates[['r']]]
rcpPreds$Winner <- apply(rcpPreds[,c(candidates[['d']],candidates[['r']])],1,function(x) return(names(which.max(x))))

## Get my predictions
myPreds <- ldply(preds)
myPreds <- myPreds[,2:3]
myPreds <- data.frame(myPreds)
rownames(myPreds) <- names(preds)
colnames(myPreds) <- c(candidates[['r']],candidates[['d']])
myPreds <- round((myPreds*100),1)
myPreds <- myPreds[,c(candidates[['d']],candidates[['r']])]
myPreds[,'Outcome'] <- myPreds[,candidates[['d']]] - myPreds[,candidates[['r']]]
myPreds$Winner <- apply(myPreds[,c(candidates[['d']],candidates[['r']])],1,function(x) return(names(which.max(x))))

## if Election is passed
if(elecPassed == T){
	## get actual outcomes
	outcomes <- ldply(raw_dfs, function(x) return(x[with(x,Poll=='Final Results'),5:6]))
	colnames(outcomes) <- sub(paste(candidates[['d']],'..D.',sep=''),'D',colnames(outcomes))
	colnames(outcomes) <- sub(paste(candidates[['r']],'..R.',sep=''),'R',colnames(outcomes))
	rownames(outcomes) <- outcomes[,1]
	outcomes <- outcomes[,c('D','R')]
	colnames(outcomes) <- c(candidates[['d']],candidates[['r']])
	outcomes[,'Outcome'] <- outcomes[,candidates[['d']]] - outcomes[,candidates[['r']]]	
	outcomes$Winner <- apply(outcomes[,c(candidates[['d']],candidates[['r']])],1,function(x) return(names(which.max(x))))

	## Compare forecasts to outcomes
	abs(rcpPreds[,'Outcome'] - outcomes[,'Outcome']) -> rcpPreds[,'fcstError']
	abs(myPreds[,'Outcome'] - outcomes[,'Outcome']) -> myPreds[,'fcstError']
	rcpCorr <- sum(rcpPreds$Winner == outcomes$Winner)
	myCorr <- sum(myPreds$Winner == outcomes$Winner)

	## abuse of rmse
	closeStates <- rownames(subset(outcomes, abs(Outcome) < 2))
	myPreds_rmse <- sqrt(mean(myPreds[,'fcstError']^2))
	rcpPreds_rmse <- sqrt(mean(rcpPreds[,'fcstError']^2))
	myPreds_rmse_close <- sqrt(mean(myPreds[closeStates,'fcstError']^2))
	rcpPreds_rmse_close <- sqrt(mean(rcpPreds[closeStates,'fcstError']^2))
	rmse <- rbind('My Predictions'=c(myPreds_rmse, myPreds_rmse_close, myCorr),
		'RealClearPolitics Predictions'=c(rcpPreds_rmse, rcpPreds_rmse_close, rcpCorr))
	colnames(rmse) <- c('RMSE','RMSE, Diff < 2%','Number Correctly Predicted')
}
