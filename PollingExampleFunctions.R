##################################################################
## Function: Preprocess Dataframe, Polling Example
##################################################################
ppDf_polling <- function(inDf, candidates, elecDay, elecPassed, cutOff=NA){
	inDf <- subset(inDf,!(Poll %in% c('Final Results','RCP Average')))
	colnames(inDf) <- sub(paste(candidates[['d']],'..D.',sep=''),'D',colnames(inDf))
	colnames(inDf) <- sub(paste(candidates[['r']],'..R.',sep=''),'R',colnames(inDf))	
	## Process sample size and type
	Samples <- t(as.data.frame(strsplit(inDf[,'Sample'], split=' ')))
	SampleSize <- as.numeric(Samples[,1])
	SampleType <- Samples[,2]

	## Process polling period
	Per <- t(as.data.frame(strsplit(inDf[,'Date'],split=' - ')))
	PerBeg <- as.Date(paste(Per[, 1], '/', elecYear, sep=''), format = '%m/%d/%Y')
	PerEnd <- as.Date(paste(Per[, 2], '/', elecYear, sep=''), format = '%m/%d/%Y')
	if(elecPassed==T){
	if(any(PerBeg > as.Date(elecDay))){
		PerBeg[min(which(PerBeg > as.Date(elecDay))):length(PerBeg)] <- PerBeg[min(which(PerBeg > as.Date(elecDay))):length(PerBeg)] - years(1)
		PerEnd[min(which(PerEnd > as.Date(elecDay))):length(PerEnd)] <- PerEnd[min(which(PerEnd > as.Date(elecDay))):length(PerEnd)] - years(1)
	}}
	if(elecPassed==F){
	if(any(PerBeg > as.Date(Sys.Date()))){
		PerBeg[min(which(PerBeg > as.Date(Sys.Date()))):length(PerBeg)] <- PerBeg[min(which(PerBeg > as.Date(Sys.Date()))):length(PerBeg)] - years(1)
		PerEnd[min(which(PerEnd > as.Date(Sys.Date()))):length(PerEnd)] <- PerEnd[min(which(PerEnd > as.Date(Sys.Date()))):length(PerEnd)] - years(1)
	}}
	PerLength <- PerEnd - PerBeg + 1
	PerFromEl <- as.Date(elecDay) - PerEnd

	## Output Proportions
	Y_r <- with(inDf, round((R/100) * SampleSize))
	Y_d <- with(inDf, round((D/100) * SampleSize))
	N <- Y_r + Y_d

	## Output df, only polls after cutoff
	outDf <- data.frame(inDf[,c('Poll', 'R', 'D', 'Spread', 'MoE')],
				Y_r, Y_d, N,
				SampleSize, SampleType,
				PerBeg, PerEnd, PerLength, PerFromEl)
	if (!is.na(cutOff)){
		outDf <- subset(outDf, (PerBeg >= as.Date(cutOff)))
	}
	outDf <- outDf[!is.na(outDf$SampleSize),]
	outDf <- outDf[order(outDf$PerBeg),]
	## Return final processed df
	return(outDf)
}

##################################################################
## Function: Run Updating Models, Polling Example
## Defaults: 	Weak Uniform Prior: init_m=0.5, init_n=2
## 				Alpha and Beta unweighted, change value of wtVars to add weights
##################################################################
runUpdatingModels <- function(	yVar, DF, init_m=0.5, init_n=2, 
								pollWts = data.frame('alpha'=rep(1,nrow(DF)),'beta'=rep(1,nrow(DF)))){
	init_N <- DF[1,'SampleSize']
	init_Y <- DF[1,yVar]
	model_lists <- list()
	model_lists[[1]] <- getModelValues(init_m, init_n, init_N, init_Y)
	models <- as.data.frame(matrix(nrow=nrow(DF), ncol=ncol(model_lists[[1]][['InfDf']])))
	colnames(models) <- colnames(model_lists[[1]][['InfDf']])
	models[1,] <- model_lists[[1]][['InfDf']][1,]
	for (i in seq(from=2, to=nrow(models))){
		models[(i-1),'alpha_prior'] <- 	models[(i-1),'alpha_posterior'] * pollWts[i-1,'alpha']
		models[(i-1),'beta_prior'] <- 	models[(i-1),'beta_posterior'] * pollWts[i-1, 'beta']
		N <- DF[i,'SampleSize']
		Y <- DF[i,yVar]
		a <- models[(i-1),'alpha_prior']
		b <- models[(i-1),'beta_prior']
		model_lists[[i]] <- getModelValues(0,0,N,Y,a,b)
		models[i,] <- model_lists[[i]][['InfDf']][1,]
	}
	return(list('models_df'=models,'models_list'=model_lists))
}


## Posterior Trend Plot
postTrendPlot <- function(state, candidates){
	plot_cols <- c('blue','red')
	plot_cols <- plot_cols[order(c(candidates[['d']],candidates[['r']]))]
	plot_df <- rbind(
		data.frame(
			d_models[[state]][['models_df']], 
			"PollDate" = as.Date(poll_dfs[[state]]$PerEnd), 
			"Candidate"=rep(candidates[['d']],nrow(poll_dfs[[state]]))
		),
		data.frame(
			r_models[[state]][['models_df']], 
			"PollDate" = as.Date(poll_dfs[[state]]$PerEnd), 
			"Candidate"=rep(candidates[['r']],nrow(poll_dfs[[state]]))
		)
	)
	
	plot_df$SEp1 <- plot_df$mean_posterior + (1*plot_df$stdev_posterior)
	plot_df$SEm1 <- plot_df$mean_posterior - (1*plot_df$stdev_posterior)
	plot_df$SEp2 <- plot_df$mean_posterior + (2*plot_df$stdev_posterior)
	plot_df$SEm2 <- plot_df$mean_posterior - (2*plot_df$stdev_posterior)
	p <- ggplot(data=plot_df, aes(x=as.Date(PollDate), y=mean_posterior)) 
	p <- p + theme_bw()
	p <- p + geom_ribbon(aes(ymax=SEp1,ymin=SEm1, fill=Candidate), alpha=.1)
	p <- p + geom_ribbon(aes(ymax=SEp2,ymin=SEm2, fill=Candidate), alpha=.1)
	p <- p + geom_point(aes(color=Candidate))
	p <- p + geom_smooth(se=F,aes(color=Candidate))
	p <- p + scale_fill_manual(values=plot_cols)
	p <- p + scale_color_manual(values=plot_cols)
	p <- p + ggtitle(paste(state,' Posterior Trend'))
	p <- p + xlab('Poll Date')
	p <- p + ylab('Mean of Posterior')
	p <- p + scale_y_continuous(labels=percent)
	return(p)
}

## Map plots
mapPlotPolling <- function(preds_df,plot_title){
	states <- map_data("state")
	states[,'State'] <- paste(state.abb[match(states$region, tolower(state.name))],sep='')
	preds_df[,'State'] <- rownames(preds_df)
	states <- merge(preds_df,states, by="State", all=T)
	gradBreaks <- c(max(preds_df$Outcome), 0.0001, 0, -0.0001, min(preds_df$Outcome))
	p <- ggplot(data=states, aes(x=long, y=lat, group=group, order=order, fill=Outcome, label=Outcome))
	p <- p + geom_polygon(color='black') + coord_map(proj='azequidistant')
	p <- p + scale_fill_gradient2(breaks=gradBreaks, high='blue4', low='red4', labels=round(gradBreaks,1), na.value='black',guide=F)
	p <- p + theme_nothing()
	p <- p + ylab('') + xlab('')
	p <- p + theme(axis.line=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())
	p <- p + ggtitle(plot_title)
	return(p)
}
