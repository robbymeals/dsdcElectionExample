source('PollingExampleDriver2008.R')
state = 'IN'
plot_cols <- c('blue','red')
plot_cols <- plot_cols[order(c(candidates[['d']],candidates[['r']]))]	
first <- 1
last <- nrow(poll_dfs[[state]])
middle <- ceiling(mean(c(first,last)))
polls_callout <- c(first,middle,last)
polls_all <- seq(last)
pp_df <- c()
for(i in polls_all){
	df_d <- d_models[['IN']][['models_list']][[i]][['PlotDf']]
	df_r <- r_models[['IN']][['models_list']][[i]][['PlotDf']]
	outDf_d <- data.frame(
	'Candidate' = rep(candidates[['d']], nrow(df_d)),
	'Model' = rep(i,nrow(df_d)),
	df_d
	)
	outDf_r <- data.frame(
	'Candidate' = rep(candidates[['r']], nrow(df_r)),
	'Model' = rep(i,nrow(df_r)),
	df_r
	)
	outDf <- rbind(outDf_d, outDf_r)
	pp_df <- rbind(pp_df, outDf)
}

pp_df <- subset(pp_df,Dist=='Posterior')
xLims <- c(min(pp_df[round(pp_df$y,2)>0,'x']),
max(pp_df[round(pp_df$y,2)>0,'x']))
pp_df[,'Model_Group'] <- paste(pp_df[,'Model'],pp_df[,'Candidate'])
pp_df_co <- subset(pp_df, Model%in%polls_callout)
pp_df[,'Model'] <- factor(pp_df[,'Model'])
pp_df_co[,'Model'] <- factor(pp_df_co[,'Model'])

p <- ggplot(data=pp_df, aes(x=x, y=y, group=Model_Group,order=rev(Model))) 
p <- p + theme_bw()
p <- p + scale_fill_manual(values=plot_cols)
p <- p + geom_area(aes(fill=Candidate, alpha=Model),color='white',size=.1) 
p <- p + facet_grid(Candidate ~ .)
p <- p + xlim(xLims)
p <- p + geom_line(data=pp_df_co)

p <- ggplot(data=data.frame(x=c(0,1)),aes(x))
alphas <- d_models[['IN']][['models_df']][,'alpha_posterior']
betas <- d_models[['IN']][['models_df']][,'beta_posterior']
for(i in seq(nrow(d_models[['IN']][['models_df']]))){
	print(i)
	print(c(alphas[i],betas[i]))
	p <- p + stat_function(fun=function(x) dbeta(x,alphas[i],betas[i]), geom='area', fill='blue', alpha=i/length(betas))
}

p <- ggplot(data=data.frame(x=c(0,1)),aes(x))
p <- p + stat_function(fun=function(x) dbeta(x,alphas[1],betas[1]), geom='area', fill='blue', alpha=i/length(betas))
p <- p + stat_function(fun=function(x) dbeta(x,alphas[13],betas[13]), geom='area', fill='blue', alpha=i/length(betas))
p <- p + stat_function(fun=function(x) dbeta(x,alphas[25],betas[25]), geom='area', fill='blue', alpha=i/length(betas))

