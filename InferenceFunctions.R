###################################################
### Function: Prior Plot Values
###################################################
Prior <- function(m, n, a=n*m, b=n*(1-m)){
	dom <- seq(0,1,0.005)
	val <- dbeta(dom,a,b)
	return(data.frame('x'=dom, 'y'=val))
}

###################################################
### Function: Likelihood Plot Values
###################################################
Likelihood <- function(N, Y){
	a <- Y + 1
	b <- N - Y + 1
	dom <- seq(0,1,0.005)
	val <- dbeta(dom,a,b)
	return(data.frame('x'=dom, 'y'=val))
}

###################################################
### Function: Posterior Plot Values
###################################################
Posterior <- function(m,n,N,Y,a_in=n*m, b_in=n*(1-m)){
	a_out <- Y + a_in
	b_out <- N - Y + b_in
	dom <- seq(0,1,0.005)
	val <- dbeta(dom,a_out,b_out)
	return(data.frame('x'=dom, 'y'=val))
}

###################################################
### Function: Mean of Posterior Beta
###################################################
MeanOfPosterior <- function(m,n,N,Y,a=n*m, b=n*(1-m)){
	a_out <- Y + a - 1
	b_out <- N - Y + b - 1
	E_posterior <- a_out / (a_out + b_out)
	return(E_posterior)
}


###################################################
### Function: Mode of Posterior Beta
###################################################
ModeOfPosterior <- function(m,n,N,Y,a=n*m, b=n*(1-m)){
	a_out <- Y + a - 1
	b_out <- N - Y + b - 1
	mode_posterior <- (a_out - 1)/(a_out + b_out - 2)
	return(mode_posterior)
}


###################################################
### Function: Standard Deviation of Posterior Beta
###################################################
StdDevOfPosterior <- function(m,n,N,Y,a=n*m, b=n*(1-m)){
	a_out <- Y + a - 1
	b_out <- N - Y + b - 1
	sigma_posterior <- sqrt((a_out*b_out)/(((a_out+b_out)^2)*(a_out+b_out+1)))
	return(sigma_posterior)
}

###################################################
### Function: Get List of All Model Outputs
###################################################
getModelValues <- function(m,n,N_samp,Y_samp,a_in=(n*m),b_in=n*(1-m)){
	a <- Y_samp + a_in - 1
	b <- N_samp - Y_samp + b_in - 1
	pr <- Prior(m,n,a=a_in,b=b_in)
	lk <- Likelihood(N_samp,Y_samp)
	po <- Posterior(m,n,N_samp,Y_samp,a=a,b=b_in)
	mean_po <- MeanOfPosterior(m,n,N_samp,Y_samp,a=a_in,b=b_in)
	mode_po <- ModeOfPosterior(m,n,N_samp,Y_samp,a=a_in,b=b_in)
	sd_po <- StdDevOfPosterior(m,n,N_samp,Y_samp,a=a_in,b=b_in)

	inf_df <- data.frame("mean_posterior"=mean_po,
				"mode_posterior"=mode_po,
				"stdev_posterior"=sd_po,
				"alpha_posterior"=a,
				"beta_posterior"=b)

	plot_df <- data.frame('Dist'=c(rep('Prior',nrow(pr)), 
							rep('Likelihood',nrow(lk)), 
							rep('Posterior',nrow(po))),
							rbind(pr,lk,po))
	with(plot_df, Dist <- factor(Dist, levels = c('Prior', 'Likelihood',
		'Posterior'), ordered = TRUE))

	return(list("InfDf"=inf_df,"PlotDf"=plot_df))
}

####################################################################
### Function: Plot model with list from getModelValues function
####################################################################
plotModel <- function(model_list, plot_title, w=5, h=3, s=1.2, save_plot=F, file_name=NA){
	mean_po <- model_list[["InfDf"]][1,'mean_posterior']
	mode_po <- model_list[["InfDf"]][1,'mode_posterior']
	sd_po <- model_list[["InfDf"]][1,'stdev_posterior']
	a_po <- model_list[["InfDf"]][1,'alpha_posterior']
	b_po <- model_list[["InfDf"]][1,'beta_posterior']
	
	plot_title <- paste(plot_title,': ',expression(alpha),'=',a_po,', ',expression(beta), '=', b_po, sep='')
	
	p <- ggplot(data=model_list[["PlotDf"]],aes(x=x, y=y, 
	color=Dist, linetype=Dist))
	p <- p + geom_vline(xint=mean_po,color='darkblue',linetype=3)
	p <- p + geom_vline(xint=(-1.96*sd_po)+mean_po,color='gray',linetype=3)
	p <- p + geom_vline(xint=(1.96*sd_po)+mean_po,color='gray',linetype=3)
	p <- p + geom_line()
	p <- p + theme_bw()
	p <- p + labs(title=plot_title)
	p <- p + ylab(expression(paste('p(',theta,')', sep = '')))
	p <- p + xlab(expression(theta))
	p <- p + opts(legend.position='bottom')
	if(save_plot==T){
		ggsave(paste(file_name,'.png',sep=''),  width = w, height = h, scale = s)
	}
	return(p)
}
