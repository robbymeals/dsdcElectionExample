################################################################
### Functions for exploring binomial datasets with
### different values of theta
################################################################
getThetaDf <- function(theta,r.x=F, N=1000){
	A = round(theta*N)
	B = N - A
	Z <- c(rep(1,A),rep(0,B))
	thetaDf <- 
	if(r.x==T){
	thetaDf <- data.frame('Z'=sample(Z), 
		'x'=sample(seq(length(Z))),
		'y'=sample(seq(length(Z))))
	}
	if(r.x==F){	
	thetaDf <- data.frame('Z'=Z, 
		'x'=c(sample(seq(from=1, to=A)), sample(seq(from=(A+1), to=N))),
		'y'=sample(seq(length(Z))))
	}
	return(thetaDf)
}

plotThetaDf <- function(thetaDf, filename, save.png=T){
	p <- ggplot(thetaDf, aes(x=x,y=y,color=factor(Z),
	shape=factor(Z)))
	p <- p + theme_bw()
	p <- p + geom_point(size=6) 
	p <- p + scale_shape_manual(values=c('B','A'),guide='none')
	p <- p + scale_color_manual(values=plotCols,guide='none')
	p <- p + opts(axis.text.y = theme_blank())
	p <- p + opts(axis.text.x = theme_blank())
	p <- p + opts(axis.ticks = theme_blank())
	p <- p + opts(panel.border = theme_blank())
	p <- p + opts(panel.background = theme_rect(fill = "transparent",colour = NA), # or theme_blank()
    panel.grid.minor = theme_blank(), 
    panel.grid.major = theme_blank(),
    plot.background = theme_rect(fill = "transparent",colour = NA))
	p <- p + xlab('') + ylab('')
	p <- p + opts(legend.position = 'bottom', legend.title=theme_blank())
	p <- p + opts(panel.grid.major = theme_blank(), 
	panel.grid.minor = theme_blank())
	p <- p + opts(plot.margin = unit(c(1,1,-1,-1),"lines"))
	if(save.png==T){
		ggsave(paste(filename,'.png',sep=''), width = 11, height = 7, scale = 1, bg='transparent')
	}
	return(p)
}

thetaSamplePlot <- function(theta, file_name, save.png=T){
	theta_df <- getThetaDf(theta)
	theta_plot <- plotThetaDf(theta_df,file_name, save.png=save.png)
	return(theta_plot)
}

###################################################
### Plot betas with different values of alpha 
### and beta to explore prior representations
###################################################
betaplot <- function(a,b){
	theta = seq(0,1,0.005)
	p_theta = dbeta(theta, a, b)
	p <- qplot(theta, p_theta, geom='line')
	p <- p + theme_bw()
	p <- p + ylab(expression(paste('p(',theta,')', sep = '')))
	p <- p + xlab(expression(theta))
	return(p)
}

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
plotModel <- function(model_list, plot_title, w=8, h=5, s=1, save_plot=F, file_name=NA){
	mean_po <- model_list[["InfDf"]][1,'mean_posterior']
	mode_po <- model_list[["InfDf"]][1,'mode_posterior']
	sd_po <- model_list[["InfDf"]][1,'stdev_posterior']
	a_po <- model_list[["InfDf"]][1,'alpha_posterior']
	b_po <- model_list[["InfDf"]][1,'beta_posterior']
	ss_df <- data.frame(' '=c('Mean of Posterior','Mode of Posterior','Std Dev of Posterior'), ' '=round(c(mean_po,mode_po,sd_po),3))

	yMax <- dbeta(mean_po,a_po,b_po)
	yMax <- yMax - 0.05*yMax
	yMin <- yMax - 0.2*yMax
	x1 <- ifelse(any(mean_po>.6,mean_po<.4),1-mean_po,0.7)
	x2 <- ifelse(x1>.5,.95,0.05)
	xMax <- max(c(x1,x2))
	xMin <- min(c(x1,x2))
	
	summaryStats <- tableGrob(ss_df,show.rownames=F, show.colnames=F, show.vlines=T,
	gpar.coltext=gpar(col='black',cex=0.5),gpar.corefill = gpar(fill = "white", col = "gray95"))
	
	plot_title <- paste(plot_title,', ',expression(alpha),'=',a_po,', ',expression(beta), '=', b_po, sep='')
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
	p <- p + annotation_custom(grob=summaryStats, xmax=xMax, ymin=yMin, ymax=yMax)
	if(save_plot==T){
		ggsave(paste(file_name,'.png',sep=''),  width = w, height = h, scale = s)
	}
	return(p)
}
