plot_marginal <-
function(parameter,percent.burnin=0,thinning=1,histogram=TRUE,density=TRUE,population.names=NULL,param.name=deparse(substitute(parameter))){
		burnin <- (percent.burnin/100)*length(which(parameter!=0))
		x <- seq(from = burnin,to = length(which(parameter!=0)),by = thinning)
			marginal <- density(parameter[x],adj=1)
			if(is.null(population.names)){
				plot(1,type="n",
					ylim=c(0,max(marginal$y)+max(marginal$y)/5),
					xlim=c(min(marginal$x),max(marginal$x)),
					main=paste("Marginal density of",param.name,sep=" "),
					ylab="density",
					xlab=param.name
                )
			}
			if(!is.null(population.names)){
				plot(1,type="n",
					ylim=c(0,max(marginal$y)+max(marginal$y)/5),
					xlim=c(min(marginal$x),max(marginal$x)),
					main=paste("Marginal density of F parameter,",population.names,sep=" "),
					ylab="density",
					xlab=param.name
                )
			}			
			if(histogram){
				graphics::hist(parameter[x],
					freq=FALSE,
					col="gray",
					add=TRUE)
			}
			if(density){
				graphics::lines(marginal,adj=1)
				graphics::polygon(x=c(0,marginal$x,0),y=c(0,marginal$y,0),col=grDevices::adjustcolor("blue",0.6))					
			}
				graphics::segments(x0=stats::median(parameter[x]),
					y0=0,
					x1=stats::median(parameter[x]),
					y1=max(marginal$y+marginal$y/20),
					col="red",
					lwd=3)		
	}
