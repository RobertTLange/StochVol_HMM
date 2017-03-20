multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

myplot <- function( dates , y , col='darkblue' , t='l' , lwd=2 , ylim=NULL , main=NULL ){
  if( is.null(main) ){ par( mar=c(2,2,0.1,0.1) ) }
  plot( dates , y , t=t , col=col , lwd=lwd , axes=F , xlab='' , ylab='' , xaxs="i" , ylim=ylim , main=main )
  xticks <- axis.Date(1, x=dates, at=seq(dates[1], dates[length(dates)], "year") , lwd=0, lwd.tick=1, tck=0.02)
  yticks <- axis(2 , lwd=0, lwd.tick=1, tck=0.02)
  axis.Date(3, x=dates, at=seq(dates[1], dates[length(dates)], "year"), lwd=0, lwd.tick=1, tck=0.02, lab=F)
  axis(4, lwd=0, lwd.tick=1, tck=0.02, lab=F)
  abline( h=yticks , lty=3 )
  abline( v=xticks , lty=3 )
  box()
}
