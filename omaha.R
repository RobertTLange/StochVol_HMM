# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# 125005 Forecasting competition 2017
#
# (Authors) Davide Viviano, Hans-Peter Höllwirth, Robert Lange 
# (Date)    02.2017

predictor.omaha <- function(y){

    T <- nrow(y)

    y.in <- y[2:T,1]
    X.in <- y[1:(T-1),1]
    X.out<- y[T,1]

    mdl <- lm( y.in ~ X.in )
    beta <- coef(mdl)
  
    f <- beta[1] + beta[2]*X.out[1]

	return( f )
}
