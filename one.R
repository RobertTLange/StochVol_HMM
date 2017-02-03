
predictor.one <- function(data){

	T <- nrow(data)

  y.in <- data[2:T,1]
  X.in <- data[1:(T-1),1]
  X.out<- data[T,1]

  mdl <- lm( y.in ~ X.in )
  beta <- coef(mdl)
  
  f <- beta[1] + beta[2]*X.out[1]

	return( f )
}
