sv_sim <- function(theta, T){
    #Simulate log volatility modelled as AR(1)
    #Input I : intercept/constant/bias
    #Input II: Autocorrelation coefficient phi
    #Input III: Normal error with tau2 variance
    #Output I: Returns y
    #Output II: Volatility alpha
    const = theta[1]
    phi = theta[2]
    tau2 = theta[3]
##Stupid change
    alpha = rep(0,T)              #placeholder volatility
    y = rep(0,T)                  #placeholder returns
    eta = rnorm(T, 0, sqrt(tau2)) #error of AR(1) volatility model
    z = rnorm(T, 0, 1)            #multiplicative term return model
    alpha[1] = const              #no autocorrelated observation in starting period

    y[1] = z[1] * exp(alpha[1]/2)
    for (t in 2:T){
        alpha[t] = const + phi*(alpha[t-1]) + eta[t]
        y[t] = z[t]*exp(alpha[t]/2)
    }

    retDF = data.frame(alpha, y)
    retDF
}


#theta = c(0.05, 0.98, 0.02)
#sv_sim(theta, 100)
