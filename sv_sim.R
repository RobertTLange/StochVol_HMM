# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# Stochastic Volatility Modeling 
# with Hidden Markov Models
#
#  --> Simulate log volatility modelled as AR(1)
#
# (Authors) Davide Viviano | Robert Lange | Hans-Peter Höllwirth
# (Date)    02.2017

#Input 1: theta - parameter vector (bias, AR coefficient, error variance) 
#Input 2: T     - size of time series
#Output:  retDF - simulated returns (y) and volatility (alpha)
sv_sim <- function(theta, T) {

    const <- theta[1]              # intercept/constant/bias
    phi <- theta[2]                # autocorrelation coefficient phi
    tau2 <- theta[3]               # normal error with tau2 variance
    alpha <- rep(0,T)              # placeholder volatility
    y <- rep(0,T)                  # placeholder returns
    eta <- rnorm(T, 0, sqrt(tau2)) # error of AR(1) volatility model
    z <- rnorm(T, 0, 1)            # multiplicative term return model
    alpha[1] <- const              # no autocorrelated observation in starting period

    y[1] <- z[1] * exp(alpha[1]/2)
    for (t in 2:T) {
        alpha[t] <- const + phi*(alpha[t-1]) + eta[t]
        y[t] <- z[t]*exp(alpha[t]/2)
    }

    retDF = data.frame(alpha, y)
    return(retDF)
}

# test run
# theta = c(0.05, 0.98, 0.02)
# sv_sim(theta, 100)
