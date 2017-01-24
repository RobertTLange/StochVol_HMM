sv_sim <- function(theta, T){

    const = theta[1]
    phi = theta[2]
    tau2 = theta[3]

    alpha = rep(0,T)
    y = rep(0,T)

    eta = rnorm(T, 0, sqrt(tau2))
    z = rnorm(T, 0, 1)
    nu = runif(T, 0, 1)

    alpha[1] = const

    y[1] = z[1] * exp(alpha[1]/2)
    for (t in 2:T){
        alpha[t] = const + phi*(alpha[t-1]) + eta[t]
        y[t] = z[t]*exp(alpha[t]/2)
    }

    retList = list("alpha" = alpha, "y" = y)
    retList
}


#theta = c(0.05, 0.98, 0.02)
#sv_sim(theta, 100)
