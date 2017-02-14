# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# Stochastic Volatility Modeling 
# with Hidden Markov Models
#
#  --> Define model log likelihood
#
# (Authors) Davide Viviano | Robert Lange | Hans-Peter Höllwirth
# (Date)    02.2017
dyn.load("csir.so")

sv_loglik <- function(theta, y, eta_sim, u_sim, alpha_up, alpha_wt) {
    
    T <- length(y)
    P <- length(alpha_up)
    const <- theta[1]
    phi <- theta[2]
    tau2 <- theta[3]
    
    alpha_up_pr <- matrix(0, nrow=T, ncol=4)
    
    loglik <- 0 

    for (t in 1:T) {
        alpha_pr <- const + phi*alpha_up + sqrt(tau2)*eta_sim[,t]
        lik <- dnorm( y[t]*rep(1,P) , rep(0,P) , exp(alpha_pr/2))
        log_mean_lik <- tryCatch(log(mean(lik)), error=function(e)(-Inf))
        #print(log_mean_lik)
        
        #if (is.na( log_mean_lik )) {
        if (log_mean_lik == -Inf) {
            print(paste('problem at ',as.character(t),as.character(theta)))
            loglik <- Inf
            alpha_up_pr <- NA
            break
        } else {
            loglik <- loglik - log_mean_lik
            # update
            alpha_wt <- lik
            alpha_up <- csir.c(alpha_pr, alpha_wt, u_sim[,t])
    
            # quantiles
            alpha_up_pr[t,1] <- mean( alpha_up )
            alpha_up_pr[t,2] <- mean( alpha_pr )
            alpha_up_pr[t,c(3,4)] <- quantile( alpha_pr ,c(0.05,0.95))
        } 
    }
  
    loglik <- loglik/T
    return(list(loglik = loglik, alpha_up_pr = alpha_up_pr))
}
 
