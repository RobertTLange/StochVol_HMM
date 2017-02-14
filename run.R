# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# Stochastic Volatility Modeling 
# with Hidden Markov Models
#
# (Authors) Davide Viviano | Robert Lange | Hans-Peter Höllwirth
# (Date)    02.2017

# ----------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------
# house cleaning
rm(list=ls())
save.plots <- FALSE
set.seed(1234, kind = NULL, normal.kind = NULL)

# load libraries
library(fanplot)

# loading depending sources
source("multiplot_ts.R")
source("sv_sim.R")       # simulates the returns and volatility time series
source("sir.R")          # computes the sequential importance resampling (prediction + filtering)
source("csir.R")
source("sv_fit.R")
source("sv_loglik.R")
dyn.load("csir.so")

# ----------------------------------------------------------------------
# Construct and plot simulated TS
# ----------------------------------------------------------------------
T <- 1000; const <- 0.05; phi <- 0.98; tau2 <- 0.02
theta <- c(const, phi, tau2)

# simulate time series
sim_df <- sv_sim(theta, T)
y <- sim_df$y
alpha <- sim_df$alpha

# plot time series with 95% confidence interval
if(save.plots) pdf("../images/ts_returns.pdf")
plot(y, type="l")
lines( 1.96 * exp(alpha/2), col= "red")
lines(-1.96 * exp(alpha/2), col= "red")
if(save.plots) dev.off()

# plot time series volatility
if(save.plots) pdf("../images/ts_volatility.pdf")
plot(sqrt(252) * exp(alpha/2), type='l')
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# Setup particle filtering
# ----------------------------------------------------------------------
P <- 200 # set number of particles
alpha_up <- rnorm(P,0,0.1)
alpha_pr <- rep(0,P)
alpha_wt <- rep(1,P)/P

alpha_up_mat <- matrix(rep(0, T*3),T)
alpha_pr_mat <- matrix(rep(0, T*3),T)
alpha_pr_are <- matrix(rep(0, T*20),T)

# generate a particle set of P random draws from an approximation
# of the prediction and filtering distribution for every time series point
for (t in 1:T){
    # prediction step
    alpha_pr <- const + phi * alpha_up  + rnorm(P,0,sqrt(tau2))
    # update/filtering step (normal density)
    alpha_wt <- dnorm(y[t]*rep(1,P), mean=0 , sd = exp(alpha_pr/2))
    alpha_up <- sir(alpha_pr=alpha_pr,alpha_wt=alpha_wt, u=sort(runif(P,0,1)))

    alpha_up_mat[t,2] <- mean(alpha_up)
    alpha_up_mat[t,1] <- quantile(alpha_up,0.05)
    alpha_up_mat[t,3] <- quantile(alpha_up,0.95)

    alpha_pr_mat[t,2] <- mean(alpha_pr)
    alpha_pr_mat[t,1] <- quantile(alpha_pr,0.05)
    alpha_pr_mat[t,3] <- quantile( alpha_pr,0.95)
    alpha_pr_are[t,] <- quantile(alpha_pr, seq(0,1,1/(ncol(alpha_pr_are)-1)))
}

# plot prediction density
if(save.plots) pdf("../images/prediction_density.pdf")
plot(sqrt(252) * exp(alpha/2), type='l')
lines(sqrt(252) * exp(alpha_up_mat[,1]/2), col='blue')
lines(sqrt(252) * exp(alpha_up_mat[,2]/2), col='blue')
lines(sqrt(252) * exp(alpha_up_mat[,3]/2), col='blue')
if(save.plots) dev.off()

# plot filtering density
if(save.plots) pdf("../images/filtering_density.pdf")
plot(sqrt(252) * exp(alpha/2), type='l')
lines(sqrt(252) * exp(alpha_pr_mat[,1]/2), col='blue')
lines(sqrt(252) * exp(alpha_pr_mat[,2]/2), col='blue')
lines(sqrt(252) * exp(alpha_pr_mat[,3]/2), col='blue')
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# Plotting heat maps
# ----------------------------------------------------------------------
heat <- matrix(rep(1,T*20), T, 20)
for (i in 1:20) {
    heat[,i] <- sqrt(252)*exp(alpha_pr_are[,21-i]/2) 
}
jet.colors <- colorRampPalette(c("red", "#FF7F00", "yellow","#7FFF7F", "cyan", "#007FFF", "blue"), 
                               bias=1, space="rgb", interpolate="spline")

if(save.plots) pdf("../images/sv-sim-predicting-dist.pdf")
plot(NULL, xlim = c(1, T), ylim = c(0, 160))
fan(data = t(heat), fan.col = jet.colors)
if(save.plots) dev.off()

if(save.plots) pdf("../images/sv-sim-predicting-dist-act.pdf")
plot(NULL, xlim = c(1, T), ylim = c(0, 160))
fan(data = t(heat), fan.col = jet.colors)
lines( sqrt(252)*exp(alpha/2) ,  col="black")
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# Estimate time series parameters
# ----------------------------------------------------------------------
values <- sv_fit(y, theta, P, 1)
