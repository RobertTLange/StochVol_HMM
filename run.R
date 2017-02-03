rm(list=ls())
save.plots = FALSE
set.seed(1234, kind = NULL, normal.kind = NULL)
################################################################################
source("multiplot_ts.R")
source("sv_sim.R")
source("sir.R")
source("csir.R")
source("sv_fit.R")
source("sv_loglik.R")
################################################################################

#General Function SetUp:
## 1. sv_sim.R - simulates the returns and volatility time series
## 2. sir.R - computes the sequential importance resampling (prediction + filtering)
## 3.

##################################################
#Construct and Plot simulated TS
##################################################
T = 1000; const = 0.05; phi = 0.98; tau2 = 0.02
theta = c(const, phi, tau2)

sim_df = sv_sim(theta, T)
y = sim_df$y
alpha = sim_df$alpha

if(save.plots) {pdf("../images/ts_returns.pdf")}
plot(y, type="l")
lines(1.96*exp(alpha/2), col= "red")
lines(-1.96*exp(alpha/2), col= "red")
if(save.plots) {dev.off()}

if(save.plots) {pdf("../images/ts_volatility.pdf")}
plot(sqrt(252)*exp(alpha/2), type='l')
if(save.plots) {dev.off()}

###################################################
#SetUp - Particle Filtering
###################################################
#t=1
P = 200 #set number of particle draws to 200
alpha_up = rnorm(P,0,0.1)
alpha_pr = rep(0,P)
alpha_wt = rep(1,P)/P

alpha_up_mat = matrix(rep(0, T*3),T)
alpha_pr_mat = matrix(rep(0, T*3),T)
#alpha_pr_are = matrix(rep(0, T*20),T)

###################################################
#For each t in 1,...,T generate a particle set of P random draws
#from an approx. of the prediction and filtering distribution
for (t in 1:T){
    #Prediction Step
    alpha_pr = const + phi * alpha_up  + rnorm(P,0,sqrt(tau2))
    #Update/Filtering step - Normal Density
    alpha_wt = dnorm(y[t]*rep(1,P), mean=0 , sd = exp(alpha_pr/2))
    alpha_up = sir(alpha_pr=alpha_pr,alpha_wt=alpha_wt, u=sort(runif(P,0,1)))

    alpha_up_mat[t,2] = mean(alpha_up)
    alpha_up_mat[t,1] = quantile(alpha_up,0.05)
    alpha_up_mat[t,3] = quantile(alpha_up,0.95)

    alpha_pr_mat[t,2] = mean(alpha_pr)
    alpha_pr_mat[t,1] = quantile(alpha_pr,0.05)
    alpha_pr_mat[t,3] = quantile( alpha_pr,0.95)
#   alpha_pr_are[t,] = quantile(alpha_pr, 0:(1/(size(alpha_pr_are,2)-1)):1 );
}

if(save.plots) {pdf("../images/prediction_density.pdf")}
plot(sqrt(252)*exp(alpha/2), type='l')
lines(sqrt(252)*exp(alpha_up_mat[,1]/2), col='blue')
lines(sqrt(252)*exp(alpha_up_mat[,2]/2), col='blue')
lines(sqrt(252)*exp(alpha_up_mat[,3]/2), col='blue')
if(save.plots) {dev.off()}

if(save.plots) {pdf("../images/filtering_density.pdf")}
plot(sqrt(252)*exp(alpha/2), type='l')
lines(sqrt(252)*exp(alpha_pr_mat[,1]/2), col='blue')
lines(sqrt(252)*exp(alpha_pr_mat[,2]/2), col='blue')
lines(sqrt(252)*exp(alpha_pr_mat[,3]/2), col='blue')
if(save.plots) {dev.off()}


####PLOTTING#### Heat Maps
# figure()
# hold
# js = jet(11);
# js(1,:) = [1 1 1];
# for i=1:20
#     area( sqrt(252)*exp(alpha_pr_are(:,21-i)/2) , 'FaceColor',js(11-abs((i)-10),:) ,'LineWidth',0.01 , 'EdgeColor',js(11-abs((i)-10),:));
#     11-abs((i)-10)
# end
# set(gca,'Layer','top')
# myprint('../images/sv-sim-predicting-dist.pdf')
#
# figure()
# hold
# js = jet(11);
# js(1,:) = [1 1 1];
# for i=1:20
#     area( sqrt(252)*exp(alpha_pr_are(:,21-i)/2) , 'FaceColor',js(11-abs((i)-10),:) ,'LineWidth',0.01 , 'EdgeColor',js(11-abs((i)-10),:));
#     11-abs((i)-10)
# end
# set(gca,'Layer','top')
# plot( sqrt(252)*exp(alpha/2) ,  'k')
# myprint('../images/sv-sim-predicting-dist-act.pdf')

values  = sv_fit(y,theta,P,1);
