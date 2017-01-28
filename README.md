# Stochastic Volatility Modelling using Hidden Markov Model

Project carried out for Financial Econometrics class tought by Prof. Christian Brownlees.
We estimate a common stochastic volatility model which models the volatility process as a latent state.
We infer the dynamics in the time series by utilizing a Hidden Markov Model (HMM) which allows identification by Sequential Monte Carlo.

The following repository contains these scripts:

* run.R - Main pipeline which executes all of the following scripts

* sv_sim.R - Simulates a time series that follows a specified stochastics volatility model

* sir.R - Simulation-based filter (sequential importance sampling)

* csir.R - Simulation-based filter (continuous sequential importance sampling) which is needed for the likelihood estimation


To be continued ....
