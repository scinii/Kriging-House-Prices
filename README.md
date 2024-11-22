# Median House Value Prediction using Kriging with External Drift

This project models the median house value of block groups in California using Kriging with External Drift . The goal is to predict house values by accounting for spatial dependencies and external covariates using various regression techniques. The models are applied across three areas: the entire state of California, the Bay Area, and the Redding area (northern California).

## Methods

### Kriging with External Drift
Kriging with external drift is used to model the residuals as a Gaussian process, with a mean of 0 and a covariance structure dependent on the spatial location. The external drift is calculated using three regression techniques:

- Ordinary Least Squares (OLS)
- Principal Component Regression (PCR)
- Lasso Regression

## Key Observations
- Modeling house values as a spatial phenomenon through Kriging results in better predictions than using traditional regression methods.
- A Bayesian approach (Bayesian Kriging) could further enhance performace, but it would require significant computational power, making it more suitable for smaller regions. 
