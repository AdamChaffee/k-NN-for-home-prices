# k-NN-for-home-prices

The R code file implements a k-NN housing price model where the nearest neighbors are identified as the closest Euclidean (l2 norm) distance based on the Latitude and Longitude variables provided. The function uses the model for which k = 4 as a default and assumes the data is already sorted by increasing date order. The function also prevents "time leakage," which means that nearest neighbors only considers homes that have sold prior to the home whose price is being estimated.

The algorithm was done in R and only requires the base R packages that come with typical installation. Full analysis and visualization of the example data is currently in progress.

