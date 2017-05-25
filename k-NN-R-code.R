## Adam Chaffee
## Opendoor take-home assignment
## May 13, 2017

## Load the data
setwd("C:/Users/achaf/OneDrive/Documents/Work Related/Take home tests/Opendoor 5-5-17/")
houses = read.csv(file = "data.csv", header = T)

##########################################
##########################################
## Section 1: k-NN function and Error
##########################################
##########################################

#############################################################################
## First I create a function that sorts the home data from earliest to latest close date.
## This function will be used inside the k-NN function to ensure that we will
## only consider previously sold homes as valid neighbors
#############################################################################
sort_by_date = function(data = houses, descending = TRUE)
{
  ## Sort houses by date - first convert dates to strings. R initially reads the 
  ## data as factor class, which does not work for sorting
  data$close_date = as.character(data$close_date)
  
  ## First create an index that contains the order of the house closing dates
  time_sort_index = sort(data$close_date, index.return = TRUE)$ix
  
  ## Use this index to rearrange the data by time from earliest sale to latest
  data_time_sorted = data[time_sort_index,]
  return(data_time_sorted)
}

## I first sort my data by date before proceeding
houses = sort_by_date(houses)


#############################################################################
## I then create a function that computes all the distances between an observation
## and any amount of comparison data, provided that they have latidudes and
## longitudes
#############################################################################
compute_distances = function(observation_data, comparison_data)
{
  lat_dist = (observation_data$latitude - comparison_data$latitude)
  lon_dist = (observation_data$longitude - comparison_data$longitude)
  distances = sqrt(lat_dist^2 + lon_dist^2)
  return(distances)
}
###########################################################################
## k-NN function. The parameters it will take in are:
##    (1) the value of k
##    (2) the method: either mean ("equal"), or inverse weight by distance ("inverse distance")
##    (3) sorted data frame of houses that must contain:
##      (a) columns named "latitude" and "longitude" for location
##      (b) The closing date named "close_date"
##      (c) The closing price named "close_price"
## The function also sorts the home data by close date. It is not necessary
## for the data to be pre-processed in chronological order
############################################################################
k_nearest = function(k = 4,  weight_method = "equal", data = houses)
{
  ## Find the total number of data points so that we know the number of iterations
  ## to use in the upcoming for() loop
  num_observations = length(data_time_sorted[,1])
  
  ## Initialize an estimate vector to be filled with estimated home price. Note that the
  ## earliest k observations cannot be calculated. I left the price estimate as 0
  ## for those first few homes.
  k_NN_estimates = c(rep(0,times = num_observations))
  
  ## I also want to save the mean k_NN distances for analysis
  k_NN_distances = matrix(rep(0, times = k*num_observations), ncol = k)
  
  ## Utilize a for loop. We start at k+1, because that is the first index where
  ## we have enough observations to run k-NN
  for(i in (k+1):num_observations)
  {
    ## First create a subset of only previously sold homes from the target home. 
    ## Doing this will reduce computing time as it will be easier to sort this
    ## subset by distance instead of sorting the entire data frame
    valid_neighbors = data_time_sorted[1:(i-1),]
    
    ## I decide to reverse the order of the valid neighbors. This will
    ## re-order from most recent sale to least recent
    valid_neighbors = rev(valid_neighbors)
    
    ## Calculate distance from the target home to all other valid homes
    distances = compute_distances(observation_data = data_time_sorted[i,], 
                                  comparison_data = valid_neighbors)
    
    ## Find the indices of the nearest k neighbors (lowest k distances)
    ## Uses a partial sort to find the lowest k distances
    kth_lowest_distance = sort(distances, partial = k)[k]
    k_lowest_indices = which(distances <= kth_lowest_distance)
    
    ## Occasionally, re-sales will result in duplicate locations that
    ## give us more than k values. This line only takes the most recent k sales
    k_lowest_indices = k_lowest_indices[1:k]
    
    ## Store an estimate for the lowest prices
    k_nearest_prices = valid_neighbors$close_price[k_lowest_indices]
    
    ## Also save the corresponding distances. The small constant prevents
    ## a weight of infinity when a home is resold and has distance of 0
    k_nearest_distances = distances[k_lowest_indices]+0.00001
    
    ## Determine what weighting method to use, and calculate weights
    if(weight_method == "inverse distance"){
      k_inv_distances = 1/k_nearest_distances
      weights = k_inv_distances/sum(k_inv_distances)
    } else if(weight_method == "equal") weights = 1/k
    
    ## Save the k_NN estimate of the prices in the ith element of estimate vector
    k_NN_estimates[i] = sum(weights*k_nearest_prices)
    k_NN_distances[i,] = k_nearest_distances
  }
  ## Returns a vector of estimates by k-NN
  return(list(k_NN_estimates, k_NN_distances))
}
##############################################################################
## Run the function to obtain 4-NN home prices. Took about 40 minutes for me.
##############################################################################
k_NN_inverse_weights = k_nearest(k = 4, weight_method = "inverse distance", data = houses)
k_NN_equal_weights = k_nearest(k = 4, weight_method = "equal", data = houses)

#############################################################################
## Visualizations and analysis - work in progress
#############################################################################
inverse_weight_err = abs(k_NN_inverse_weights[[1]] - houses$close_price)
distances = rowMeans(k_NN_inverse_weights[[2]])
histogram(inverse_weight_err, nint = 10000, xlim = c(-100000,600000))
histogram(inverse_weight_err/houses$close_price, xlim = c(0,2), nint = 1000000)
plot(inverse_weight_err ~ distances)

length(inverse_weight_err)
length(rowMeans(k_NN_inverse_weights[[2]]))

mean(abs(k_NN_inverse_weights[[1]] - houses$close_price))
(k_NN_inverse_weights[[1]])[1:10]
head(k_NN_inverse_weights[[1]])
head(houses$close_price)
k_NN_inverse_weights[[1]]

## Function to calculate mean relative absolute error
MRAE = function(actual, estimated)
{
  return(median(abs(estimated - actual)/actual))
}
## MRAE for this assignment
MRAE(houses_time_sorted$close_price, estimates)
################################################
################################################
## Section 2: Additional analysis for questions
################################################
################################################
## load package stringr to clean up the date field
library(stringr)

sale_date_time = matrix(unlist(strsplit(houses_time_sorted$close_date, split = " ")))
head(sale_date_time)
length(sale_date_time)

## Define a new date field
sale_date = sale_date_time[((1:num_houses)*2)-1,1]
head(sale_date)

## Convert into R's date format
houses$sale_date = as.Date(sale_date)

## Calculate error and plot over time
err = (estimates - houses_time_sorted$close_price)
plot(houses$sale_date, err, main = "Home price error over time",
     ylab = "error", xlab = "time")

## Calculate absolute error and plot with point sizes scaled to error
abs_err = abs(estimates - houses_time_sorted$close_price)
plot(houses_time_sorted$longitude, houses_time_sorted$latitude, cex = abs_err/mean(abs_err))

