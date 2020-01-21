
#' Covariance
#'
#' @param vectorx 1st vector parameter
#' @param vectory 2nd vector parameter
#'
#' @return returns the covariance of the two vectors
#'
#'
#'
Covariance = function(vectorx, vectory){

  #top part of the covariance function
  sumOfDifferences = 0

  #Mean value of the first vector parameter
  xbar = VectorMean(vectorx)

  #Mean values of the second vector parameter
  ybar = VectorMean(vectory)

  #length of the two vectors
  n = length(vectorx)

  #Sums the differences of the vector values - their respective means and multiplies the results
  for(i in 1:n) {

    sumOfDifferences = sumOfDifferences + ((vectorx[i] - xbar) * (vectory[i] - ybar))

  }

  #Calculates the covariance
  covar = sumOfDifferences / (n-1)

  #Returns the covariance of the vectors
  return(covar)

}



#' Correlation
#'
#' @param vectorx 1st vector parameter
#' @param vectory 2nd vector parameter
#'
#' @return returns the correlation of the two vectors
#'
#'
#'
Correlation = function(vectorx, vectory){

  numOfDataPoints = length(vectorx)

  devScoreSquared

  for (i in 1:numOfDataPoints) {



  }



}
