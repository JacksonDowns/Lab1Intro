
#' Covariance
#'
#' @param vectorx 1st vector parameter
#' @param vectory 2nd vector parameter
#'
#' @return returns the covariance of the two vectors
#' @export
#'
#' @examples
Covariance = function(vectorx, vectory){

  sumOfDifferences = 0

  xbar = VectorMean(vectorx)
  ybar = VectorMean(vectory)
  n = length(vectorx)

  for(i in 1:n) {

    sumOfDifferences = sumOfDifferences + ((vectorx[i] - xbar) * (vectory[i] - ybar))

  }

  covar = sumOfDifferences / (n-1)

  return(covar)

}









