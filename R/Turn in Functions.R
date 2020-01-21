#' Vector Mean
#'
#' @param x a vector object
#'
#' @return The meean of the vector object passed as a parameter
#' @export
#'
#' @examples The mean of a sample vector [1,1,1,1] = 1. The mean of a vector [1,2,3,4,5] = 3
#'
#'
VectorMean = function(x){

  #returns the mean of the parameter
  return(mean(x))

}


#'Covariance matrix function
#'
#' @param matrix matrix of vectors, with the number of vectors represented by columns, and the amount of data represented as rows
#'
#' @return a matrix of the covariances of all the vectors
#' @export
#'
#' @examples
CovarianceMatrix = function(matrix){

  #Number of data sets
  dataSets =  ncol(matrix)

  #Number of data points in each vector
  numDataPoints = nrow(matrix)

  for (i in 1:dataSets) {

    for (j in i+1:dataSets) {

      covarVector = append(Covariance(matrix[,i],matrix[,j]))


    }

  }

  #Covariance matrix that will be populated with values and returned
  covarMatrix = matrix(data = covarvector,nrow=dataSets,ncol=dataSets,TRUE)


}


