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


#'Covariance Matrix Function
#'
#' @param matrix matrix of vectors, with the number of vectors represented by columns, and the amount of data represented as rows
#'
#' @return a matrix of the covariances of all the vectors
#' @export
#'
#' @examples a matrix 4x3 matrix will return a 4x4 zero matrix
CovarianceMatrix = function(matrix){

  #Number of data sets
  dataSets =  ncol(matrix)

  #Number of data points in each vector
  numDataPoints = nrow(matrix)

  #Vector of the Covariance values
  covarianceVector = vector()

  ##For loops to calculate the covariances of the data sets
  for (i in 1:dataSets) {

    for(j in 1:dataSets){

      covarianceVector = c(covarianceVector,Covariance(matrix[,i],matrix[,j]))

    }

  }

  #Returns the covariance values as a matrix
  return(matrix(covarianceVector,dataSets,dataSets,TRUE))

}


#' Correlation Matrix Function
#'
#' @param matrix matrix of values used to calculate correlation, where columns represent datasets and rows represent n datapoints for each dataset.
#'
#' @return a matrix of correlation values for the data
#' @export
#'
#' @examples
CorrelationMatrix = function(matrix){

  #Number of data sets
  dataSets =  ncol(matrix)

  #Number of data points in each vector
  numDataPoints = nrow(matrix)

  #Vector of the Correlation values
  correlationVecVAlues = vector()


  for (i in 1:dataSets) {

    for(j in 1:dataSets)
    {

      correlationVecVAlues = c(correlationVecVAlues,Correlation(matrix[,i],matrix[,j]))

    }



  }

  #Returns the covariance values as a matrix
  return(matrix(correlationVecVAlues,dataSets,dataSets,TRUE))


}
