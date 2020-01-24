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

  variableNames = colnames(matrix)

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
  return(matrix(covarianceVector,dataSets,dataSets,TRUE,list(variableNames,variableNames)))

}
