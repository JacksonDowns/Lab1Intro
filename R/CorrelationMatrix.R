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

  variableNames = colnames(matrix)

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
  return(matrix(correlationVecVAlues,dataSets,dataSets,TRUE,list(variableNames,variableNames)))

}
