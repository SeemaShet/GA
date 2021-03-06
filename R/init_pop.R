#' Initialize population
#'
#' Initialize random population
#' 
#' @param N vector; Size of population (models to be tested)
#' @param X matrix/dataframe; containing predictor information
#' @return returns a matrix of initial candidate population
#' @export
init_pop <- function(N,X){
  X=as.data.frame(X)
  pop <- matrix(rnorm(N*length(X)), nrow=N, ncol=length(X))
  init_pop <- pop < qnorm(0.5)
  return(init_pop*1)
}
