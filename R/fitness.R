#' Calculates fitness score
#'
#' This function calculates the fitness level and the variance of fitness for all
#' individuals in the current generation and uses a helper function compute_fitn
#' ss to do that.
#'
#' @param Y vector; target variable
#' @param X matrix/dataframe; design matrix (predictors)
#' @param models matrix: individuals of the population (candidate models)
#' @param core: positive integer; No of cores to be used for parallization
#' (default 1 which means no parallization)
#' @param criteria string; objective criteria to be used
#' @return returns a vector of fitness levels and variance of fitness
#' @export

fitness <- function(Y, X, models, core=1, criteria="AIC",reg_model=lm) {
  model_count <- nrow(models)
  # if choose to use multiple cores (and run in parallel)
  plan(multiprocess, workers = core)
  cl <- makeCluster(core) #not to overload your computer
  registerDoParallel(cl)
  if(core > 1){

    fit <- foreach(i=1:model_count, .combine='c') %dopar% {

      cur_X <- X[,which(models[i,]==TRUE) ]
      compute_fitness(Y, cur_X, criteria,reg_model=lm)
    }
  #stop cluster
  stopCluster(cl)
  var_fit <- var(unlist(fit))
  return(list(fit, var_fit))
  }
  # otherwise do a simple loop
  else if(core ==1 ){
    fit <- rep(100000, times=model_count )
    for(i in 1:model_count){
      cur_X <- X[,which(models[i,]==TRUE) ]
      fit[i] <- compute_fitness(Y, cur_X, criteria,reg_model=lm)
    }
    var_fit <- var(fit)
    return(list(fit, var_fit))
  }
}
