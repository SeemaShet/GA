#' Compute the fitness score
#'
#' This function calculates the fitness level
#'
#' @param Y vector; target variable
#' @param X matrix/dataframe; design matrix (predictors)
#' @param criteria string; objective criteria to be used
#' @param reg_model model type (lm or glm) default is lm
#' @return returns a vector of fitness level for each individual
#' @export
compute_fitness <- function(Y, X, criteria,reg_model=lm){
  cur_df <- data.frame(Y, X)
  cur_model <- reg_model(cur_df)

  if(criteria=="RMSE"){
    return(sqrt(crossprod(c(cur_model$residuals)) / length(cur_model$residuals)))
  }else{
    criteria= match.fun(criteria)
    return(criteria(cur_model))
  }
}
