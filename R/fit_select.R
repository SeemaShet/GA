#' Selects fit individuals
#'
#' This function selects the best fitted individuals to be used as candidates
#' for next generation.
#'
#' @param Y vector; target variable
#' @param X matrix/dataframe; design matrix (predictors)
#' @param models matrix; current generation of individuals (candidate models)
#' @param fit_levs vector; fitness score for indivuals of current generation
#' @param f float; fraction of indiviuals to be selected from the current
#' generation
#' @param fselect string; selection criteria to be used
#' @return returns a vector of fitness levels and variance of fitness in the
#' population
#' @export
fit_select <- function(Y, X, models,fit_levs, f =0.5, fselect=c("standard", "stochastic")){
  
    if(fselect!="standard" & fselect!="stochastic") 
    stop("Select from stochastic or standard for the selection procedure")
  if (f<0.1) warning("Low value of f. Consider using >0.5 for better results")
  
  d<-data.frame(models, fit_levs) 
  if(fselect=='standard'){
    #print(quantile(d$fit_levs, f))
    selected_parents <- d[which(d$fit_levs <= quantile(d$fit_levs, probs=f)), ]
  }
  else if(fselect=="stochastic"){
    #creating probability column
    prob <- (sum(d$fit_levs) - d$fit_levs)/sum(d$fit_levs)
    n_samp <- floor(f * nrow(d))
    s=sample(seq(1:length(fit_levs)), size=n_samp ,prob=prob,replace=FALSE)
    selected_parents <- d[s,]
  }
}
