#' Primary Select Function
#'
#' Main function that uses all the other auxillary functions to select the
#' best variables for a given regression problem.
#'
#' @param Y vector; target variable
#' @param X matrix/dataframe; design matrix (predictors)
#' @param models matrix; current generation of individuals (candidate models)
#' @param core positive integer; No of cores to be used for parallization
#' (default 1 which means no parallization)
#' @param criteria string; objective criteria to be used
#' @param f float; fraction of indiviuals to be selected from the current
#' generation during fitness selection
#' @param fselect string; selection criteria to be used
#' @param crossover_count positive integer; Number of genes to be cross overed
#' (default set as 1)
#' @param mut_prob  float; probability of mutation (between 0 and 1) (default is
#' .05))
#' @param mutations positive integer; number of genes to be mutated if selected
#' to mutate (default set to 1)
#' @param pop_size positive integer; size of the original population (no of
#' individuals)
#' @param converge string; converging criteria to be used (should be count or
#' delta)
#' @param num_iter positive integer; maximum number of iterations
#' @param reg_model model type (lm or glm) default is lm
#' @return returns a list of variables best fitted for the problem
#'
#' @examples
#'
#' library(tidyverse)
#' library(GA)
#'
#' #let's simulate some data to test our algorithm on
#'
#' set.seed(121119)
#' x <- matrix(rnorm(2000*100, 0,5), nrow=2000, ncol=100)
#'
#' #Let's simulate 5 coefficients to be non-zero
#' beta0 <- 2
#' beta1 <- 3
#' beta10 <- 1
#' beta60 <- 6
#' beta92 <- 9
#' beta87 <- 2
#'
#' #Simulate sample y (output variable)
#' y <- beta0 + beta1*x[,1] + beta10 * x[,10] + beta60*x[,60] + beta92*x[,92] + beta87*x[,87] + rnorm(2000)
#'
#' #Calling the Select function with default arguments
#' select(y,x)
#'
#' #Calling the select function with user defined inputs
#' library(doParallel)
#' library(future)
#' select(y,x,core=4,criteria="BIC",f=0.6,crossover_count=3,mut_prob=0.05, num_iter=500,reg_model=glm)
#'
#' @export
select <- function(Y, X, models, core=1, criteria="AIC",
                   f =0.5, fselect="standard",
                   crossover_count = 1,
                   mut_prob = 0.01, mutations=5,
                   pop_size=200, converge="delta" ,num_iter=100,reg_model=lm){
  #tests on input
  if(class(X)!="matrix" & class(X)!="data.frame") stop("X should be either dataframe or a matrix")
  if(class(Y)!="numeric") stop("Y should be numeric")
  if(pop_size<10) warning("Very low population size! Consider increasing it.")
  if(core==1) warning("Consider running it parallelly using multiple cores to improve efficiency")

  test_criteria=try(match.fun(criteria),silent=TRUE)
  if(class(test_criteria)=="try-error") {
  warning("Criteria not valid, using default of AIC")
  criteria<-"AIC"}

  X=as.data.frame(X)

  # Initial population if not given through models
  if(missing(models))  models=init_pop(pop_size,X)

  # initial fitness levels and selection for breeding
  fit_levs <- fitness(Y,X, models, core, criteria,reg_model)
  #print(fit_levs)
  prev_var <- fit_levs[[2]]
  # converge once you reduce sampling variance by 90 percent
  converge_var <- .1*prev_var
  select_fit <- fit_select(Y,X, models, fit_levs[[1]],  f, fselect)
  #print(select_fit)
  # begin breeding
  if( converge=='count'){
    count <- 1
    while(count <= num_iter){
      cur_breed <- breed(select_fit, pop_size, crossover_count, mut_prob, mutations=5)
      fit_levs <- fitness(Y,X, cur_breed, core, criteria,reg_model)
      select_fit <- fit_select(Y,X, cur_breed, fit_levs[[1]], f, fselect=fselect)
      #print(nrow(select_fit))
      count <- count + 1

    }
    # return fittest individual from final population
    return(arrange(select_fit, fit_levs)[1,1:ncol(X)])
  }
  if( converge=='delta'){
    done <- FALSE
    count <- 1
    while(done == FALSE){
      #print(count)
      cur_breed <- breed(select_fit, pop_size, crossover_count, mut_prob, mutations=5)
      fit_levs <- fitness(Y,X, cur_breed, core, criteria,reg_model)
      select_fit <- fit_select(Y,X, cur_breed, fit_levs[[1]], f, fselect=fselect)
      cur_var <- fit_levs[[2]]
      count <- count + 1
      if(cur_var < converge_var ){
        done <- TRUE
        return(arrange(select_fit, fit_levs)[1,1:ncol(X)])
      }
      else{
        prev_var <- cur_var
      }
    }
  }
}
