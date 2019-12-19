#' Mutation
#'
#' The mutation function takes the vector of a chromsome and introduce random
#' mutations into it. Mutations occur with a probability determined by the user
#' and if an offspring is selected for mutation, user determines the number
#' of genes that will be mutated.
#'
#' @param chromosome vector; an individual chromosome for mutation
#' @param mut_prob float; probability of mutation (between 0 and 1) (default is
#' .05)
#' @param mutations positive integer; number of genes to be mutated if selected
#' to mutate (default set to 1)
#' @return returns a vector after mutation
#' @export
mutation <- function(chromosome, mut_prob = 0.05, mutations=1) {

  #check if mut_prob is a valid probability
  if (mut_prob > 1 | mut_prob < 0)
  {stop("The probability of mutation should be between 0 and 1.")}
  
   #warning for good value of p
  if (mut_prob > 0.5) 
  {warning("Very high mutation probability!")}

  if( runif(1) <= mut_prob){
    length(chromosome)
    muts <- sample(length(chromosome), mutations, replace=FALSE)
    chromosome[muts] <- 1- chromosome[muts]
    return(chromosome)
  }
  else{
    return(chromosome)
  }
}
