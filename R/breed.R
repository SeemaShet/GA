#' Breed
#'
#' The function breed a new population given the current population by doing
#' cross-over and mutation of the individuals from current population. It uses
#' two helper functions, cross over and mutation. Two candidate parents
#' are selected everytime at random from the current population to be cross
#' overed and mutated to create offsprings for the new generation. Function makes
#' sure to breed the same number of individuals in the new generation as the
#' original population size.
#'
#' @param current_gen matrix; chromosomes for the current generation
#' @param pop_size positive integer; size of the original population (no of individuals)
#' @param crossover_count vector; number of genes to be cross overed (default set
#' as 1)
#' @param mut_prob float; probability of mutation (between 0 and 1) (default is
#' .05)
#' @param mutations positive integer; number of genes to be mutated if selected
#' to mutate (default  set to 1)
#' @return returns a matrix of new generation
#' @export
breed <- function(current_gen, pop_size, crossover_count = 1, mut_prob = 0.01, mutations=5){

  if(crossover_count < 1 | floor(crossover_count) != crossover_count ){
    stop("Number of crossover points should be a positive integer!.")
  }

  #check if mut_prob is a valid probability
  if (mut_prob > 1 | mut_prob < 0) {
    stop("The probability of mutation should be between 0 and 1.")}

  #Length of current_gen
  current_gen_len = nrow(current_gen)

  #No of genes
  gene_len <- ncol(current_gen)

  new_gen = matrix(NA,nrow=2*ceiling(pop_size/2), ncol=gene_len)

  for (i in 1:ceiling(pop_size/2)){

    #Select two chromosomes for crossover

    parent_chromeSet <- current_gen[sample(nrow(current_gen),size=2),]

    #Doing Crossover
    offspring_chromeSet <- crossover(parent_chromeSet[1,],parent_chromeSet[2,], crossover_count)

    offspring_1 <- unlist(offspring_chromeSet[1])
    offspring_2 <- unlist(offspring_chromeSet[2])

    #Mutation on the offsprings
    offspring_1 <- mutation(offspring_1, mut_prob, mutations)
    offspring_2 <- mutation(offspring_2, mut_prob, mutations)

    new_gen[(2*i-1),] <- offspring_1
    new_gen[(2*i),] <- offspring_2
  }

  #Obtaining the next generation
  new_gen <- new_gen[sample(nrow(new_gen),size=pop_size),]
  return(new_gen)
}
