#' Crossover
#'
#' This function crosses over two chromosomes (referred as parent chormosomes) to
#' generate two offsprings. Default cross over count is 1.
#'
#' @param p1 vector; chromosome for Parent 1
#' @param p2 vector; chromosome for Parent 2
#' @param crossover_count positive integer; Number of genes to be cross overed
#' (default set as 1)
#' @return returns a list of two vectors corresponding to two offsprings
#' @export
crossover <- function(p1,p2,crossover_count = 1) {

  #check both the parents have same length:
  if (length(p1) != length(p2))
  {stop("Both the parents should have same no of genes!")}

  #check if the no of crossover points is a valid input
  if (round(crossover_count) != crossover_count)
  {stop("Number of crossover points should be an integer!")}

  gene_count = length(p1)
  cross_points <- unique(ceiling(runif(crossover_count,1,gene_count)))

  cross_points <- sort(cross_points)

  #Initialize offsprings same as parents
  offspring1 = p1
  offspring2 = p2


  for(i in 1:length(cross_points)){
    j = i- 1
    gene_index = cross_points[i]
    if(i == 1){
      offspring1[1:gene_index] <- p1[1:gene_index]
      offspring2[1:gene_index] <- p2[1:gene_index]
    }
    else{
      past_index = cross_points[j]
      offspring1[past_index:gene_index] <- p2[past_index:gene_index]
      offspring1[past_index:gene_index] <- p2[past_index:gene_index]
    }
  }
  # Do leftover
  if(cross_points[length(cross_points)] < length(cross_points) ){
    last_index <- cross_points[length(cross_points)] + 1
    offspring1[last_index:length(p1)] <- p1[last_index:length(p1)]
    offspring2[last_index:length(p2)] <- p2[last_index:length(p2)]
  }

  return(list(offspring1,offspring2))
}
