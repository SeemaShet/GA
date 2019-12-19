context("Testing all the functions")


test_that("Fitness function errors", {
  a=matrix(rnorm(5*5), nrow=5, ncol=5)
  expect_error(compute_fitness(1,2,3,criteria = abc),)
  })

test_that("Selection errors", {

  expect_error(fit_select(1, 1, 1,1, f =0.5, fselect="abc"),"Select from stochastic or standard for the selection procedure")
  expect_warning(fit_select(1, 1, 1,1, f =0.09, fselect="standard"),"Low value of f. Consider using >0.5 for better results")

})

test_that("Crossover and mutation errors", {

  expect_error(crossover(c(1,2),4,crossover_count = 2),"Both the parents should have same no of genes!")
  expect_error(crossover(3,4,crossover_count = 2.3),"Number of crossover points should be an integer!")
  expect_warning(mutation(c(0,1,0,1), mut_prob = 0.7, mutations=2),"Very high mutation probability!")

})

test_that("Breed errors", {

  expect_error(breed(2, 3, crossover_count = -9, mut_prob = 0.01, mutations=5),"Number of crossover points should be a positive integer!.")
  expect_error(breed(2, 3, crossover_count = 9, mut_prob = 3, mutations=5),"The probability of mutation should be between 0 and 1.")

})

test_that("Select errors", {
  a=matrix(rnorm(5*5), nrow=5, ncol=5)
  expect_warning(select(Y=a[,1], X=a, core=1,
                        pop_size=15,converge = "count",num_iter = 1),
                 "Consider running it parallelly using multiple cores to improve efficiency!")
  expect_warning(GA::select(Y = a[, 1], X = a,
                            pop_size = 8,core=2,converge="count",num_iter = 4),
                 "Very low population size! Consider increasing it.")
  expect_error(select(Y=a, X="a"),"X should be either dataframe or matrix")
  expect_error(select(Y="a", X=a),"Y should be numeric")


})
