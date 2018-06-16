# -------------------------------- #
# test List to R filter functions
# -------------------------------- #

context('Filters and so on')


library(tidyverse)


test_that("list_to_Rfilter makes expected string", {
  
  filter_list = list(Species = c('versicolor','virginica'),
                     Species = c('versicolor-virginica'),
                     Sepal.Width = c('>3','<5'))
  
  filter_statement = list_to_Rfilter(filter_list)
  expected_string = "Species %in% c('versicolor', 'virginica') & Species %in% c('versicolor', 'virginica') &  Sepal.Width > 3 &  Sepal.Width < 5"
  
  
  expect_equal(filter_statement, expected_string)

  
})


test_that("make_status_from_rule makes valid column", {
  
  iris_aux = iris %>%
    mutate(status = as.integer(Species %in% c('versicolor', 'virginica')))
  
  
  expect_equal(make_status_from_rule(iris, "Species %in% c('versicolor', 'virginica')")
               , iris_aux)
  
})














