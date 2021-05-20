library(tidyverse)
library(assertr)

test = tribble(
  ~x,~y,
  1,4,
  3,8,
  1,-2,
  3,1,
  5,81
)


my_error_fun = function( ... ) {
  args = list(...)
  
  do.call( just_warn, args )
  
  bad.data = args[[1]][[1]]$error_df
  
  these.failed.rows = args$data %>% 
    slice( bad.data$index )
  
  if(!exists("my.failed.rows", inherits=TRUE)) {
    my.failed.rows = NULL
  }
  my.failed.rows = these.failed.rows
  #assign( "my.failed.rows", my.failed.rows, envir=parent.frame(n=3) )
  assign( "my.failed.rows", my.failed.rows, envir= .GlobalEnv )
  
  good.rows = args$data %>% slice(-bad.data$index)
  
  return(good.rows)
  
}

my.result <- test %>%
  insist(within_n_mads(3),y, error_fun = my_error_fun) %>% 
  group_by(x) %>%
  summarise(avg = mean(y))

print(my.result)

print(my.failed.rows)

  
  
