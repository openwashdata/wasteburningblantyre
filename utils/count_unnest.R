#' Count and unnest a variable in a data frame with list-columns
#'
#' The `count_unnest()` function is an R function designed to handle data 
#' frames with list columns. It takes a data frame and a variable as input, 
#' groups the data by the specified variable, calculates the count of each 
#' group, and unnests the specified variable if it is a list column.
#'
#' @param data A data frame
#' @param ... The variable(s) to group by and unnest
#' @param percent Logical, if TRUE, adds a percent column to the result (default: TRUE)
#' @param sort Logical, if TRUE, sorts the result by count in descending order (default: TRUE)
#' @return A data frame with the count of each group and the unnested variable
#' 
count_unnest <- function(data, ..., percent = TRUE, sort = TRUE) {
 
  result <- data |> 
    group_by(...) |> 
    summarise(n = n()) |> 
    unnest(cols = c(...), keep_empty = TRUE) |> 
    group_by(...) |> 
    summarise(
      n = sum(n)
    ) |> 
    ungroup() 
  
  if (percent) {
  result <- result |> 
      mutate(percent = n / nrow(data) * 100)
  }
 
 if (sort) {
   result <- result |> 
     arrange(desc(n))
 }
  
  return(result)
  
}


