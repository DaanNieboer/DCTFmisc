#' Recode the values in a vector
#'
#' @param x vector containing values
#' @param from vector containing the unique elements of x
#' @param to values in which the corresponging elements of from needs to be changed in
#' @return  Returns a vector where the values of x are changed from the values in the vector from to the values in the vector to

recode_values <- function(x, from, to){
  pos_values <- match(x, from)
  res        <- to[pos_values]

  return(res)
}

