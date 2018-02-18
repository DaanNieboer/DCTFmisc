#' Create a table describing the dataframe.
#'
#'  @param X data.frame containing variables
#'  @param vars variables for which to create the table
#'  @param names Optional vector containing names which will be entered in the table
#'  @return  Returns a matrix which containing a description of the variables.
#'

create_table <- function(X, vars = NULL, names = NULL){
  if(is.null(vars)){
    vars <- names(X)
  }
  if(is.null(names)){
    names <- vars
  }
  if(length(vars)!=length(names)){
    stop("Length of the vector with variables and variable names are not the same!")
  }
  nvars <- length(vars)
  tab     <- NULL
  for(i in 1:nvars){
    tab <- rbind(tab, descr_vars(X[, vars[i]], names[i]))
  }
  return(tab)
}

