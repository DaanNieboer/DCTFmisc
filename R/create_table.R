#' Create a table describing the dataframe.
#'
#' @param X data.frame containing variables
#' @param vars variables for which to create the table
#' @param names Optional vector containing names which will be entered in the table
#' @param strata Optional vector containing membership to different strata. Will add a column to the table for each stratum.
#' @return  Returns a matrix which containing a description of the variables.
#' @examples
#'    set.seed(1234)
#'    X <- data.frame(age = rnorm(1000), educ = sample(0:2, replace = T, size = 1000))
#'    strata <- sample(0:2, replace = T, size = 1000)
#'    create_table(X, names = c("Age", "Education"), strata = strata)

create_table <- function(X, vars = NULL, names = NULL, strata = NULL){
  if(is.null(vars)){
    vars <- names(X)
  }
  if(is.null(names)){
    names <- vars
  }
  if(!is.null(strata)&length(strata)!=nrow(X)){
    stop("Length of strata not equal to the total number of observations!")
  }
  if(length(vars)!=length(names)){
    stop("Length of the vector with variables and variable names are not the same!")
  }
  nvars <- length(vars)
  tab     <- NULL
  for(i in 1:nvars){
    tab <- rbind(tab, descr_vars(X[, vars[i]], names[i]))
  }
  colnames(tab)[ncol(tab)] <- paste0("Total (n=", nrow(X), ")")


  if(!is.null(strata)){
    names_strata <- unique(strata)
    for(i in 1:length(names_strata)){
      tab_strata <- NULL
      for(j in 1:nvars){
        tab_strata <- rbind(tab_strata, descr_vars(X[strata==names_strata[i], vars[j]], names[j]))
      }
      tab <- cbind(tab, tab_strata[, ncol(tab_strata)])
      colnames(tab)[ncol(tab)] <- paste0(names_strata[i], " (n=", sum(strata==names_strata[i]), ")")
    }
  }
  return(tab)
}

