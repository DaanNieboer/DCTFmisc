descr_vars <- function(X, var){
  values <- unique(X)[!is.na(unique(X))]
  if(length(values)<=15){
    if(length(values)==2){
      result <- matrix(nrow = 2, ncol = 3)
      result[1, 1]              <- var
      result[1, 2]              <- "Total"
      n                         <- sum(!is.na(X))
      result[1, 3]              <- paste(n, " (", sprintf('%.0f',
                                                          100 * n/length(X)),
                                         "%)", sep = "")
      X            <- X[!is.na(X)]
      frequencies  <- as.data.frame(table(X))
      result[2, 1] <- ""
      result[2, 2] <- as.character(frequencies[2, 1])
      n_cat        <- frequencies[2 ,2]
      result[2, 3] <- paste(n_cat, " (", sprintf('%.0f', 100 * n_cat/n), "%)",
                            sep = "")

    }else{
      result <- matrix(nrow = length(values) + 1, ncol = 3)
      result[1, 1]              <- var
      result[1, 2]              <- "Total"
      n                         <- sum(!is.na(X))
      result[1, 3]              <- paste(n, " (", sprintf('%.0f',
                                                          100 * n/length(X)),
                                         "%)", sep = "")
      X           <- X[!is.na(X)]
      frequencies <- as.data.frame(table(X))
      result[2:nrow(result), 2] <- as.character(frequencies[, 1])
      for(i in 1:nrow(frequencies)){
        n_cat            <- frequencies[i , 2]
        result[i + 1, 1] <- ""
        result[i + 1, 3] <- paste(n_cat, " (", sprintf('%.0f', 100 * n_cat/n),
                                  "%)", sep = "")
      }
    }
  }else{
    result       <- matrix(nrow = 2, ncol = 3)
    result[1, 1] <- var
    result[1, 2] <- "Total"
    n            <- sum(!is.na(X))
    result[1, 3] <- paste(n, " (", sprintf('%.0f', 100 * n/length(X)), "%)",
                          sep = "")
    result[2, 1] <- ""
    result[2, 2] <- "Median (25th - 75th percentile)"
    m <- sprintf('%.1f', median(X, na.rm = T))
    l <- sprintf('%.1f', quantile(X, na.rm = T, probs = 0.25))
    u <- sprintf('%.1f', quantile(X, na.rm = T, probs = 0.75))
    result[2, 3] <- paste(m, " (", l, " - ", u, ")", sep = "")
  }
  colnames(result)  <- c("Variable", "Measure/Category", "")
  return(result)
}


descr_cont <- function(X, digits = 1){
  ndigits <- paste('%.', digits, "f", sep = "")
  m <- sprintf(ndigits, median(X, na.rm = T))
  l <- sprintf(ndigits, quantile(X, na.rm = T, probs = 0.25))
  u <- sprintf(ndigits, quantile(X, na.rm = T, probs = 0.75))
  result <- paste(m, " (", l, " - ", u, ")", sep = "")
  return(result)
}

descr_cat <- function(X, val = 1){
  n_cat <- sum(X==val, na.rm = T)
  n     <- length(X)
  result <- paste(format(n_cat, big.mark = ",", scientific = FALSE),
                  " (", sprintf('%.0f', 100 * n_cat/n), "\\%)", sep = "")
  return(result)
}

n_missing <- function(X){
  n_complete <- format(sum(!is.na(X)), big.mark = ",", scientific = FALSE)
  results    <- paste(n_complete, " (",
                      sprintf('%.0f', 100 * sum(!is.na(X))/length(X)), "\\%)",
                      sep = "")
  return(results)
}

descr_cat2 <- function(X){
  result <- as.data.frame(table(X))
}
