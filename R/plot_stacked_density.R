#' Create a stacked stacked density chart
#'
#' @param x values for the density plot.
#' @param grp variable containing the membership to groups.
#' @param xlab label of the x-axis.
#' @param ylab label of the y-axis.
#' @param y_labels labels at the ticks of the y-axis.
#' @return A stacked density chart showing the density of \code{x} in the different groups contained in \code{grp}



plot_stacked_density <- function(x, grp, xlim, xlab, ylab, y_labels = NULL){
  n_grp      <- length(unique(grp))
  unique_grp <- sort(unique(grp))
  if(is.null(y_labels)){
    y_labels <- unique_grp
  }
  if(length(unique_grp)!=length(y_labels)){
    stop("Nr. of groups not equal to the number of y_labels!")
  }

  dens_list <- list()

  par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
  plot(1, type = "n", xlim = xlim, ylim = c(1, n_grp + 2),
       axes = FALSE, xlab = "", ylab = "")

  for(i in 1:n_grp){
    dens <- density(x[grp==unique_grp[i]])
    max_y <- max(dens$y)
    lines(  dens$x, 0.8 * dens$y/max_y + i)
    polygon(dens$x, 0.8 * dens$y/max_y + i, col = rgb(0, 0, 0, 0.4), border = NA)
    abline(h = i, lwd = 0.5)
  }
  axis(1)
  axis(2, at = 1:n_grp, labels = y_labels,las = 2)

  mtext(side = 1, line = 2, xlab)
  mtext(side = 2, line = 2, ylab)
}
