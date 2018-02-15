plot_stacked_density <- function(x, grp, xlim, xlab, ylab){
  n_grp      <- length(unique(grp))
  unique_grp <- sort(unique(grp))

  dens_list <- list()

  par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
  plot(1, type = "n", xlim = xlim, ylim = c(1, n_grp + 2),
       axes = FALSE, xlab = "", ylab = "")

  for(i in 1:n_grp){
    dens <- density(x[grp==unique_grp[i]], na.rm = T)
    max_y <- max(dens$y)
    lines(  dens$x, 0.8 * dens$y/max_y + i)
    polygon(dens$x, 0.8 * dens$y/max_y + i, col = rgb(0, 0, 0, 0.4), border = NA)
    abline(h = i, lwd = 0.5)
  }
  axis(1)
  axis(2, at = 1:n_grp, labels = unique_grp,las = 2)

  mtext(side = 1, line = 2, xlab)
  mtext(side = 2, line = 2, ylab)
}
