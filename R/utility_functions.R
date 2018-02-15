not_na_count <- function(x) sum(!is.na(x))

filter_groups_by_size <- function(x, grp, min_size = 3){
  nr_points <- aggregate(x, by = list(grp), FUN = not_na_count)
  grp_sel   <- nr_points$Group.1[nr_points$x>=min_size]

  res <- data.frame(value = x, group = grp)
  res <- res[res$group %in% grp_sel, ]
  res <- res[!is.na(res$value), ]
  return(res)
}
