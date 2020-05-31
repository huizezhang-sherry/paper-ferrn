calc_kol <- function(data, proj, sphere = FALSE) {

  if(sphere){
    data <- sphere_data(rescale(data))
  }

  mat <- as.matrix(data) %*% proj

  mat_bin_count <- ash::bin1(mat %>% scale(), c(min(mat), max(mat)), 100)$nc
  norm_bin_count <- ash::bin1(rnorm(nrow(mat)) %>% scale(), c(min(mat), max(mat)), 100)$nc
  diff <- sum((mat_bin_count - norm_bin_count)^2)/nrow(mat)

  diff
}

calc_kol_cdf <- function(data, proj, sphere = FALSE) {

  if(sphere){
    data <- sphere_data(rescale(data))
  }

  mat <- as.matrix(data) %*% proj
  norm <- rnorm(nrow(mat))

  as.numeric(ks.test(mat, norm)$statistic)
}
