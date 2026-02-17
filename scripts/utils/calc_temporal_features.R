calculate_temporal_features <- function(iris) {
  n <- length(iris)
  if (n < 3) {
    return(NA)
  }
  mu_tau <- mean(iris)
  sigma_tau <- sd(iris)
  
  # Burstiness parameter
  B <- (sigma_tau - mu_tau) / (sigma_tau + mu_tau)
  return(B)
  
  # # Coefficient of variation (related measure)
  # cv <- sigma_tau / mu_tau
  # 
  # # Memory coefficient (temporal correlation)
  # if (n >= 3) {
  #   memory_coef <- cor(iris[-1], iris[-n], use = "complete.obs")
  # } else {
  #   memory_coef <- NA
  # }
  # return(paste(B,memory_coef,cv, sep = ";;"))
}
