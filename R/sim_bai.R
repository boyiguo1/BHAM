#' Title
#'
#' @param n
#' @param p
#'
#' @return
#' @export
#'
#' @examples
sim_Bai_logistic <- function(n, p){
  x <- MASS::mvrnorm(n=n, mu= rep(0, p), Sigma = diag(p))
  lp <- 5*sin(2*pi*x[,1]) - 4*cos(2*pi*x[,2]-0.5) + 6*(x[,3]-0.5)-5*(x[,4]^2-0.3)
  theta <- exp(lp)/(1+exp(lp))
  y <- rbinom(n, 1, theta)

  return(list(x=x, lp=lp, theta = theta, y = y))
}
