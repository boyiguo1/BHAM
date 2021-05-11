#' Simulate Spline Data
#'
#' @description
#'
#' @param n Integer, sample size
#' @param p Integer, number of predictors. The minimum number of predictors is default to 4. If user input p<4, p changes to 4 with a warning message
#' @param family Family object, one of `binomial` (default), `gaussian` , `poisson`. Please see \code{\link[stats]{family}} for more detail
#' @param dispersion Double, required for gaussian distribution, match to sd parameter.
#'
#' @return a list contains `dat`, `eta`, `mu`
#' \itemize{
#'     \item{dat}{ a numeric matrix, contains predictors `x1` to `xp` where p is replaced by the integer value. The outcome variable is named `y`}
#'     \item{eta}{the linear predictor of the simulated data}
#'     \item{mu}{the mean of the simulated data}
#'
#' }
#' @export
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rpois rbinom rnorm
#'
#' @examples
#' # Binomial Outcome
#' sim_Bai(100, 4)
#'
#' # Logistic Outcome
#' sim_Bai(100, 4, family = poisson)
#'
#' # Gaussian Outcome with measurement error variance = 1
#' sim_Bai(100, 4, family = gaussian, dispersion = 1 )
sim_Bai <- function(n, p, family = binomial(), dispersion = 1){
  if(p < 4){
    p <- 4
    warning("There must be more than 4 variables. Changing p to 4")
  }

  x <- MASS::mvrnorm(n=n, mu= rep(0, p), Sigma = diag(p))
  eta <- 5*sin(2*pi*x[,1]) - 4*cos(2*pi*x[,2]-0.5) + 6*(x[,3]-0.5)-5*(x[,4]^2-0.3)
  # theta <- exp(lp)/(1+exp(lp))
  mu <- family$linkinv(eta)

  if(family$family == "binomial")
    y <- stats::rbinom(n, size = 1, mu)
  else if(family$family == "gaussian")
    y <- stats::rnorm(n, mean = mu, sd = dispersion)
  else if(family$family == "poisson")
    y <- stats::rpois(n, mu)
  else
    stop("Does not support")

  colnames(x) <- paste0("x", 1:p)


  return(list(dat=cbind(x,y), eta=eta, mu=mu))
}
