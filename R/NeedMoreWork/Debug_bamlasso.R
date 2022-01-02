library(tidyverse)
library(mgcv)
# library(BHAM)
library(BhGLM)
library(survival)
library(simsurv)


# Fixed Parameters --------------------------------------------------------a
# * Basic Parameters --------------------------------------------------------
k <- 10               # Smoothing function degrees
n_test <- 1000        # Training set Sample size

n_train <- 200
p <- c(4, 10, 50, 100, 200)[1]
rho <- c(0, 0.5)[1]
pi_cns <- c(0.15, 0.3, 0.4)[1]

# * Survival Parameters ---------------------------------------------------
shape.t <- 1.2     # Shape par of hazard distribution
scale.t <- 1       # Scale par of hazard distribution
shape.c <- 0.8     # Shape par of censoring distribution


# * Derived Parameters
n_total <- n_train + n_test


# Functions ---------------------------------------------------

# * Nonliner Functions ----------------------------------------------------
f_1 <- function(x) (x+1)^2/10
f_2 <- function(x) exp(x+1)/100
f_3 <- function(x) 3*sin(x)/20
f_4 <- function(x) (1.4*x+0.5)/10


#' Auto-regressive correlation matrix
#'
#' @param p integer, the number of dimension
#' @param rho double, range between 0-1, 0 means mutually independent, 1 means complete correlated
#'
#' @return A p*p dimension matrix
#'
#' @examples
#' p <- 5
#' rho <- 0.8
#' X_mat <- MASS::mvrnorm(n=100, rep(0, p), AR(p, rho))
AR <- function(p, rho){
  rho^abs(outer(1:p,1:p,"-"))
}



it <- 1
set.seed(it)


# Helper Functions --------------------------------------------------------
#' Estimate the Weibull scale parameter for controlled censoring proportion
find_censor_parameter <- function(
  lambda, pi.cen,
  shape_hazard, shape_censor
){
  # Estimate the density function of the linear predictor
  dens <- density(lambda, bw = "nrd0",na.rm=TRUE)

  x<-dens$x
  y<-dens$y
  y.loess <-loess(y ~ x,span=0.1)


  density.fun.lambda<-function(x){
    pred.y <- predict(y.loess, newdata=x)
    return(pred.y)
  }

  integration_over_time <- function(
    u,               # Different values of lambda
    # Arguments for integration
    theta, shape_hazard, shape_censor
  ){
    ret <- integrate(            # Start (Integrate of time)
      f = function(ti, theta, alpha.t, alpha.c){
        lambda.i<-u
        part1<-density.fun.lambda(lambda.i)
        part2<-dweibull(ti,alpha.c,theta)
        part3<-exp(-(ti/lambda.i)^alpha.t)
        return(part1*part2*part3)
      },
      0, Inf,
      theta = theta, alpha.t = shape_hazard, alpha.c = shape_censor
    )
    return(ret$value)
  }


  censor.prop <- function(theta, p, shape_hazard, shape_censor){
    cen.P <- integrate(         # Start (Integrate of lambda)
      function(u, theta, shape_hazard, shape_censor){
        sapply(u, integration_over_time,
               # Arguments of integration_over_time
               theta = theta, shape_hazard = shape_hazard, shape_censor = shape_censor
        )
      },
      # Limits of integration
      lower = min(lambda), upper = max(lambda),
      ## Arguments for integration function
      theta = theta, shape_hazard = shape_hazard, shape_censor = shape_censor
    )$value     # End(int of lambda)
    return(cen.P-p)
  }

  theta<-uniroot(censor.prop, interval = c(0.1,200),
                 ## Argument for censor.prop
                 p = pi.cen, shape_hazard = shape_hazard, shape_censor = shape_censor,
                 tol=0.00000001)$root

  return(theta)
}


#' Create spline formula for high-dimension applications
create_HD_formula <- function(formula, data, spl_df, rm_overlap = TRUE, verbose=T){

  if(missing(formula) & missing(data))
    stop("Either formula or data must be provided. The response variable of the model is not supplied.")

  if(missing(formula)){
    if(!is.data.frame(data)) data <- data.frame(data)
    formula = DF2formula(data)
  }

  if(!missing(data)){
    warning("Both formula and dat provided, dat is ignored.")
    warning("Please consider use the function DF2formula and update to improve your formula.")
  }


  if(missing(spl_df)) {
    warning(" No additional spline terms supplied")
  } else {
    # Manipulate spl_df
    sp_trm <-  spl_df %>%
      dplyr::filter(Func!="")  %>% # Removing unnecessary terms
      glue::glue_data("{Func}( {Var}{ifelse(is.na(Args)||Args=='', '', paste0(',', Args))})") %>%
      paste(collapse  = " + ")
    sp_trm <- paste0("~ . + " , sp_trm)
  }
  # Adding Spline Terms
  ret <- update(formula, sp_trm)

  if(verbose){
    cat("Create formula:\n")
    print(ret)
  }

  return(ret)
}


# * Generate Data -------------------------------------------------
x_all <- MASS::mvrnorm(n_train+n_test, rep(0, p), AR(p, rho)) %>%
  data.frame
eta_all <- with(x_all, f_1(X1) + f_2(X2) + f_3(X3) + f_4(X4))
dat_all <- simsurv::simsurv(dist = "weibull",
                            lambdas = scale.t,
                            gammas = shape.t,
                            x = data.frame(eta = eta_all) ,
                            beta = c(eta = 1)) %>%
  data.frame( x_all, eta = eta_all, .)

train_dat <- dat_all[1:n_train, ]
test_dat <- dat_all[(n_train+1):n_total, ]


## Censoring Distribution, Weibull(alpha.c, scale.p)
scale.p <- find_censor_parameter(lambda = exp(-1*train_dat$eta/shape.t),
                                 pi.cen = pi_cns,
                                 shape_hazard = shape.t, shape_censor = shape.c)

train_dat <-  train_dat %>%
  data.frame(
    c_time = rweibull(n = n_train, shape = shape.c, scale = scale.p)
  ) %>%
  mutate(
    cen_ind = (c_time < eventtime),
    status = (!cen_ind)*1
  ) %>%
  rowwise() %>%
  mutate(time = min(c_time, eventtime)) %>%
  ungroup()



# Fit Models------------------------------------------------------------------


# * Spline Specification --------------------------------------------------
mgcv_df <- data.frame(
  Var = grep("X", names(train_dat), value = TRUE),
  Func = "s",
  Args = paste0("bs='cr', k=", k)
)


train_sm_dat <- construct_smooth_data(mgcv_df, train_dat)
train_smooth_data <- train_sm_dat$data

test_sm_dat <- BHAM::make_predict_dat(train_sm_dat$Smooth, dat = test_dat)

bamlasso_raw_mdl <- bamlasso( x = train_smooth_data, y = Surv(train_dat$time, event = train_dat$status),
                              family = "cox", group = make_group(names(train_smooth_data)),
                              ss = c(0.04, 0.5))
s0_seq <- seq(0.005, 0.1, length.out = 20)    # TODO: need to be optimized
cv_res <- tune.bgam(bamlasso_raw_mdl, nfolds = 5, s0= s0_seq, verbose = FALSE)
s0_min <- cv_res$s0[which.min(cv_res$deviance)]
bamlasso_mdl <- bamlasso( x = train_smooth_data, y = Surv(train_dat$time, event = train_dat$status),
                          family = "cox", group = make_group(names(train_smooth_data)),
                          ss = c(s0_min, 0.5))
