
#' Bayesian hierarchical additive model
#'
#' This function fits a Bayesian hierarchical generalized additive model (BGAM)
#' using a spline-based approach. It supports various families, including
#' Gaussian, Binomial, Poisson, and Negative Binomial (NegBin)
#'
#' @param formula,data,offset,weights,subset,na.action,start,etastart,mustart,control These arguments are the same as in \code{\link{glm}}
#' @param family can be all the standard families defined in glm. also can be Negative Binomial (NegBin or "NegBin").
#' @param prior Prior distributions for the coefficients. Four types of priors can be used; Student-t: Student(mean, scale, df, autoscale) (default: mean=0, scale=0.5, df=1, autoscale=TRUE), Double-exponetial: De(mean, scale, autoscale) (default: mean=0, scale=0.5, autoscale=TRUE), mixture double-exponential: mde(mean, s0, s1, b), and mixture Student-t: mt(mean, s0, s1, df, b), s0 < s1, default: mean=0, s0=0.04, s1=0.5, df=1, b=1. The mean, scale, df and b can be a vector. For example, scale = c(a1,a2,...,ak); if k < the total number of predictors, it is internally expanded to c(a1,a2,...,ak, rep(ak,J-k)). If autoscale=TRUE, scale will be modified internally (see details).
#' @param group a numeric vector, or an integer, or a list defining the groups of predictors. Only used for mde or mt priors. If group = NULL, all the predictors form a single group. If group = K, the predictors are evenly divided into groups each with K predictors. If group is a numberic vector, it defines groups as follows: Group 1: (group\[1\]+1):group\[2\], Group 2: (group\[2\]+1):group\[3\], Group 3: (group\[3\]+1):group\[4\], ..... If group is a list of variable names, group\[\[k\]\] includes variables in the k-th group.
#' @param method.coef jointly updating all coefficients or updating coefficients group by group. The default is jointly updating. If method.coef = NULL or method.coef is missing, jointly updating. If method.coef = K, update K coefficients at a time. method.coef can be a numeric vector or a list of variable names (as defined by group) that defines groups. If the number of coefficients is large, the group-by-group updating method can be much faster than the jointly updating.
#' @param theta.weights Optional weights for the dispersion parameter.
#' @param inter.hierarchy Optional specification for hierarchical interaction terms.
#' @param inter.parents  Optional specification for parent-child relationships in hierarchical terms.
#' @param prior.sd Standard deviation for the prior distribution.
#' @param dispersion Dispersion parameter for the model.
#' @param Warning 	logical. If TRUE, show the error messages of not convergence and identifiability.
#' @param verbose logical. If TRUE, print out number of iterations and computational time.
#'
#' @return  This function returns an object of class "glm", including all outputs from the function \code{\link{glm}}, and also results for the additional parameters in the hierarchical models.
#'
#'
#' @export
#'
#' @examples
bgam <- function (formula, family=gaussian, data, offset, weights, subset, na.action,
                  start=NULL, etastart, mustart, control=stats::glm.control(epsilon=1e-04, maxit=50),
                  prior=Student(), group=NULL, method.coef,
                  theta.weights=NULL, inter.hierarchy=NULL, inter.parents=NULL,
                  prior.sd=0.5, dispersion=1, Warning=FALSE, verbose=FALSE)
{
  start.time <- Sys.time()

  autoscale <- prior$autoscale
  if (is.null(autoscale)) autoscale <- FALSE
  prior.mean <- prior$mean
  prior.scale <- prior$scale
  if (is.null(prior.scale)) prior.scale <- 0.5
  prior.df <- prior$df
  if (is.null(prior.df)) prior.df <- 1
  ss <- prior$ss
  if (is.null(ss)) ss <- c(0.04, 0.5)
  b <- prior$b
  prior <- prior[[1]]
  if (missing(method.coef)) method.coef <- NULL

  contrasts <- NULL
  call <- match.call()
  if (is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family))
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  if (missing(data))
    data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "etastart", "mustart", "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- NULL
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  Y <- model.response(mf, "any")
  if (length(dim(Y)) == 1) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if (!is.null(nm))
      names(Y) <- nm
  }
  X <- if (!is.empty.model(mt))
    stats::model.matrix(mt, mf, contrasts)
  else matrix(, NROW(Y), 0L)
  weights <- as.vector(model.weights(mf))
  if (!is.null(weights) && !is.numeric(weights))
    stop("'weights' must be a numeric vector")
  if (!is.null(weights) && any(weights < 0))
    stop("negative weights not allowed")
  offset <- as.vector(model.offset(mf))
  if (!is.null(offset)) {
    if (length(offset) != NROW(Y))
      stop(gettextf("number of offsets is %d should equal %d (number of observations)",
                    length(offset), NROW(Y)), domain = NA)
  }
  mustart <- model.extract(mf, "mustart")
  etastart <- model.extract(mf, "etastart")

  fit <- bglm_spline.fit(x=X, y=Y, weights=weights, start=start,
                         etastart=etastart, mustart=mustart, offset=offset,
                         family=family, control=control, intercept=attr(mt, "intercept") > 0,
                         prior=prior, group=group, method.coef=method.coef,
                         dispersion=dispersion, prior.mean=prior.mean, prior.sd=prior.sd,
                         prior.scale=prior.scale, prior.df=prior.df, autoscale=autoscale, ss=ss, b=b,
                         theta.weights=theta.weights, inter.hierarchy=inter.hierarchy, inter.parents=inter.parents,
                         Warning=Warning)

  fit$model <- mf
  fit$na.action <- attr(mf, "na.action")
  fit <- c(fit, list(call = call, formula = formula, terms = mt,
                     data = data, offset = offset, control = control,
                     contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, mf)) )

  class(fit) <- c("glm", "lm")
  if (family[[1]]==NegBin()[[1]]) class(fit) <- c("negbin", "glm", "lm")
  stop.time <- Sys.time()
  minutes <- round(difftime(stop.time, start.time, units = "min"), 3)
  if (verbose) {
    cat("EM-IWLS iterations:", fit$iter, "\n")
    cat("Computational time:", minutes, "minutes \n")
  }

  fit
}

#*******************************************************************************

bglm_spline.fit <- function (x, y, weights=rep(1, nobs), start=NULL, etastart=NULL, mustart=NULL,
                             offset=rep(0, nobs), family=gaussian(), control=glm.control(), intercept=TRUE,
                             prior="de", group=NULL, method.coef=1,
                             dispersion=1, prior.mean=0, prior.sd=0.5, prior.scale=1, prior.df=1, autoscale=TRUE,
                             ss=c(0.05, 0.1), b=1, theta.weights=NULL, inter.hierarchy=NULL, inter.parents=NULL,
                             Warning=FALSE)
{
  ss <- sort(ss)
  if (prior == "mde" | prior == "mt")
    prior.sd <- prior.scale <- ss[length(ss)]  # used for ungrouped coefficients

  if (is.null(dispersion)) dispersion <- 1

  d <- prepare(x=x, intercept=intercept, prior.mean=prior.mean, prior.sd=prior.sd, prior.scale=prior.scale,
               prior.df=prior.df, group=group)
  x <- d$x
  prior.mean <- d$prior.mean
  prior.sd <- d$prior.sd
  prior.scale <- d$prior.scale
  prior.df <- d$prior.df
  sd.x <- d$sd.x
  min.x.sd <- d$min.x.sd
  group <- d$group
  group.vars <- d$group.vars
  ungroup.vars <- d$ungroup.vars

  if (autoscale){
    prior.scale <- prior.scale / auto_scale(x, min.x.sd)
    if (family[[1]]=="gaussian") prior.scale <- prior.scale * stats::sd(y)
  }

  x0 <- x
  if (intercept) x0 <- x[, -1, drop = FALSE]
  g0 <- Grouping(all.var = colnames(x0), group = method.coef)
  group0 <- g0$group.vars
  covars0 <- g0$ungroup.vars
  if (intercept) covars0 <- c(colnames(x)[1], covars0)
  method.coef <- "joint"
  if (length(group0) > 1) method.coef <- "group"

  # for mixture prior
  if (prior == "mde" | prior == "mt") {
    if (length(ss) != 2) stop("ss should have two positive values")
    gvars <- unlist(group.vars)
    theta <- p <- rep(0.5, length(gvars))
    names(theta) <- names(p) <- gvars

    if (is.null(theta.weights)) theta.weights <- rep(1, length(gvars))
    if (length(theta.weights)!=length(gvars)) stop("all grouped variables should have theta.weights")
    if (any(theta.weights > 1 | theta.weights < 0)) stop("theta.weights should be in [0,1]")
    names(theta.weights) <- gvars

    if (length(b) < length(group.vars))
      b <- c(b, rep(b[length(b)], length(group.vars) - length(b)) )
    b <- b[1:length(group.vars)]
  }

  # for negative binomial model
  nb <- FALSE
  if (family[[1]] == NegBin()[[1]]) nb <- TRUE
  # if (nb){
  #   if (!requireNamespace("MASS")) install.packages("MASS")
  #   library(MASS)
  # }

  # *************************
  x <- as.matrix(x)
  xnames <- dimnames(x)[[2]]
  ynames <- if (is.matrix(y))
    rownames(y)
  else names(y)
  conv <- FALSE
  nobs <- NROW(y)
  nvars <- ncol(x)
  EMPTY <- nvars == 0
  if (is.null(weights))
    weights <- rep.int(1, nobs)
  if (is.null(offset))
    offset <- rep.int(0, nobs)
  variance <- family$variance
  dev.resids <- family$dev.resids
  aic <- family$aic
  linkinv <- family$linkinv
  mu.eta <- family$mu.eta
  if (!is.function(variance) || !is.function(linkinv))
    stop("'family' argument seems not to be a valid family object")
  valideta <- family$valideta
  if (is.null(valideta))
    valideta <- function(eta) TRUE
  validmu <- family$validmu
  if (is.null(validmu))
    validmu <- function(mu) TRUE
  if (is.null(mustart)) {
    eval(family$initialize)
  }
  else {
    mukeep <- mustart
    eval(family$initialize)
    mustart <- mukeep
  }
  if (EMPTY) {
    eta <- rep.int(0, nobs) + offset
    if (!valideta(eta))
      stop("invalid linear predictor values in empty model")
    mu <- linkinv(eta)
    if (!validmu(mu))
      stop("invalid fitted means in empty model")
    dev <- sum(dev.resids(y, mu, weights))
    w <- ((weights * mu.eta(eta)^2)/variance(mu))^0.5
    residuals <- (y - mu)/mu.eta(eta)
    good <- rep(TRUE, length(residuals))
    boundary <- conv <- TRUE
    coef <- numeric(0)
    iter <- 0
  }
  else {
    coefold <- NULL
    if (!is.null(etastart)) {
      eta <- etastart
    }
    else if (!is.null(start)) {
      if (length(start) != nvars)
        stop(gettextf("length of 'start' should equal %d and correspond to initial coefs for %s",
                      nvars, paste(deparse(xnames), collapse = ", ")),
             domain = NA)
      else {
        eta <- offset + as.vector(if (NCOL(x) == 1)
          x * start
          else x %*% start)
        coefold <- start
      }
    }
    else {
      eta <- family$linkfun(mustart)
    }
    mu <- linkinv(eta)
    if (!(validmu(mu) && valideta(eta)))
      stop("cannot find valid starting values: please specify some")
    devold <- sum(dev.resids(y, mu, weights))
    boundary <- conv <- FALSE


    if (!is.null(start)){
      coefs.hat <- start
      names(coefs.hat) <- names(prior.mean)
    }
    else coefs.hat <- prior.mean
    dispersionold <- dispersion
    for (iter in 1:control$maxit) {

      good <- weights > 0
      varmu <- variance(mu)[good]
      varmu <- ifelse(varmu == 0, 1e-04, varmu)
      if (any(is.na(varmu)))
        stop("NAs in V(mu)")
      if (any(varmu == 0))
        stop("0s in V(mu)")
      mu.eta.val <- mu.eta(eta)
      mu.eta.val <- ifelse(mu.eta.val == 0, 1e-04, mu.eta.val)
      if (any(is.na(mu.eta.val[good])))
        stop("NAs in d(mu)/d(eta)")
      good <- (weights > 0) & (mu.eta.val != 0)
      if (all(!good)) {
        conv <- FALSE
        warning("no observations informative at iteration ",
                iter)
        break
      }
      z <- (eta - offset)[good] + (y - mu)[good]/mu.eta.val[good]
      w <- sqrt((weights[good] * mu.eta.val[good]^2)/varmu[good])
      ngoodobs <- as.integer(nobs - sum(!good))

      w <- ifelse(w == 0, 1e-04, w) # I add

      if (iter > 1) {
        beta0 <- (coefs.hat - prior.mean)/sqrt(dispersion)

        if (prior == "mde" | prior == "mt") {
          out <- update.scale.p.group(prior=prior, df=prior.df[gvars], b0=beta0[gvars], ss=ss, theta=theta,
                                      group.vars = group.vars)
          prior.scale[gvars] <- out[[1]]
          p <- out[[2]]
          if (!is.matrix(group))
            theta <- update.ptheta.group(group.vars=group.vars, p=p, w=theta.weights, b=b)
          else theta <- update.ptheta.network(theta=theta, p=p, w=group)

          if (!is.null(inter.hierarchy))
            theta.weights <- update.theta.weights(gvars=gvars,
                                                  theta.weights=theta.weights,
                                                  inter.hierarchy=inter.hierarchy,
                                                  inter.parents=inter.parents,
                                                  p=p)
        }

        prior.sd <- update.prior.sd(prior=prior, beta0=beta0, prior.scale=prior.scale,
                                    prior.df=prior.df, sd.x=sd.x, min.x.sd=min.x.sd)
      }


      if (method.coef == "joint") {
        z.star <- c(z, prior.mean)
        x.prior <- diag(NCOL(x))
        colnames(x.prior) <- colnames(x)
        x.star <- rbind(x, x.prior)
        w.star <- c(w, 1/(prior.sd + 1e-04) )
        #            good.star <- c(good, rep(TRUE, NCOL(x.prior)))
        #            fit <- qr(x.star[good.star, , drop = FALSE] * w.star, tol = min(1e-07, control$epsilon/1000))
        fit <- qr(x.star * w.star, tol = min(1e-07, control$epsilon/1000))
        fit$coefficients <- qr.coef(fit, z.star * w.star)
        coefs.hat <- fit$coefficients
        if (any(!is.finite(fit$coefficients))) {
          conv <- FALSE
          warning("non-finite coefficients at iteration ", iter)
          break
        }
      }

      if (method.coef != "joint") {
        for (j in 1:length(group0)) {
          vars <- c(covars0, group0[[j]])
          if (iter <= 5 | any((abs(coefs.hat[vars] - prior.mean[vars])) > 1e-03)) {
            if (iter > 5) vars <- vars[abs(coefs.hat[vars] - prior.mean[vars]) > 1e-03]
            x0 <- x[, vars, drop = FALSE]
            eta0 <- x0 %*% coefs.hat[vars]
            z0 <- z - (eta - eta0) + offset
            z0.star <- c(z0, prior.mean[vars])
            x0.prior <- diag(NCOL(x0))
            colnames(x0.prior) <- vars
            x0.star <- rbind(x0, x0.prior)
            w0.star <- c(w, 1/(prior.sd[vars] + 1e-04))
            #                 good.star <- c(good, rep(TRUE, NCOL(x0.prior)))
            #                 fit <- qr(x0.star[good.star, , drop = FALSE] * w0.star, tol = min(1e-07, control$epsilon/1000))
            fit <- qr(x0.star * w0.star, tol = min(1e-07, control$epsilon/1000))
            coefs.hat[vars] <- qr.coef(fit, z0.star * w0.star)
            eta <- eta - eta0 + x0 %*% coefs.hat[vars]
          }
        }
        fit$coefficients <- coefs.hat
      }

      #            start[fit$pivot] <- fit$coefficients
      start <- fit$coefficients
      eta <- drop(x %*% start)
      mu <- linkinv(eta <- eta + offset)
      dev <- sum(dev.resids(y, mu, weights))
      if (control$trace)
        cat("Deviance =", dev, "Iterations -", iter,
            "\n")
      boundary <- FALSE
      if (!is.finite(dev)) {
        if (is.null(coefold))
          stop("no valid set of coefficients has been found: please supply starting values",
               call. = FALSE)
        warning("step size truncated due to divergence",
                call. = FALSE)
        ii <- 1
        while (!is.finite(dev)) {
          if (ii > control$maxit)
            stop("inner loop 1; cannot correct step size")
          ii <- ii + 1
          start <- (start + coefold)/2
          eta <- drop(x %*% start)
          mu <- linkinv(eta <- eta + offset)
          dev <- sum(dev.resids(y, mu, weights))
        }
        boundary <- TRUE
        if (control$trace)
          cat("Step halved: new deviance =", dev, "\n")
      }
      if (!(valideta(eta) && validmu(mu))) {
        if (is.null(coefold))
          stop("no valid set of coefficients has been found: please supply starting values",
               call. = FALSE)
        warning("step size truncated: out of bounds",
                call. = FALSE)
        ii <- 1
        while (!(valideta(eta) && validmu(mu))) {
          if (ii > control$maxit)
            stop("inner loop 2; cannot correct step size")
          ii <- ii + 1
          start <- (start + coefold)/2
          eta <- drop(x %*% start)
          mu <- linkinv(eta <- eta + offset)
        }
        boundary <- TRUE
        dev <- sum(dev.resids(y, mu, weights))
        if (control$trace)
          cat("Step halved: new deviance =", dev, "\n")
      }

      if ( !(family[[1]] %in% c("binomial", "poisson")) & !nb ){
        Sum <- sum((w * (z - (eta - offset)[good]))^2) + sum((coefs.hat - prior.mean)^2/(prior.sd^2 + 1e-04))
        n.df0 <- nobs
        n.df <- n.df0 - length(coefs.hat[prior.sd >= 1e+04])
        if (n.df <= 0) n.df <- n.df0
        dispersion <- Sum/n.df
      }
      dispersion <- ifelse(dispersion > 1e+04,1e+04, dispersion)
      dispersion <- ifelse(dispersion < 1e-04, 1e-04, dispersion)

      if(nb)  # for negative binomial model
      {
        th <- suppressWarnings( theta.ml(y=y, mu=mu, n=sum(weights), weights=weights, limit=10, trace=FALSE) )
        if (is.null(th)) th <- family$theta
        family <- NegBin(theta = th)

        variance <- family$variance
        dev.resids <- family$dev.resids
        aic <- family$aic
        linkinv <- family$linkinv
        mu.eta <- family$mu.eta
        valideta <- family$valideta
        if (is.null(valideta))
          valideta <- function(eta) TRUE
        validmu <- family$validmu
        if (is.null(validmu))
          validmu <- function(mu) TRUE
      }

      if (iter > 2 & abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon) {
        conv <- TRUE
        coef <- start
        break
      }
      else {
        devold <- dev
        dispersionold <- dispersion
        coef <- coefold <- start
      }

    }  # iter end

    nvars <- ncol(fit$qr)

    if (Warning) {
      if (!conv)
        warning("algorithm did not converge", call. = FALSE)
      if (boundary)
        warning("algorithm stopped at boundary value", call. = FALSE)
      eps <- 10 * .Machine$double.eps
      if (family$family == "binomial") {
        if (any(mu > 1 - eps) || any(mu < eps))
          warning("fitted probabilities numerically 0 or 1 occurred", call. = FALSE)
      }
      if (family$family == "poisson" | nb) {
        if (any(mu < eps))
          warning("fitted rates numerically 0 occurred", call. = FALSE)
      }
    }
    if (fit$rank < nvars)
      coef[fit$pivot][seq(fit$rank + 1, nvars)] <- NA
    xxnames <- xnames[fit$pivot]
    residuals <- rep.int(NA, nobs)
    residuals[good] <- z - (eta - offset)[good]
    fit$qr <- as.matrix(fit$qr)
    nr <- min(sum(good), nvars)
    if (nr < nvars) {
      Rmat <- diag(nvars)
      Rmat[1:nr, 1:nvars] <- fit$qr[1:nr, 1:nvars]
    }
    else Rmat <- fit$qr[1:nvars, 1:nvars]
    Rmat <- as.matrix(Rmat)
    Rmat[row(Rmat) > col(Rmat)] <- 0
    names(coef) <- xnames
    colnames(fit$qr) <- xxnames
    dimnames(Rmat) <- list(xxnames, xxnames)
  } # end
  names(residuals) <- ynames
  names(mu) <- ynames
  names(eta) <- ynames
  wt <- rep.int(0, nobs)
  wt[good] <- w^2
  names(wt) <- ynames
  names(weights) <- ynames
  names(y) <- ynames
  wtdmu <- if (intercept)
    sum(weights * y)/sum(weights)
  else linkinv(offset)
  nulldev <- sum(dev.resids(y, wtdmu, weights))
  n.ok <- nobs - sum(weights == 0)
  nulldf <- n.ok
  if (all(prior.sd >= 1e+04)) nulldf <- n.ok - as.integer(intercept)
  rank <- if (EMPTY)
    0
  else fit$rank
  if (method.coef != "joint") rank <- ncol(x)
  resdf <- n.ok
  if (all(prior.sd >= 1e+04)) resdf <- n.ok - rank
  aic.model <- aic(y, n, mu, weights, dev) + 2 * rank

  loglik <- -(aic.model - 2 * rank)/2

  if (intercept) {
    prior.mean <- prior.mean[-1]
    prior.scale <- prior.scale[-1]
    prior.df <- prior.df[-1]
  }

  out <- list(coefficients = coef, residuals = residuals, fitted.values = mu,
              effects = if (!EMPTY) fit$effects, R = if (!EMPTY) Rmat,
              rank = rank, qr = if (!EMPTY) structure(fit[c("qr", "rank", "qraux", "pivot")], class = "qr"),
              linear.predictors = eta, deviance = dev, aic = aic.model, loglik = loglik,
              null.deviance = nulldev, iter = iter, weights = wt, prior.weights = weights,
              df.residual = resdf, df.null = nulldf, y = y, z = z, converged = conv, boundary = boundary,
              intercept = intercept,
              prior.sd = prior.sd, dispersion = dispersion, group = group, group.vars = group.vars,
              ungroup.vars = ungroup.vars, method.coef = method.coef, family = family )

  if (prior == "t")
    out$prior <- list(prior=prior, mean=prior.mean, scale=prior.scale, df=prior.df)
  if (prior == "de")
    out$prior <- list(prior=prior, mean=prior.mean, scale=prior.scale)
  if (prior == "mde" | prior == "mt") {
    out$prior.scale <- prior.scale
    out$p <- p
    out$ptheta <- theta
    if (prior == "mde")
      out$prior <- list(prior=prior, mean=prior.mean, s0=ss[1], s1=ss[2], b=b)
    if (prior == "mt")
      out$prior <- list(prior=prior, mean=prior.mean, s0=ss[1], s1=ss[2], df=prior.df, b=b)
    out$theta.weights <- theta.weights
  }
  if (nb){
    out$theta <- as.vector(th)
    out$SE.theta <- attr(th, "SE")
    out$twologlik <- 2 * loglik
  }

  return(out)
}


update.scale.p.group <- function(prior="mde", df=1, b0, ss, theta, group.vars)
{
  if (prior == "mde"){
    den0 <- (2 * ss[1])^(-1) * exp(-abs(b0)/ss[1]) # de density
    den1 <- (2 * ss[2])^(-1) * exp(-abs(b0)/ss[2])
  }
  if (prior == "mt"){
    den0 <- (ss[1])^(-1) * (1 + b0^2/(df * ss[1]^2))^(-(df + 1)/2) # t density
    den1 <- (ss[2])^(-1) * (1 + b0^2/(df * ss[2]^2))^(-(df + 1)/2)
  }

  p <- rep(NA, length(b0))
  names(p) <- names(b0)

  for(j in 1:length(group.vars)){
    # browser()
    vars <- group.vars[[j]]
    if(length(unique(theta[vars]))!=1)
      stop("Posterior Probability of indicator for a group should be the same")
    tmp_theta <- unique(theta[vars])

    #TODO(boyiguo1): Isolate linear and non-linear using name
    vars.lnr <- grep(pattern = ".null\\d+$", x = vars, value = TRUE)
    vars.non.lnr <- grep(pattern = ".pen\\d+$", x = vars, value = TRUE)

    # TODO(boyiguo1): Treating covariates as lnr terms
    if(length(vars.lnr) + length(vars.non.lnr) == 0)
      vars.lnr <- vars

    if(length(vars.lnr) + length(vars.non.lnr) != length(vars) | length(vars.lnr) < 1)
      stop("Not updating all components when updating p and scale")

    #TODO(boyiguo1): Check how many linear cofficients: 1) one, update p 2) more than 1, stop ("not implemented)
    if(length(vars.lnr) == 1){
      p[vars.lnr] <-  tmp_theta * prod(den1[vars.lnr]) / (tmp_theta * prod(den1[vars.lnr]) + (1 - tmp_theta) * prod(den0[vars.lnr]) + 1e-10)
    } else
      stop("More than one cofficients for linear parts. Not implemented yet.")

    if(length(vars.non.lnr)!=0){ #TODO(boyiguo1): Update p for the remaining part.
      p[vars.non.lnr] <- (tmp_theta^2 * prod(den1[vars.non.lnr])) / (tmp_theta^2 * prod(den1[vars.non.lnr]) + ((1 - tmp_theta^2) * prod(den0[vars.non.lnr])) + 1e-10)
    }
  }

  if(any(is.na(p)))
    stop("Failed to update p. NA values exist among p")

  scale <- 1/((1 - p)/ss[1] + p/ss[2] + 1e-10)

  list(scale=scale, p=p)
}
