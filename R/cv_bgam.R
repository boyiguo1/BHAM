#' Title
#'
#' @param object
#' @param nfolds
#' @param foldid
#' @param ncv
#' @param s0
#' @param verbose
#'
#' @return
#' @export
#'
#'
#' @examples
#'
tune.bgam <- function(object, nfolds=10, foldid=NULL, ncv=1, s0 = NULL, verbose=TRUE){

  if(any(class(object) %in% "glm")){
    data.obj <- model.frame(object)
    y.obj <- model.response(data.obj)
  } else if(any(class(object) %in% "bmlasso")){
    y.obj <- object$y
  }  else if(any(class(object) %in% "coxph")){
    y.obj <- model.response(model.frame(object))
  } else
    stop("Does not support")


  n <- NROW(y.obj)
  fol <- generate.foldid(nobs=n, nfolds=nfolds, foldid=foldid, ncv=ncv)


  map_dfr(s0, .f = function(.s0, .mdl, .foldid){

    tmp <- call("cv.bgam", .mdl, s0 = .s0, foldid = .foldid) %>% eval
    if(ncol(.foldid)==1)
      tmp$measures %>% t() %>% data.frame
    else {
      mean_row <- tmp$measures[1,]
      names(mean_row) <- paste0(names(mean_row), "_mean")
      sd_row <- tmp$measures[2,]
      names(sd_row) <- paste0(names(sd_row), "_sd")
      data.frame(c(mean_row, sd_row) %>% t)
    }

  },
  .mdl = object, .foldid = fol$foldid) %>%
    data.frame(s0 = s0,.)

}








#' Cross-validation & Tuning for `bgam` and `bamlasso`
#'
#' @param object
#' @param nfolds
#' @param foldid
#' @param ncv
#' @param s0
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#'
cv.bgam <- function(object, nfolds=10, foldid=NULL, ncv=1, s0 = NULL, verbose=TRUE)
{
  start.time <- Sys.time()
  # browser()
  .group <- object$group
  if (!"gam" %in% class(object))
  {
    if (any(class(object) %in% "glm"))
      out <- cv.gam.glm(object=object, nfolds=nfolds, foldid=foldid,
                        ncv=ncv, s0 = s0, group = .group, verbose=verbose)

    if (any(class(object) %in% "coxph"))
      out <- cv.gam.coxph(object=object, nfolds=nfolds, foldid=foldid, ncv=ncv, s0 = s0, group = .group, verbose=verbose)

    if (any(class(object) %in% "glmNet") | any(class(object) %in% "bmlasso"))
      out <- cv.gam.lasso(object=object, nfolds=nfolds, foldid=foldid,
                          ncv=ncv, s0 = s0, group = .group, verbose=verbose)

    if (any(class(object) %in% "polr"))
      stop("Not implemented yet")
      # out <- cv.gam.polr(object=object, nfolds=nfolds, foldid=foldid, ncv=ncv, s0 = s0, verbose=verbose)
  }
  else
  {
    fam <- object$family$family
    if (substr(fam, 1, 17) == "Negative Binomial") fam <- "NegBin"
    gam.fam <- c("gaussian", "binomial", "poisson", "quasibinomial", "quasipoisson", "NegBin",
                 "Cox PH")
    if (! fam %in% gam.fam)
      stop("Cross-validation for this family has not been implemented yet")
    if (fam %in% gam.fam[1:6]){
      out <- cv.gam.glm(object=object, nfolds=nfolds, foldid=foldid,
                        ncv=ncv, s0 = s0, group = .group, verbose=verbose)
    }

    if (fam == "coxph")
      out <- cv.gam.coxph(object=object, nfolds=nfolds, foldid=foldid, ncv=ncv, s0 = s0, group = .group, verbose=verbose)
  }

  stop.time <- Sys.time()
  Time <- round(difftime(stop.time, start.time, units = "min"), 3)
  if(verbose)
    cat("\n Cross-validation time:", Time, "minutes \n")

  out
}


#' Title
#'
#' @param nobs
#' @param nfolds
#' @param foldid
#' @param ncv
#'
#' @return
#'
#' @examples
generate.foldid <- function(nobs, nfolds=10, foldid=NULL, ncv=1)
{
  if (nfolds > nobs) nfolds <- nobs
  if (nfolds == nobs) ncv <- 1
  if (is.null(foldid)) {
    foldid <- array(NA, c(nobs, ncv))
    for(j in 1:ncv)
      foldid[, j] <- sample(rep(seq(nfolds), length=nobs))
  }
  foldid <- as.matrix(foldid)
  nfolds <- max(foldid)
  ncv <- ncol(foldid)

  list(foldid=foldid, nfolds=nfolds, ncv=ncv)
}



#' Internal function for cross validation of bgam model
#'
#' @param object
#' @param nfolds
#' @param foldid
#' @param ncv
#' @param s0
#' @param group
#' @param verbose
#'
#' @return
#'
#' @examples
cv.gam.glm <- function(object, nfolds=10, foldid=NULL, ncv=1,  s0 = NULL, group = group, verbose=TRUE)
{
  data.obj <- model.frame(object)
  y.obj <- model.response(data.obj)
  n <- NROW(y.obj)
  fol <- generate.foldid(nobs=n, nfolds=nfolds, foldid=foldid, ncv=ncv)
  foldid <- fol$foldid
  nfolds <- fol$nfolds
  ncv <- fol$ncv
  measures0 <- lp0 <- y.fitted0 <- NULL
  j <- 0
  if (!is.null(object$offset)) {
    data.obj <- object$data
    if (is.null(object$data)) stop("'data' not given in object")
  }


  prior <- object$prior
  if(is.null(s0)) prior <- object$prior
  else if(! prior$prior %in% c("mde", "mt")){
    cat(prior$prior, "\n")
    stop("Does not support the prior family")
  } else if(prior$prior ==  "mde") {
    prior <- call("mde", s0 = s0) %>% eval
  } else{
    prior <- call("mt", df = prior$df, s0 = s0) %>% eval
  }


  if (verbose) cat("Fitting", "ncv*nfolds =", ncv*nfolds, "models: \n")
  for (k in 1:ncv) {
    y.fitted <- lp <- rep(NA, n)
    deviance <- NULL

    for (i in 1:nfolds) {
      subset1 <- rep(TRUE, n)
      omit <- which(foldid[, k] == i)
      subset1[omit] <- FALSE
      fit <- update(object, data = object$data[subset1, ,drop = FALSE], #subset = subset1,
                    prior = prior, group = group)

      lp[omit] <- predict(fit, newdata=data.obj[omit, , drop=FALSE])
      y.fitted[omit] <- object$family$linkinv(lp[omit])
      if (any(class(object) %in% "negbin")) fit$dispersion <- fit$theta
      dd <- suppressWarnings( measure.glm(y.obj[omit], y.fitted[omit], family=object$family$family, dispersion=fit$dispersion) )
      deviance <- c(deviance, dd["deviance"])

      if (verbose) {
        j <- j + 1
        cat(j, "")
      }
    }

    measures <- measure.glm(y.obj, y.fitted, family=object$family$family)
    measures["deviance"] <- sum(deviance)

    measures0 <- rbind(measures0, measures)
    lp0 <- cbind(lp0, lp)
    y.fitted0 <- cbind(y.fitted0, y.fitted)

  }
  #
  out <- list()
  if (nrow(measures0) == 1) out$measures <- colMeans(measures0, na.rm = TRUE)
  else {
    out$measures <- rbind(colMeans(measures0, na.rm = TRUE), apply(measures0, 2, sd, na.rm = TRUE))
    rownames(out$measures) <- c("mean", "sd")
  }
  out$measures <- round(out$measures, digits=3)
  out$y.obs <- y.obj
  out$lp <- lp0
  out$y.fitted <- y.fitted0
  out$foldid <- foldid

  out
}



#' Title
#'
#' @param object
#' @param nfolds
#' @param foldid
#' @param ncv
#' @param s0
#' @param group
#' @param verbose
#'
#' @return
#'
#' @examples
cv.gam.lasso <- function(object, nfolds=10, foldid=NULL, ncv=1,  s0 = NULL, group = group, verbose=TRUE)
{
  family <- object$family
  x.obj <- object$x
  y.obj <- object$y
  n <- NROW(y.obj)
  offset <- object$offset
  init <- object$coefficients
  init <- init[!names(init)%in%"(Intercept)"]

  fol <- generate.foldid(nobs=n, nfolds=nfolds, foldid=foldid, ncv=ncv)
  foldid <- fol$foldid
  nfolds <- fol$nfolds
  ncv <- fol$ncv
  measures0 <- lp0 <- y.fitted0 <- NULL
  j <- 0

  #

  if(is.null(s0)) ss <- object$ss
  # else if (is.list(prior)) {
  #   ss <- prior$ss
  else ss <- c(s0, object$ss[2])

  if (verbose) cat("Fitting", "ncv*nfolds =", ncv*nfolds, "models: \n")
  for (k in 1:ncv) {
    y.fitted <- lp <- rep(NA, n)
    deviance <- NULL

    for (i in 1:nfolds) {
      subset1 <- rep(TRUE, n)
      omit <- which(foldid[, k] == i)
      subset1[omit] <- FALSE
      if (any(class(object) %in% "glmNet"))
        stop("Wrong class. This function doesn't surpport `glmNet`")
        # fit <- update(object, x=x.obj[-omit, ], y=y.obj[-omit], offset=offset[-omit],
        #               lambda=object$lambda, verbose=FALSE)
      if (any(class(object) %in% "bmlasso"))
        fit <- update(object, x=x.obj[-omit, ], y=y.obj[-omit], offset=offset[-omit],
                      # init=init,
                      verbose=FALSE, ss = ss, group = group)
      if (is.null(fit$offset)) fit$offset <- FALSE
      else fit$offset <- TRUE
      xx <- x.obj[omit, , drop=FALSE]
      off <- offset[omit]
      lp[omit] <- as.vector(predict(fit, newx=xx, newoffset=off))
      if (any(class(object) %in% "GLM")) {
        y.fitted[omit] <- as.vector(predict(fit, newx=xx, type="response", newoffset=off))
        dd <- suppressWarnings( measure.glm(y.obj[omit], y.fitted[omit], family=family, dispersion=fit$dispersion) )
        deviance <- c(deviance, dd["deviance"])
      }

      if (verbose) {
        j <- j + 1
        cat(j, "")
      }
    }

    if (any(class(object) %in% "GLM")) {
      measures <- measure.glm(y.obj, y.fitted, family=family)
      measures["deviance"] <- sum(deviance)
      y.fitted0 <- cbind(y.fitted0, y.fitted)
    }
    if (any(class(object) %in% "COXPH"))
      # stop("not implmented yet")
      measures <- measure.cox(y.obj, lp)

    measures0 <- rbind(measures0, measures)
    lp0 <- cbind(lp0, lp)
  }

  out <- list()
  if (nrow(measures0) == 1) out$measures <- colMeans(measures0, na.rm = TRUE)
  else {
    out$measures <- rbind(colMeans(measures0, na.rm = TRUE), apply(measures0, 2, sd, na.rm = TRUE))
    rownames(out$measures) <- c("mean", "sd")
  }
  out$measures <- round(out$measures, digits=3)
  out$y.obs <- y.obj
  out$lp <- lp0
  if (any(class(object) %in% "GLM")) out$y.fitted <- y.fitted0
  out$foldid <- foldid

  out
}



cv.gam.coxph <- function(object, nfolds=10, foldid=NULL, ncv=1,  s0 = NULL, group = group, verbose=TRUE)
{
  # browser()s
  data.obj <- model.frame(object)
  # data.obj <- data.obj %>% select(-`Surv(time, status)`)
  y.obj <- model.response(data.obj)
  data.obj <- data.obj %>% select(-starts_with("Surv(")) %>%
    cbind(data.matrix(y.obj), .)
  n <- NROW(y.obj)

  fol <- generate.foldid(nobs=n, nfolds=nfolds, foldid=foldid, ncv=ncv)
  foldid <- fol$foldid
  nfolds <- fol$nfolds
  ncv <- fol$ncv
  measures0 <- lp0 <- NULL
  j <- 0
  # browser()
  if (!is.null(object$offset) && any(object$offset!=0)) {
    data.obj <- object$data
    if (is.null(object$data)) stop("'data' not given in object")
  }

  prior <- object$prior
  if(is.null(s0)) prior <- object$prior
  else if(! prior$prior %in% c("mde", "mt")){
    cat(prior$prior, "\n")
    stop("Curernt function oes not support this prior family")
  } else if(prior$prior ==  "mde") {
    prior <- call("mde", s0 = s0) %>% eval
  } else{
    prior <- call("mt", df = prior$df, s0 = s0) %>% eval
  }

  if (verbose) cat("Fitting", "ncv*nfolds =", ncv*nfolds, "models: \n")
  for (k in 1:ncv) {  # start outer for loop
    lp <- rep(NA, n)

    for (i in 1:nfolds) { # Start inner for loop
      # subset1 <- rep(TRUE, n)
      omit <- which(foldid[, k] == i)
      # subset1[omit] <- FALSE
      # TODO: need add other arguments
      # browser()

      fit <- update(object, data=data.obj[-omit,], #x=data.obj[-omit, ], y=y.obj[-omit], #offset=offset[-omit],
                    # init=init,
                    prior = prior, group = group,
                    verbose=FALSE)
      lp[omit] <- predict(fit, newdata=data.obj[omit, , drop=FALSE])

      if (verbose) {
        j <- j + 1
        cat(j, "")
      }
    } # End inner for loop

    measures <- measure.cox(y.obj, lp)
    measures0 <- rbind(measures0, measures)
    lp0 <- cbind(lp0, lp)
  }   # End outer for loop


  # Prepare output
  out <- list()
  if (nrow(measures0) == 1) out$measures <- colMeans(measures0, na.rm = TRUE)
  else {
    out$measures <- rbind(colMeans(measures0, na.rm = TRUE), apply(measures0, 2, sd, na.rm = TRUE))
    rownames(out$measures) <- c("mean", "sd")
  }
  out$measures <- round(out$measures, digits=3)
  out$y.obs <- y.obj
  out$lp <- lp0
  out$foldid <- foldid

  out
}
