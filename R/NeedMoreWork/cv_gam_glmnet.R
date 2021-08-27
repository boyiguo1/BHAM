cv.gam <- function (x, y, #weights = NULL,
                    offset = NULL,
                    # lambda = NULL,
                    s0 = NULL,
                    s1 = 0.5,
                    type.measure = c("default", "mse", "deviance",
                                     "class", "auc", "mae", "C"),
                    nfolds = 10, foldid = NULL,
                    # alignment = c("lambda","fraction"),
                    grouped = TRUE, keep = FALSE, parallel = FALSE,
                    # gamma = c(0, 0.25, 0.5, 0.75, 1),
                    # relax = FALSE,
                    trace.it = 0,
                    ...)
{
  type.measure = match.arg(type.measure)
  # alignment = match.arg(alignment)
  if (!is.null(s0) && length(s0) < 2)
    stop("Need more than one value of lambda for cv.glmnet")
  # if (!is.null(lambda) && alignment == "fraction") {
  #   warning("fraction of path alignment not available if lambda given as argument; switched to alignment=`lambda`")
  #   alignment = "lambda"
  # }
  N = nrow(x)
  # if (is.null(weights))
  #   weights = rep(1, N)
  # else weights = as.double(weights)
  y = drop(y)
  cv.call = gam.call = match.call(expand.dots = TRUE)
  which = match(c("type.measure", "nfolds", "foldid",
                  "grouped", "keep"), names(gam.call), FALSE)
  if (any(which))
    gam.call = gam.call[-which]
  gam.call[[1]] = as.name("bamlasso")
  if (glmnet.control()$itrace)
    trace.it = 1
  else {
    if (trace.it) {
      glmnet.control(itrace = 1)
      on.exit(glmnet.control(itrace = 0))
    }
  }
  if (is.null(foldid))
    foldid = sample(rep(seq(nfolds), length = N))
  else nfolds = max(foldid)
  if (nfolds < 3)
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  # if (relax)
  #   cv.relaxed.raw(x, y, weights, offset, lambda, type.measure,
  #                  nfolds, foldid, alignment, grouped, keep, parallel,
  #                  trace.it, glmnet.call, cv.call, gamma, ...)
  # else

  if(is.null(s0)) {
    s0 = seq(0.005, 0.1, 0.01)
  }

  cv.gam.raw(x, y, # weights,
             offset, s0, s1,#lambda,
             type.measure, nfolds, foldid, #alignment,
             grouped, keep, parallel, trace.it,
             gam.call, cv.call, ...)
}




# cv.gam.raw --------------------------------------------------------------

cv.gam.raw <- function (x, y, #weights,
                        offset, s0, s1, #lambda,
                        type.measure, nfolds, foldid,
                        grouped, keep, parallel, trace.it,
                        gam.call, cv.call, ...)
{
  if (trace.it)
    cat("Training\n")
  bgam.object = lapply(
    s0, FUN = function(.s0, .s1, ...){
      bamlasso(x, y, #weights = weights,
               ss = c(.s0, .s1),
               offset = offset,
               # lambda = lambda,
               verbose = FALSE, ...)
    },
    .s1 = s1, ... = ...
  )

  bgam.object$call = gam.call
  pseudo.obj = bgam.object[[1]]
  subclass = class(pseudo.obj)[[1]]
  type.measure = glmnet:::cvtype(type.measure, subclass)
  is.offset = pseudo.obj$offset
  # if (inherits(pseudo.obj, "multnet") && !pseudo.obj$grouped) {
  #   nz = predict(bgam.object, type = "nonzero")
  #   nz = sapply(nz, function(x) sapply(x, length))
  #   nz = ceiling(apply(nz, 1, median))
  # }
  # else
  nz = sapply(
    bgam.object,
    FUN = function(mdl)
      predict(mdl, type = "nonzero") %>% length
  )
  outlist = as.list(seq(nfolds))
  N = nrow(x)
  if (parallel) {
    outlist = foreach(i = seq(nfolds), .packages = c("BHAM")) %dopar%
      {
        which = foldid == i
        if (length(dim(y)) > 1)
          y_sub = y[!which, ]
        else y_sub = y[!which]
        if (is.offset)
          offset_sub = as.matrix(offset)[!which, ]
        else offset_sub = NULL
        bamlasso(x[!which, , drop = FALSE], y_sub, ss = c(s0[i], s1),
                 offset = offset_sub, #weights = weights[!which],
                 ...)
      }
  }
  else {
    for (i in seq(nfolds)) {
      if (trace.it)
        cat(sprintf("Fold: %d/%d\n", i, nfolds))
      which = foldid == i
      if (is.matrix(y))
        y_sub = y[!which, ]
      else y_sub = y[!which]
      if (is.offset)
        offset_sub = as.matrix(offset)[!which, ]
      else offset_sub = NULL
      outlist[[i]] = glmnet(x[!which, , drop = FALSE],
                            y_sub, lambda = lambda, ss = c(s0[i], s1),
                            weights = weights[!which], trace.it = trace.it,
                            ...)
    }
  }
  lambda = bgam.object$lambda
  class(outlist) = paste0(subclass, "list")
  # predmat = buildPredmat(outlist, lambda, x, offset, foldid,
  #                        alignment, y = y, weights = weights, grouped = grouped,
  #                        type.measure = type.measure, family = family(bgam.object))
  fun = paste("cv", subclass, sep = ".")
  cvstuff = do.call(fun, list(predmat, y, type.measure, weights,
                              foldid, grouped))
  grouped = cvstuff$grouped
  if ((N/nfolds < 3) && grouped) {
    warning("Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold",
            call. = FALSE)
    grouped = FALSE
  }
  out = cvstats(cvstuff, foldid, nfolds, lambda, nz, grouped)
  cvname = names(cvstuff$type.measure)
  names(cvname) = cvstuff$type.measure
  out = c(out, list(call = cv.call, name = cvname, glmnet.fit = bgam.object))
  # if (keep)
  #   out = c(out, list(fit.preval = predmat, foldid = foldid))
  lamin = with(out, getOptcv.glmnet(lambda, cvm, cvsd, cvname))
  obj = c(out, as.list(lamin))
  class(obj) = "cv.gam"
  obj
}

