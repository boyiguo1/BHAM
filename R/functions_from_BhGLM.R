# The following code is copied and adapted from the R package BhGLM
#  (https://github.com/nyiuab/BhGLM)
# Reference:
# Yi, N., Tang, Z., Zhang, X., & Guo, B. (2019). BhGLM: Bayesian hierarchical
# GLMs and survival models, with applications to genomics and epidemiology. Bioinformatics, 35(8), 1419-1421.
#
# Including the MIT Licences of BhGLM
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.



# Functions Start Here ---------------------------------------------------

update.prior.sd <- function (prior, beta0, prior.scale, prior.df, sd.x, min.x.sd)
{
  prior.scale <- prior.scale + 1e-04
  J <- length(beta0)
  if (prior == "t" | prior == "mt")
    prior.sd <- sqrt((beta0^2 + prior.df * prior.scale^2)/(1 + prior.df))
  if (prior == "de" | prior == "mde")     # prior.scale = lamda in Exp(1/(2*lamda^2) )
    prior.sd <- sqrt(abs(beta0) * prior.scale)

  prior.sd <- ifelse(prior.sd > 1e+04, 1e+04, prior.sd)
  prior.sd <- ifelse(prior.sd < 1e-04, 1e-04, prior.sd)
  prior.sd <- ifelse(sd.x < min.x.sd, 1e-04, prior.sd)
  if (names(beta0)[1] == "(Intercept)") prior.sd[1] <- 1e+10
  prior.sd
}

update.ptheta.group <- function(group.vars, p, w, b) # group-specific probability
{
  f <- function(theta, w, p, bb) { # theta ~ beta(1,b)
    sum(p*log(w*theta) + (1-p)*log(1-w*theta)) + mean((bb-1)*log(1-theta))
  }
  theta <- p
  for (j in 1:length(group.vars)) {
    vars <- group.vars[[j]]
    #    theta[vars] <- mean(p[vars])  # posterior mode with theta~beta(1,1)
    theta[vars] <- stats::optimize(f, interval=c(0, 1),
                            w=w[vars], p=p[vars], bb=b[j], maximum=T)$maximum
  }
  theta <- ifelse(theta < 0.01, 0.01, theta)
  theta <- ifelse(theta > 0.99, 0.99, theta)
  theta <- w * theta

  theta
}

update.ptheta.network <- function(theta, p, w)
{
  phi <- 2
  for (j in 1:length(theta)) {
    mu <- w %*% theta
    m <- mu[j] - w[j,j]*theta[j]
    a <- m*phi
    b <- (1-m)*phi
    theta[j] <- (p[j] + a)/(1 + a + b) # posterior mean
  }
  theta <- ifelse(theta < 0.01, 0.01, theta)
  theta <- ifelse(theta > 0.99, 0.99, theta)

  theta
}

update.theta.weights <- function (gvars, theta.weights, inter.hierarchy, inter.parents, p)
{
  if (is.null(inter.parents))
    stop("'inter.parents' should be given")
  if (!is.list(inter.parents))
    stop("'inter.parents' should be a list")
  xnames <- strsplit(gvars, split=":", fixed=T)
  inter <- unlist(lapply(xnames, function(x){length(x)}))
  if (length(inter.parents)!=length(inter[inter==2]))
    stop("interactions are not correctly specified in formula or inter.parents")

  p.main <- p[inter==1]
  if (inter.hierarchy=="strong")
    ww <- lapply(inter.parents,
                 function(x, p.main){ p.main[x[1]] * p.main[x[2]] },
                 p.main)
  if (inter.hierarchy=="weak")
    ww <- lapply(inter.parents,
                 function(x, p.main){ (p.main[x[1]] + p.main[x[2]])/2 },
                 p.main)
  theta.weights[inter==2] <- unlist(ww)
  theta.weights
}

auto_scale <- function(x, min.x.sd=1e-04)
{
  scale <- apply(x, 2, sd)
  scale <- ifelse(scale<=min.x.sd, 1, scale)
  two <- which(apply(x, 2, function(u) {length(unique(u))==2}))
  scale[two] <- apply(x[, two, drop=F], 2, function(u){max(u)-min(u)})
  scale
}

#************************************************************************************

prepare <- function(x, intercept, prior.mean, prior.sd, prior.scale, prior.df, group)
{
  x0 <- x
  if (intercept) x0 <- x[, -1, drop = FALSE]
  g <- Grouping(all.var = colnames(x0), group = group)
  group <- g$group
  group.vars <- g$group.vars
  ungroup.vars <- g$ungroup.vars
  covars <- g$ungroup.vars

  if (is.list(group)) { # for overlap groups
    if (length(unlist(group)) > length(unique(unlist(group)))) {
      x1 <- as.data.frame(x0)
      x1 <- x1[, c(covars, unlist(group))]
      g <- c(length(ungroup.vars), length(ungroup.vars) + cumsum(lapply(group, length)))
      for (j in 1:(length(group)-1))
        group.vars[[j]] <- colnames(x1[, (g[j]+1):g[j+1]])
      x1 <- as.matrix(x1)
      x <- x1
      if (intercept) {
        x <- cbind(1, x)
        colnames(x)[1] <- "(Intercept)"
      }
    }
  }

  J <- NCOL(x)

  if (intercept & J > 1) {
    prior.mean <- c(0, prior.mean)
    prior.scale <- c(prior.scale[1], prior.scale)
    prior.df <- c(prior.df[1], prior.df)
  }

  if (length(prior.mean) < J)
    prior.mean <- c(prior.mean, rep(prior.mean[length(prior.mean)], J - length(prior.mean)) )
  if (length(prior.scale) < J)
    prior.scale <- c(prior.scale, rep(prior.scale[length(prior.scale)], J - length(prior.scale)) )
  if (length(prior.df) < J)
    prior.df <- c(prior.df, rep(prior.df[length(prior.df)], J - length(prior.df)) )
  prior.mean <- prior.mean[1:J]
  prior.scale <- prior.scale[1:J]
  prior.df <- prior.df[1:J]
  prior.df <- ifelse(prior.df==Inf, 1e+10, prior.df)

  if (is.null(prior.sd)) prior.sd <- prior.scale + 0.2   ## + 0.2 to avoid prior.sd=0
  if (length(prior.sd) < J)
    prior.sd <- c(prior.sd, rep(prior.sd[length(prior.sd)], J - length(prior.sd)) )
  prior.sd <- prior.sd[1:J]
  sd.x <- apply(x, 2, sd, na.rm=TRUE)
  min.x.sd <- 1e-04
  prior.sd <- ifelse(sd.x < min.x.sd, 1e-04, prior.sd)
  if (intercept) prior.sd[1] <- 1e+10

  names(prior.mean) <- names(prior.scale) <- names(prior.df) <- names(prior.sd) <- colnames(x)

  if (intercept) covars <- c(colnames(x)[1], covars)
  if (!is.null(covars)) prior.mean[covars] <- 0

  list(x=x, prior.mean=prior.mean, prior.sd=prior.sd, prior.scale=prior.scale, prior.df=prior.df,
       sd.x=sd.x, min.x.sd=min.x.sd,
       group=group, group.vars=group.vars, ungroup.vars=ungroup.vars)
}

Grouping <- function(all.var, group)
{
  n.vars <- length(all.var)
  group.vars <- list()

  if (is.matrix(group))
  {
    if (nrow(group)!=ncol(group) | ncol(group)>n.vars)
      stop("wrong dimension for 'group'")
    if (any(rownames(group)!=colnames(group)))
      stop("rownames should be the same as colnames")
    if (any(!colnames(group)%in%all.var))
      stop("variabe names in 'group' not in the model predictors")
    group.vars <- colnames(group)
    group <- abs(group)
    wcol <- rowSums(group) - diag(group)
    group <- group/wcol
  }
  else{
    if (is.list(group)) group.vars <- group
    else
    {
      if (is.numeric(group) & length(group)>1) {
        group <- sort(group)
        if (group[length(group)] > n.vars) stop("wrong grouping")
      }
      if (is.numeric(group) & length(group)==1)
        group <- as.integer(seq(0, n.vars, length.out = n.vars/group + 1))
      if (is.null(group)) group <- c(0, n.vars)
      group <- unique(group)
      for (j in 1:(length(group) - 1))
        group.vars[[j]] <- all.var[(group[j] + 1):group[j + 1]]
    }
  }
  all.group.vars <- unique(unlist(group.vars))

  if (length(all.group.vars) == n.vars) ungroup.vars <- NULL
  else ungroup.vars <- all.var[which(!all.var %in% all.group.vars)]

  group.new <- c(length(ungroup.vars), length(ungroup.vars) + cumsum(lapply(group.vars, length)))
  var.new <- c(ungroup.vars, unlist(group.vars))

  list(group=group, group.vars=group.vars, ungroup.vars=ungroup.vars,
       group.new=group.new, var.new=var.new)
}

measure.glm <- function(y, y.fitted, family, dispersion = 1)
{
  if (NROW(y)!=NROW(y.fitted))
    stop("y and y.fitted should be of the same length", call. = FALSE)
  if (is.null(dispersion)) dispersion <- 1

  mu <- y.fitted
  if (substr(family, 1, 6)=="NegBin" | substr(family, 1, 17)=="Negative Binomial"
      | family=="nb")
    family <- "NegBin"
  fam <- c("gaussian", "binomial", "poisson", "quasibinomial", "quasipoisson", "NegBin")
  if (! family %in% fam)
    stop("Measures for this family have not been implemented yet")

  if (family=="gaussian")
    logL <- dnorm(y, mean=mu, sd=sqrt(dispersion), log=TRUE)
  if (family=="binomial" | family=="quasibinomial"){
    if (is.factor(y)) y <- as.numeric(y) - 1
    L <- dbinom(y, size=1, prob=mu, log=FALSE)
    L <- ifelse(L==0, 1e-04, L)
    logL <- log(L)
  }
  if (family=="poisson" | family=="quasipoisson")
    logL <- dpois(y, lambda=mu, log=TRUE)
  if (family == "NegBin")
    logL <- dnbinom(y, size=dispersion, mu=mu, log=TRUE)

  logL <- sum(logL, na.rm=TRUE)
  deviance <- -2 * logL

  mse <- mean((y - mu)^2, na.rm = TRUE)
  mae <- mean(abs(y - mu), na.rm = TRUE)
  measures <- list(deviance=deviance, mse=mse, mae=mae)
  if (family=="gaussian") {
    R2 <- (var(y, na.rm = TRUE) - mse)/var(y, na.rm = TRUE)
    measures <- list(deviance=deviance, R2=R2, mse=mse, mae=mae)
  }
  if (family=="binomial" | family=="quasibinomial") {
    # stop("Not implemented")

    # if (!requireNamespace("pROC")) install.packages("pROC")
    # require(pROC)
    if (length(unique(y)) > 1)
      AUC <- suppressMessages( pROC::auc(y, mu) )
    else AUC <- NULL
    AUC <- as.numeric(AUC)
    misclassification <- mean(abs(y - mu) >= 0.5, na.rm = TRUE)
    measures <- list(deviance=deviance, auc=AUC, mse=mse, mae=mae,
                     misclassification=misclassification)
  }

  round(unlist(measures), digits=3)
}

measure.cox <- function(y, lp)
{
  if (NROW(y)!=NROW(lp))
    stop("y and lp should be of the same length", call. = FALSE)
  ff <- bacoxph(y ~ lp, prior=De(1, 0), verbose=FALSE)
  deviance <- -2 * ff$loglik[2]
  cindex <- summary(ff)$concordance[[1]]
  measures <- list(deviance = deviance, Cindex = cindex)
  round(unlist(measures), digits=3)
}



# only used in simulation
link.vars <- function(group.vars) {
  all.group.vars <- unique(unlist(group.vars))
  n.vars <- length(all.group.vars)
  n.groups <- length(group.vars)
  linked.vars <- vector(mode = "list", length = n.vars)
  names(linked.vars) <- all.group.vars
  for (i in 1:n.vars) {
    for (j in 1:n.groups)
      if (all.group.vars[i] %in% group.vars[[j]])
        linked.vars[[i]] <- unique(c(linked.vars[[i]], group.vars[[j]]))
    d <- which(linked.vars[[i]] %in% all.group.vars[i])
    linked.vars[[i]] <- linked.vars[[i]][-d]
  }
  linked.vars
}
