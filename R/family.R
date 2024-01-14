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



# from MASS
#' @export
NegBin <- function (theta=3, link="log")
{
  linktemp <- substitute(link)
  if (!is.character(linktemp))
    linktemp <- deparse(linktemp)
  if (linktemp %in% c("log", "identity", "sqrt"))
    stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  }
  else {
    if (inherits(link, "link-glm")) {
      stats <- link
      if (!is.null(stats$name))
        linktemp <- stats$name
    }
    else stop(gettextf("\"%s\" link not available for negative binomial family; available links are \"identity\", \"log\" and \"sqrt\"",
                       linktemp))
  }
  .Theta <- theta
  env <- new.env(parent = .GlobalEnv)
  assign(".Theta", theta, envir = env)
  variance <- function(mu) mu + mu^2/.Theta
  validmu <- function(mu) all(mu > 0)
  dev.resids <- function(y, mu, wt) 2 * wt * (y * log(pmax(1,
                                                           y)/mu) - (y + .Theta) * log((y + .Theta)/(mu + .Theta)))
  aic <- function(y, n, mu, wt, dev) {
    term <- (y + .Theta) * log(mu + .Theta) - y * log(mu) +
      lgamma(y + 1) - .Theta * log(.Theta) + lgamma(.Theta) -
      lgamma(.Theta + y)
    2 * sum(term * wt)
  }
  initialize <- expression({
    if (any(y < 0)) stop("negative values not allowed for the negative binomial family")
    n <- rep(1, nobs)
    mustart <- y + (y == 0)/6
  })
  simfun <- function(object, nsim) {
    ftd <- fitted(object)
    MASS::rnegbin(nsim * length(ftd), ftd, .Theta)
  }
  environment(variance) <- environment(validmu) <- environment(dev.resids) <- environment(aic) <- environment(simfun) <- env
  famname <- paste("NegBin(", format(round(theta,
                                           4)), ")", sep = "")
  structure(list(family = famname, link = linktemp, linkfun = stats$linkfun,
                 linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids,
                 aic = aic, mu.eta = stats$mu.eta, initialize = initialize,
                 validmu = validmu, valideta = stats$valideta, simulate = simfun, theta = .Theta),
            class = "family")
}

#' @export
Student <- function(mean=0, scale=0.5, df=1, autoscale=TRUE)
{
  if (any(scale < 0)) stop("'scale' cannot be negative")
  if (any(df < 0)) stop("'df' cannot be negative")
  list(prior="t", mean=mean, scale=scale, df=df, autoscale=autoscale)
}

#' @export
De <- function(mean=0, scale=0.5, autoscale=TRUE)
{
  if (any(scale < 0)) stop("'scale' cannot be negative")
  list(prior="de", mean=mean, scale=scale, autoscale=autoscale)
}

#' @export
mde <- function(mean=0, s0=0.04, s1=0.5, b=1)
{
  if (s0 < 0 | s1 < 0) stop("scale cannot be negative")
  if (s0 > s1) stop("s0 should be smaller than s1")
  list(prior="mde", mean=mean, ss=c(s0,s1), b=b)
}

#' @export
mt <- function(mean=0, s0=0.04, s1=0.5, df=1, b=1)
{
  if (s0 < 0 | s1 < 0) stop("scale cannot be negative")
  if (s0 > s1) stop("s0 should be smaller than s1")
  if (any(df < 0)) stop("'df' cannot be negative")
  list(prior="mt", mean=mean, ss=c(s0,s1), df=df, b=b)
}
