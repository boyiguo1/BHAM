#' Use Wald test to test if the spline is necessary
#'
#' @param G The mgcv pre-fit object that contains smooth spline information
#' @param coefs Coefficients from the model
#' @param cov
#'
#' @return A table of hypothesis testing results
#' @export
#'
#' @examples#' N <- 1000
#' p <- 10
#'
#' set.seed(1)
#'
#' dat <- data.frame(sim_Bai_logistic(N, p)$dat)
#' G <- gam(y ~ s(x1, bs = "ps", k=13)+s(x2, bs = "ps", k=15)+s(x3, bs = "ps", k=15)+s(x4, bs = "ps", k=15)+
#' s(x5, bs = "ps", k=15)+s(x6, bs = "ps", k=15)+s(x7, bs = "ps", k=15)+s(x8, bs = "ps", k=15)+
#'  s(x9, bs = "ps", k=15)+s(x10, bs = "ps", k=15),
#' data=dat %>% data.frame(), family=binomial,drop.intercept = TRUE, fit = FALSE)
#'
#' colnames(G$X) <- G$term.names
#' mdl <- bglm_spline(G$y~G$X-1, family = G$family, prior=mt(mean=0, s0=0.04, s1=1, df=Inf),
#'                         group = create_group(G))
#'
#' test_groups(G, coefficients(mdl), vcov.bh(mdl))

test_spline_terms <- function(G, coef, cov, has_intercept = F){
  n_smooth <- length(G$smooth)

  map_dfr(1:n_smooth, .f = function(i){
    term_name <- G$smooth[[i]]$label
    term_index <- G$smooth[[i]]$first.para:G$smooth[[i]]$last.para

    if(has_intercept) term_index <- term_index +1

    ret <- waldtest.bh(coef, cov, vars=term_index)
    ret$variables <- term_name
    # TODO: re-arrange the output: put the term_name to the first column
    as.data.frame(ret)
  })
