#' Plot splines according to mgcv pre-fit object
#'
#' @param term_index numeric vector, the indices of spline terms what to plot 
#' @param G The mgcv pre-fit object that contains smooth spline information
#' @param coefs Coefficients from the model
#' @param top textGrob, the title of the plot
#'
#' @return
#' @export
#'
#' @examples
#' N <- 1000
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
#' bgam_mdl <- bglm_spline(G$y~G$X-1, family = G$family, prior=mt(mean=0, s0=0.04, s1=1, df=Inf),
#'                         group = create_group(G))
#' 
#' plot_smooth_terms(1:4, G, coefficients(bgam_mdl),
#'                        top = textGrob("bgam with mgcv",gp=gpar(fontsize=20,font=3)) )

plot_smooth_terms <- function(term_index, G, coefs, has_intercept = F, top = NULL){
  
  if(!all(term_index %in% 1:length(G$smooth)))
    stop("Element of term_index is out of scope of number of splines in mgcv pre-fit object G.")
  
  g_list <- lapply(term_index, FUN = function(i){
    coef_ind <- col_ind <- G$smooth[[i]]$first.para:G$smooth[[i]]$last.para
    if(has_intercept) coef_ind <- coef_ind + 1
    lp <- G$X[, col_ind, drop=FALSE] %*% coefs[coef_ind]
    
    ggplot(data.frame(x=G$mf[,G$smooth[[i]]$term],y = G$family$linkinv(lp)) )+ 
      geom_point(aes(x,y)) + ylab("Response")+
      xlab(G$smooth[[i]]$label)+
      ylim(0,1)
    
  })
  grid.arrange(grobs = g_list, ncol=2,
               top = top)
}