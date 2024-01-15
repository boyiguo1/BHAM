#' Plot non-linear functions for BHAM objects
#'
#' @param mdl a model fitted using bgam, banlasso or bacoxph
#' @param terms a vector of characters containing the variable names
#' @param plot a logic variable, indicating if ggplots are produced
#'
#' @return
#' @export
#'
#' @examples
#' library(glmnet); data("QuickStartExample")  # Load example data
#' x <- QuickStartExample$x; colnames(x) <- paste0("X", 1:ncol(x))
#' y <- QuickStartExample$y
#'
#' dat <- data.frame(x, y)
#'
#' spl_df <- data.frame(
#'    Var = colnames(x),
#'    Func = "s",
#'   Args ="bs='cr', k=7"
#' )
#'
#' train_sm_dat <- construct_smooth_data(spl_df, dat)
#' train_smooth <- train_sm_dat$Smooth
#' train_smooth_data <- train_sm_dat$data
#' mdl <-bamlasso(x = train_smooth_data, y = y, family = "gaussian",
#'                group = make_group(names(train_smooth_data)))
#' plot_smooth_term(mdl, "X3", train_smooth, min = min(x[,"X3"])-0.1, max = max(x[,"X3"]) + 0.1)
#'

plot_smooth_term <- function(mdl, term, smooth, min, max,  plot = TRUE){


  # TODO: Implement the plotting function for other models
  if(!("bmlasso" %in% class(mdl)))
    stop("Not Implemented for bgam and bacoxph yet")

  # TODO: check if term is included in the model, and smooth

  # Loop Start here
  # for(var in terms)
  # {
    # retrieve the smoothing object
    sm <- `[[`(smooth, term)
    # Retrive the min and max value of the variable, see(min, and max argument of the funciton)

    # Construct the vector of possible value of the variable
    .dat <- data.frame(seq(min, max, length.out = 200))
    colnames(.dat) <- term

    # Construct the data matrix
    ret <- mgcv::PredictMat(sm, data = .dat)
    colnames(ret) <- create_smooth_name(sm)

    # Retrieve the coefficients
    .coef <- mdl$coefficients[colnames(ret)]

    # Calculate the linear predictors
    # browser()
    .dat$value <- `[[`(.dat, term)
    .dat$lp <- ret %*% .coef

    # Decide if we are plotting at the linear predictor scale or response scale
    # TODO: implement later

    # Make plots
    if(plot){
      # Make ggplot
      ggplot2::ggplot(.dat) +
        ggplot2::geom_smooth(ggplot2::aes(x = .data$value, y = .data$lp))+
      ggplot2::geom_point(ggplot2::aes(x = .data$value, y = .data$lp), alpha = 0.5)

    } else {
      # return Variable value, linear predicrtors, and response scale
      stop("Not implemented")
    }
  # } # End Loop
}
