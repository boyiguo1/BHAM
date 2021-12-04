# Terms, a vector of charaters containing the variable names
# plot, a logic variable, indicating if ggplots are produced
plot_smooth_term <- function(mdl, terms, plot = TRUE){

  # Loop Start here
  for(var in terms)
  {
    # retrieve the smoothing object

    # Retrive the min and max value of the variable

    # Construct the vector of possible value of the variable

    # Construct the data matrix

    # Retrieve the coefficients

    # Calculate the linear predictors

    # Decide if we are plotting at the linear predictor scale or response scale

    # Make plots
    if(plot){
      # Make ggplot
    } else {
      # return Variable value, linear predicrtors, and response scale
    }
  } # End Loop
}
