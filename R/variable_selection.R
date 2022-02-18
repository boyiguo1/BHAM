generate_var_table <- function(var_vec){
  # browser()

  if(length(var_vec) == 0)
    return(list(
      `Parametric` = vector(),
      `Non-parametric` = data.frame(
        Variable = character(),
        Linear = logical(),
        Nonlinear = logical())
    )
    )

  # Find Non-parametric Variables
  non_par_idx <- grepl("((\\.null\\d*))|(\\.pen\\d*)$", var_vec)

  # Parametric Variables
  par_list <- var_vec[!non_par_idx]

  if(any(non_par_idx)){
    # Non-parametric Variables
    non_par_df <- var_vec[non_par_idx] %>%
      # Form 3-by-2 table
      unglue::unglue_data( "{var}.{part=pen|null}") %>%
      mutate(ext = TRUE) %>%
      reshape(
        direction = "wide",
        idvar = "var", timevar = "part",
        v.names = "ext")


    if(ncol(non_par_df)!=3){
      if(!("ext.null" %in% names(non_par_df)))
        non_par_df <- non_par_df %>% mutate( ext.null = NA)
      if(!("ext.pen" %in% names(non_par_df)))
        non_par_df <- non_par_df %>% mutate( ext.pen = NA)
    }

    # Rename Tablenon_par_df
    non_par_df <- non_par_df %>%
      transmute(
        Variable = var,
        Linear = replace(.data$ext.null, is.na(.data$ext.null), FALSE),
        Nonlinear = replace(.data$ext.pen, is.na(.data$ext.pen), FALSE)
      ) #%>%
    # Analytically Remove Functions without Linear components
    #filter(.data$Linear == TRUE)
  } else {
    non_par_df <- data.frame(
      Variable = character(),
      Linear = logical(),
      Nonlinear = logical())
  }

  return(
    list(
      `Parametric` = par_list,
      `Non-parametric` = non_par_df
    )
  )
}


#' Variable Selection for Bamlasso
#'
#' @param mdl Model returned from Bamlasso
#'
#' @return A list of two consisting a character vector, named Parametric, and a data frame, named Non-parametric. The character vector contains the parametric variabels selected in the model if any. The dataframe, 3-by-p, contains the selected smoothing function, separated for linear components and nonlinear components
#' @export
#'
#' @examples
bamlasso_var_selection <- function(mdl){

  # Find non-zero coefficients
  ## Remove Intercept, and trailing index for splines
  ## Remove trailing index for splines and keep unique spline components
  mdl$coefficients[which(mdl$coefficients!=0)] %>%
    names() %>%
    setdiff("(Intercept)") %>%
    gsub("\\d*$", "", .) %>%
    unique %>%
    generate_var_table()
}
