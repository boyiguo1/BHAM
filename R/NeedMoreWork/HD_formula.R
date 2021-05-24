#' Create spline formula for high-dimension applications
#'
#' @description
#' Create a formula that contains spline terms for high-dimension applications using spline_df
#'
#' @details
#' One of the [formula] and [data] arguments are required to create the parametric part of the formula. When both arguments are supplied,
#' [data] arguments would be ignored. When [data] is supplied, \link[stats]{DF2formula} will be called to generate [formula].
#'
#' [spl_df] is a data frame of three string columns: "Var" column contains the variable for splines, "Func" column contains the corresponding spline of choice (see also \link{mgcv}[smooth.terms]),
#' "Args" column contains the arguments for the spline function.
#'
#'
#'
#' @param formula an optional object of class "\link[stats]{formula}": a symbolic description of the parametric part of the model
#' @param data an optional data frame, containing the parametric variables in the model, which will directly translate to a formula
#' @param spl_df a data frame of three columns ("Var","Func", "Args") that is used to create spline terms in the model. Please see example or details or vignettes
#' @param rm_overlap a logical variable: when there are overlapping variable(s) in both parametric part and spline part of the model, if the overlapping variables are removed from the parametric part.
#' @param verbose a logical variable: if the result formula would be printed
#'
#' @return an object of class "\link[stats]{formula}": the final spline formula that could be to  \link{bgam} or \link[mgcv]{gam}
#'
#' @seealso
#' \link{stats}[update], \link{stats}[formula], \link{mgcv}[smooth.terms]
#'
#' @export
#'
#' @examples
#' # spl_df template
#' spline_df <- tribble(
#'  ~Var, ~Func, ~Args,
#'  "x0", "s", "k=1, bs='cr'",
#'  "x1", "s", "k=2, bs='cr'",
#'  "x2", "", "k=2" # will be ignored
#' )
create_HD_formula <- function(formula, data, spl_df, rm_overlap = TRUE, verbose=T){

  if(missing(formula) & missing(data))
    stop("Either formula or data must be provided. The response variable of the model is not supplied.")

  if(missing(formula)){
    if(!is.data.frame(data)) data <- data.frame(data)
    formula = DF2formula(data)
  }

  if(!missing(data)){
    warning("Both formula and dat provided, dat is ignored.")
    warning("Please consider use the function DF2formula and update to improve your formula.")
    # formula[[3]] <- paste(". +", formula[[3]])
    # if(!is.data.frame(dat)) dat <- data.frame(dat)
    # formula <- update(DF2formula(dat),
    #                   formula)  #TODO:update DF2formula given

  }


  if(missing(spl_df)) {
    warning(" No additional spline terms supplied")
  } else {
  # Manipulate spl_df
  sp_trm <-  spl_df %>%
    dplyr::filter(Func!="")  %>% # Removing unnecessary terms
    glue::glue_data("{Func}( {Var}{ifelse(is.na(Args)||Args=='', '', paste0(',', Args))})") %>%
    paste(collapse  = " + ")
  sp_trm <- paste0("~ . + " , sp_trm)
  }
  # Adding Spline Terms
  ret <- update(formula, sp_trm)

  if(verbose){
    cat("Create formula:\n")
    print(ret)
  }

  return(ret)
}

