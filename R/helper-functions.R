#' Creating design matrix based on spline configuration
#'
#' @description contruct_smooth_data creates the design matrix according to the spline function defined
#' in sm_df. The spline design matrix is created using smoothCon from the pacakge mgcv.
#'
#'
#' @param sm_df a data frame that has three columns, _Func_, _Var_, _Args_ (case sensitive). See example
#' @param dat the raw data set
#'
#' @return a list contains
#' \itemize{
#'   \item{data}{the contructed design matrix, the intercept and outcome is not included}
#'   \item{Smooth}{}
#' }
#'
#' @export
#'
#' @importFrom glue glue_data
#' @import mgcv
#' @importFrom  rlang parse_expr
#'
#' @examples
#'
#' raw_dat <- sim_Bai(100, 5)$dat %>% data.frame
#'
#' sm_df <- data.frame(
#'  Var = setdiff(names(raw_dat), "y"),
#'  Func = "s",
#'  Args ="bs='cr', k=5"
#' )
#'
#'
#' construct_smooth_data(sm_df, raw_dat)
#'
#'
construct_smooth_data <- function(sm_df, dat){

  # To construct a list of expression
  fml_df <- sm_df %>% glue::glue_data("{Func}( {Var}{ifelse(is.na(Args)||Args=='', '', paste0(', ', Args))})")

  .dat <- data.frame(id = 1:nrow(dat))

  SMs <- list()

  for(i in 1:length(fml_df)){
    raw_sm <- (fml_df[i] %>% rlang::parse_expr() %>% eval() %>%
                 smoothCon(object = ., data = dat,
                           scale.penalty = TRUE,
                           absorb.cons = TRUE,
                           null.space.penalty = TRUE,
                           diagonal.penalty = TRUE))[[1]]


    if(length(raw_sm$rank) ==2 ){
      pen.ind <- colSums(raw_sm$S[[1]])!=0
      null.ind <- colSums(raw_sm$S[[2]])!=0
      colnames(raw_sm$X)[pen.ind] <- paste0(raw_sm$term, ".pen", 1:sum(pen.ind))
      colnames(raw_sm$X)[null.ind] <- paste0(raw_sm$term, ".null", 1:sum(null.ind))
    } else if(length(raw_sm$rank) == 1 ){
      colnames(raw_sm$X) <- paste0(raw_sm$term, ".base", 1:ncol(raw_sm$X))
      warning("Abnormal behaviour when creating spline design matrix. No null space.")
    } else{
      error("Fail to create spline design matrix. Rank length > 2")
    }



    .dat <- cbind(.dat,
                  raw_sm$X)
    SMs[[raw_sm$term]] <- raw_sm

    # expr(`$`(SMs, !!raw_sm$term)<-raw_sm) %>% eval()
  }


  return(
    list(data = .dat %>% select(-id),
         Smooth = SMs)
  )
}


#' Create grouping for spline design matrix
#'
#' @param .names spline matrix names, always in the format "var_name.baseX". Directly from Construct_Smooth_Data
#' #param null_group A indicator if the null space are in its own group, i.e. if null space are penalized
#' @param penalize_null do we penalize the null space, i.e. do we fit the null space into group parameter in bglm group parameter.
#' @param shared_null  A indicator if the null space have a shared indicator with the penalized space
#'
#' @return
#' @export
#'
#' @importFrom unglue unglue_unnest
#' @import dplyr
#' @importFrom magrittr `%>%`
#'
#' @examples
#' raw_dat <- sim_Bai(100, 5)$dat %>% data.frame
#'
#' sm_df <- data.frame(
#'  Var = setdiff(names(raw_dat), "y"),
#'  Func = "s",
#'  Args ="bs='cr', k=5"
#' )
#'
#' dsn_mat <- construct_smooth_data(sm_df, raw_dat)$data
#'
#' make_group(names(dsn_mat))
make_group <- function(.names,
                       penalize_null = TRUE,
                       # null_group = TRUE,
                       shared_null = FALSE
){

  # null_group & shared_null should not be set at TRUE at the same time
  data.frame(names = .names, stringsAsFactors = FALSE) %>%
    unglue::unglue_unnest(names, "{var}.{part=pen|null}{ind=\\d*}", remove=FALSE) %>%
    mutate(ind = as.numeric(ind)) %>%
    {
      if(shared_null){
        group_by(., var)
      }
      else{
        group_by(., var, part)
      }
    } %>%
    {
      if(!penalize_null)
        filter(., part == "pen")
      else
        .
    } %>%
    summarize(res =  list(names), .groups = "drop") %>%
    pull(res)

  # if(null_group & !shared_null) {
  #   concat_list <- data.frame(names = .names, stringsAsFactors = FALSE) %>%
  #     unglue::unglue_unnest(names, "{var}.base{ind}", remove=FALSE) %>%
  #     mutate(ind = as.numeric(ind)) %>%
  #     group_by(var) %>%
  #     arrange(ind) %>%
  #     slice(., n()) %>%
  #     summarize(res =  list(names), .groups = "drop") %>%
  #     pull(res)
  # }

  # ret <- c(ret, concat_list)
}

#' Title
#'
#' @param Smooth
#' @param dat
#'
#' @return
#' @export
#'
#' @import purrr
#'
#' @examples
make_predict_dat <- function(Smooth, dat){

  map_dfc(Smooth,
          .f= function(sm, .dat){
            ret <- mgcv::PredictMat(sm, data = .dat)

            # TODO: fix the naming of this part as well.

            colnames(ret) <- paste0(sm$term, ".base",1:ncol(sm$X))
            ret %>% data.frame
          },
          .dat = dat
  )

}

# coef <- coefficients(bgam_local)
# cov <- vcov.bh(bgam_local)
# test_vars <- function(coef, cov){
#   pos_vec <- data.frame(names = names(coef)) %>%
#     mutate(pos = 1:n()) %>%
#     unglue::unglue_unnest(names, "{var}.base{ind}", remove=FALSE) %>%
#     mutate(ind = as.numeric(ind)) %>%
#     filter(!is.na(ind)) %>%
#     group_by(var) %>%
#     arrange(ind) %>%
#     summarize(pos =  list(pos), .groups = "drop")
#
#
#     waldtest.bh(coef, cov, pos_vec$pos[[3]])
#   })
#
# }
