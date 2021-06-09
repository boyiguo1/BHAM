parse_var_name <- function(vec){
  # name, fit){
  # fit %>% `$`({{name}}) %>%
    vec %>%
    tibble::enframe() %>%
    unglue::unglue_unnest(name, "{var}.{part=pen|null}{ind=\\d*}", remove = FALSE) %>%
    group_by(var, part) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    dplyr::select(-name, -ind) %>%
    # dplyr::rename("{{name}}"=value) %>%
    dplyr::select(var, part, value)
                  #{{name}})
}

#' Summarize ancillary parameters
#'
#' @param fit the fitted object from bglm_spline or bmlasso_spline
#'
#' @return a data frame contains summarized ancillary parameters for spline components of each variable
#' @export
#'
#' @examples
summarize_ancillary_parameters <- function(fit){

  list(ptheta = fit$ptheta,
       prior.scale = fit$prior.scale) %>%
  # list(ptheta,
  #      prior.scale) %>%
    purrr::map(parse_var_name) %>%
    purrr::reduce(full_join, by = c("var", "part")) %>%
    dplyr::rename(ptheta = value.x,
           prior.scale = value.y)

  # theta <- fit$ptheta %>%
  #
  # scale <- fit$prior.scale %>% enframe() %>%
  #   unglue::unglue_unnest(name, "{var}.{part=pen|null}{ind=\\d*}", remove = FALSE) %>%
  #   group_by(var, part) %>%
  #   slice_head(n = 1) %>%
  #   ungroup() %>%
  #   select(-name, -ind) %>%
  #   select(var, part, prior.scale=value)


}
