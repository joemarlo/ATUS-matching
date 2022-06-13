
#' Select the best gls model based on BIC
#'
#' @param ... gls models produced from nlme::gls
#'
#' @return a gls model
#' @export
select_model <- function(...){
  
  # run anova and select best based on BIC
  anova_results <- anova(...)
  best_model <- rownames(anova_results)[which.min(anova_results$BIC)]
  best_model <- eval(as.symbol(best_model))
  cli::cli_alert_success('Best model is {best_model$call}')
  
  # print best model coef
  tmp <- summary(best_model)
  tmp$tTable %>% 
    as_tibble() %>% 
    mutate(Coef = names(tmp$coefficients)) %>% 
    slice(-1) %>% 
    select(Coef, Beta = Value, Std.Error, `p-value`) %>% 
    print()
  
  return(best_model)
}
