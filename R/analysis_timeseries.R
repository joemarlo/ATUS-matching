
#' Select the best gls model based on BIC
#'
#' @param ... gls models produced from nlme::gls
#'
#' @return a gls model
#' @export
#' @import dplyr
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
  
  # plot coef plot
  print(plot_coef(best_model) + labs(title = 'Best model by BIC', subtitle = best_model$call))
  
  return(best_model)
}

#' Plot gls model coefficients
#'
#' @param .model 
#'
#' @return
#' @export
#' @import ggplot2, dplyr
plot_coef <- function(.model){
  
  if(!inherits(.model, 'gls')) cli::cli_abort('.model must be of class "gls"')
  
  coef_table <- summary(.model)$tTable
  coef_table %>%
    as_tibble() %>% 
    mutate(Coef = rownames(coef_table),
           low_95 = Value - (Std.Error * 1.96),
           high_95 = Value + (Std.Error * 1.96),
           sig = if_else(`p-value` <= 0.05, 'Significant at alpha=0.05', 'Not significant at alpha=0.05')) %>% 
    filter(Coef != '(Intercept)') %>% 
    ggplot(aes(x = Value, y = Coef, color = sig)) +
    geom_vline(xintercept = 0, color = 'grey70', linetype = 'dashed') +
    geom_point(size = 3) +
    geom_linerange(aes(xmin = low_95, xmax = high_95)) +
    scale_color_manual(values = c('Significant at alpha=0.05' = '#0d9e97', 
                                  'Not significant at alpha=0.05' = '#e33d3d')) +
    labs(x = NULL,
         y = NULL,
         color = NULL) +
    theme(legend.position = 'bottom')
}
