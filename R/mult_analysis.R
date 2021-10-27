#' Multivariate analysis
#'
#' Fit the multivariable model comprising all variables identified in
#' the univariate analysis (univ_analysis()).
#' Then, the variables that do not contribute to the model
#' based in a p.value threshold (e.g., p-value > 0.05)
#' will be excluded and a new smaller model will be fitted.
#'
#' @param data A data frame.
#' @param p_threshold Numeric variable between 0 and 1.
#'
#' @return The summary for the final logistic model based on FRACTURE outcome versus the significant numerical variables.
#'
#' @examples
#' univ_analysis()
#'
#' @export

mult_analysis <- function(data, p_threshold=0.05){
  # Significant numeric variables from step1
  signif_vars = univ_analysis(data=glow)[[1]]

  mult_log_model <- glm(FRACTURE ~ ., family="binomial", data=data[,c("FRACTURE", signif_vars)])

  the_p_values = summary(mult_log_model)$coefficients[,4]

  the_final_vars = names(the_p_values[the_p_values < p_threshold])
  final_mult_log_model <- glm(FRACTURE ~ ., family="binomial", data=data[,c("FRACTURE", the_final_vars)])

  return(summary(final_mult_log_model))

}
