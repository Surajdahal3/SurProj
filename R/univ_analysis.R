#' Univariate analysis
#'
#' For each individual variable in the glow dataset.
#' For categorical variables, a contingency table analysis of
#' the outcome vs the 𝑘 level of the independent variable will
#' be conducted. For continuous variables, we will fit univariable
#' logistic regression modes. From the results of the univariable
#' logistic regression, we will identify candidate variables for the
#' first multivariable model, any variable whose univariable test has
#' a p-value < 0.25 will be considered as a candidate variable.
#'
#' @param data A data frame.
#' @param p_threshold Numeric variable between 0 and 1.
#'
#' @return A list with the significant numeric variables and contingency tables for categorical variables.
#'
#' @examples
#' univ_analysis()
#'
#' @export

univ_analysis <- function(data, p_threshold=0.25){

  # Checking if the variable is categorical or not
  check_factor = sapply(data,class)=="factor"
  # Creating a vector to save the significant numeric variables
  num_var_cand = character()
  # Creating a list to save the contingency tables
  ctg_tables = list()
  cont=0
  for (i in 1:10){
    if(check_factor[i]){
      cont=cont+1
      ctg_tables[[cont]] = table(data[,11], data[,i])
      names(ctg_tables)[cont] = names(data)[i]
    }
    else{
      log_model_temp = glm(FRACTURE ~ data[,i], family="binomial", data)
      p_check = summary(log_model_temp)$coefficients[2,4]
      if (p_check < p_threshold) num_var_cand = c(num_var_cand, names(data)[i])
    }

  }
  return(list(num_var_cand, ctg_tables))
}
