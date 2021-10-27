#' Loading the glow dataset
#'
#' Function to load the dataset
#'
#'
#' @return A data frame with glow data.
#'
#' @examples
#' load_glow()
#'
#' @export

load_glow <- function(path=""){
  library(tidyverse)
  glow <- read.csv(path)

  glow <- glow %>%
    mutate(PRIORFRAC = factor(PRIORFRAC, levels=c(0,1), labels=c("No","Yes")),
           PREMENO = factor(PREMENO, levels=c(0,1), labels=c("No","Yes")),
           MOMFRAC = factor(MOMFRAC, levels=c(0,1), labels=c("No","Yes")),
           ARMASSIST = factor(ARMASSIST, levels=c(0,1), labels=c("No","Yes")),
           SMOKE = factor(SMOKE, levels=c(0,1), labels=c("No","Yes")),
           FRACTURE = factor(FRACTURE, levels=c(0,1), labels=c("No","Yes")))
}
