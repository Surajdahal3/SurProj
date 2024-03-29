% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univ_analysis.R
\name{univ_analysis}
\alias{univ_analysis}
\title{Univariate analysis}
\usage{
univ_analysis(data, p_threshold = 0.25)
}
\arguments{
\item{data}{A data frame.}

\item{p_threshold}{Numeric variable between 0 and 1.}
}
\value{
A list with the significant numeric variables and contingency tables for categorical variables.
}
\description{
For each individual variable in the glow dataset.
For categorical variables, a contingency table analysis of
the outcome vs the 𝑘 level of the independent variable will
be conducted. For continuous variables, we will fit univariable
logistic regression modes. From the results of the univariable
logistic regression, we will identify candidate variables for the
first multivariable model, any variable whose univariable test has
a p-value < 0.25 will be considered as a candidate variable.
}
\examples{
univ_analysis()

}
