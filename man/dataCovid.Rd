\name{dataCovid}
\alias{dataCovid}
\docType{data}
\title{
COVID-19 statistics
}
\description{
This data set consists of several statistics about the COVID-19 pandemic in 45 countries.
}
\usage{data("dataCovid")}
\format{
  A data frame with 18400 observations on the following 27 variables.
  \describe{
    \item{\code{location}}{a character vector}
    \item{\code{date}}{a Date}
    \item{\code{new_cases}}{a numeric vector}
    \item{\code{new_cases_per_million}}{a numeric vector}
    \item{\code{new_cases_smoothed_per_million}}{a numeric vector}
    \item{\code{new_cases_smoothed}}{a numeric vector}
    \item{\code{new_deaths_per_million}}{a numeric vector}
    \item{\code{new_deaths}}{a numeric vector}
    \item{\code{new_deaths_smoothed_per_million}}{a numeric vector}
    \item{\code{new_deaths_smoothed}}{a numeric vector}
    \item{\code{total_deaths_per_million}}{a numeric vector}
    \item{\code{total_deaths}}{a numeric vector}
    \item{\code{total_cases}}{a numeric vector}
    \item{\code{total_cases_per_million}}{a numeric vector}
    \item{\code{hosp_patients}}{a numeric vector}
    \item{\code{hosp_patients_per_million}}{a numeric vector}
    \item{\code{icu_patients_per_million}}{a numeric vector}
    \item{\code{icu_patients}}{a numeric vector}
    \item{\code{reproduction_rate}}{a numeric vector}
    \item{\code{new_tests}}{a numeric vector}
    \item{\code{new_tests_per_thousand}}{a numeric vector}
    \item{\code{tests_per_case}}{a numeric vector}
    \item{\code{positive_rate}}{a numeric vector}
    \item{\code{new_tests_smoothed}}{a numeric vector}
    \item{\code{new_tests_smoothed_per_thousand}}{a numeric vector}
    \item{\code{total_tests}}{a numeric vector}
    \item{\code{total_tests_per_thousand}}{a numeric vector}
  }
}
\details{
This is a subset of the complete data set available online, downloaded on March 31, 2021.
}
\source{
https://covid.ourworldindata.org/data/owid-covid-data.csv
}

\examples{
library(zoo)
data(dataCovid)
casesFin <- subset(dataCovid,subset=location=="Finland", select=c(date,new_cases))
plot(zoo(casesFin$new_cases,order.by=casesFin$date),ylab="New COVID-19 cases in Finland",
xlab="")
}

