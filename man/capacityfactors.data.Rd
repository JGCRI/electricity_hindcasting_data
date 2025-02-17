% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{capacityfactors.data}
\alias{capacityfactors.data}
\title{Capacity Factors, Form Data}
\format{A data framewith 84312 and 5 variables}
\usage{
data(capacityfactors.data)
}
\description{
Plant-specific capacity factors for specified fuel and technology choices.
}
\details{
This calculation can be found in data-raw/costs/capacityfactors.R. Due to data errors,
some capacity factors are greater than one. These are set to 1.

Calculated capacity factors less than 0.1 are filtered out.

\describe{
\item{yr}{Year of reported data}
\item{plntcode}{EIA-assigned plant code}
\item{overnightcategory}{Technology category}
\item{fuel.general}{Fuel category}
\item{capacityfactor}{8760*capacity/generation, unitless}
}
}
