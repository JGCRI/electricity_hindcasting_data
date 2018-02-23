#' energy.markets: A data package for U.S. electricity capacity, generation, and investment.
#'
#' The energy.markets package provides cleaned and processed data for electricity, generation, and investment,
#' as well as calculations of plant-level capacity factors and levelized cost. The final levelized cost data is
#' used externally as the target dataset for a multinomial logistic regression, as part of JGCRI hindcasting
#' experiments.
#'
#' @section Data sources:
#' Capacity and generation data are processed from raw data files providing the responses to EIA forms.
#'
#' Investment data is processed from Annual Energy Outlook forms published by the EIA.
#'
#' Fuel price data is processed from tables provided in back issues of the EIA Electric Power Monthly.
#'
#' A GDP deflator series for the St. Louis Federal Reserve is used to normalize USD$ values to 2010.
#'
#' @section Calculations:
#'
#' Documentation.
#'
#'
#' @docType package
#' @name energy.markets
NULL

#' Electrical Capacity
#'
#' U.S. electrical capacity compiled from EIA form 860 for 1990-2016, using constructed
#' fuel and technology categorical variables to provide a broader description of the data.
#'
#' See data-raw/generators/1990to2000_utilities.R and 2001to2016_utilities.R for functions which
#' produced the unmapped data set from the raw original data files provided by the EIA.
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{vintage}{First year capacity is available}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{nameplate}{Reported nameplate capacity, MW}
#' }
#' @name capacity
#' @usage data(capacity)
#' @format A data framewith 239483 and 7 variables
#' @source \url{https://www.eia.gov/electricity/data/eia860/}
NULL


#' Electrical Capacity, Unmapped
#'
#' U.S. electrical capacity compiled from EIA form 860 for 1990-2016, using native EIA
#' primemover and fuel codes.
#'
#' See data-raw/generators/1990to2000_utilities.R and 2001to2016_utilities.R for functions which
#' produced the unmapped data set from the raw original data files provided by the EIA.
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{primemover}{Native EIA prime mover code}
#' \item{fuel}{Native EIA fuel code}
#' \item{vintage}{First year capacity is available}
#' \item{nameplate}{Reported nameplate capacity, MW}
#' }
#' @name capacity.unmapped
#' @usage data(capacity.unmapped)
#' @format A data framewith 245936 and 7 variables
#' @source \url{https://www.eia.gov/electricity/data/eia860/}

NULL


#' Capacity Factors
#'
#' Plant-specific capacity factors for specified fuel and technology choices.
#'
#' This calculation can be found in data-raw/costs/capacityfactors.R. Due to data errors,
#' some capacity factors are greater than one.
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{capacityfactor}{8760*capacity/generation, unitless}
#' }
#' @name capacityfactors
#' @usage data(capacityfactors)
#' @format A data framewith 101437 and 6 variables
NULL


#' Capacity Factors, clamped to 1 from above
#'
#' Plant-specific capacity factors for specified fuel and technology choices.
#'
#' This calculation can be found in data-raw/costs/capacityfactors.R. Observations with a
#' calculated capacity factor greater than one are set equal to one.
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{capacityfactor}{8760*capacity/generation, unitless}
#' }
#' @name capacityfactors.clamp
#' @usage data(capacityfactors.clamp)
#' @format A data framewith 101437 and 6 variables
NULL


#' Capital Costs
#'
#' Input assumptions to EIA's National Energy Model System, which is used to produce the Annual
#' Energy Outlook report, for 1996-2015.
#'
#' Tables from each report appear under the name 'Cost and Performance Characteristics of New
#' Central Station Electricity Generating Technologies' and were manually compiled into one table.
#' This file is an input into the processiong function stored in data-raw/costs/AEO/capitalcosts.R, which
#' uses a gdpdeflator series to get costs in USD$2010. The reported overnight categories are also
#' mapped to our technology categories. Original reports, manually-compiled datafile, and technology mapping
#' included in data-raw/costs/AEO/.
#'
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{overnightcategory}{Technology category}
#' \item{overnight}{Base overnight costs, in $/kW}
#' \item{om.var}{Variable operations & management costs, in $/kW/yr}
#' \item{om.fixed}{Fixed operations & management costs, in $/MWh}
#' }
#' @name capitalcosts
#' @usage data(capitalcosts)
#' @format A data framewith 225 and 5 variables
#' @source \url{https://www.eia.gov/outlooks/aeo/archive.php}
NULL


#' Fuel Prices
#'
#' Fossil fuel prices taken from EIA Electric Power Monthly reports.
#'
#' A data file compiles prices reported in the following issues:
#' June 1996 - Table 26
#' January 2010 - Table 4.2
#' May 2016 - Table 4.2
#'
#' The original reports and compiled data file can be found in data-raw/costs/fuel/.
#'
#' Uranium source??
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{fuel.general}{Fuel category}
#' \item{fuel.price}{Fuel price, in $/BTU}
#' }
#' @name fuelprices
#' @usage data(fuelprices)
#' @format A data framewith 94 and 3 variables
#' @source \url{https://www.eia.gov/electricity/monthly/backissues.html}
NULL


#' Electricity Generation Net Output
#'
#' U.S. electrical generation net output compiled from EIA forms 906, 920, and 923 (depending on year), using
#' constructed fuel and technology categorical variables to provide a broader description of the data.
#'
#' See data-raw/generation/1990to2000_utilities.R and 2001to2016_utilities.R for functions which
#' produced the unmapped data set from the raw original data files provided by the EIA.
#'
#' Generation is negative where generator consumed more electricity than it produced. These rows are filtered out
#' when calculating capacity factors.
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{generation}{Reported net electrical output, MWh}
#' \item{consumption}{}
#' }
#' @name generation
#' @usage data(generation)
#' @format A data framewith 183802 and 7 variables
#' @source \url{https://www.eia.gov/electricity/data/eia923/}
NULL


#' Electricity Generation Net Output, Unmapped
#'
#' U.S. electrical generation net output compiled from EIA form 906, 920, and 923 (depending on year), using
#' native EIA primemover and fuel codes.
#'
#' #' See data-raw/generation/1990to2000_utilities.R and 2001to2016_utilities.R for functions which
#' produced the unmapped data set from the raw original data files provided by the EIA.
#'
#' Generation is negative where generator consumed more electricity than it produced. These rows are filtered out
#' when calculating capacity factors.
#'
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{primemover}{Native EIA prime mover code}
#' \item{fuel}{Native EIA fuel code}
#' \item{generation}{Reported net electrical output, MWh}
#' \item{consumption}{}
#' }
#' @name generation.unmapped
#' @usage data(generation.unmapped)
#' @format A data framewith 208679 and 7 variables
#' @source \url{https://www.eia.gov/electricity/data/eia923/}

NULL


#' Levelized Capital Costs
#'
#' Capital costs with levelized overnight and fixed O&M costs.
#'
#' Equation for levelized cost can be found in data-raw/costs/levelize.R, which involves
#' capacity factors dataset and a fixed charge rate, which spreads the cost of the investment over the
#' expected lifetime of the capacity addition. The fixed charge rate is taken to be 0.13 in order to
#' match GCAM calibration.
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{overnight.lev}{Levelized overnight cost, in $/MWh}
#' \item{om.fixed.lev}{Levelized fixed O&M, in $/MWh}
#' \item{om.var}{Variable O&m, in $/MWh}
#' }
#' @name levelizedcosts
#' @usage data(levelizedcosts)
#' @format A data framewith 82036 and 8 variables
NULL


#' Mapping to GCAM Fuel-Tech Options
#'
#' Takes all unique observations of prime mover and fuel, and provides a technology category and
#' fuel category to describe the data at a courser level than the EIA-native prime mover (33 observed) and fuel
#' codes (61 observed).
#'
#' \describe {
#' \item{primemover}{Native EIA prime mover code}
#' \item{fuel}{Native EIA fuel code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' }
#' @name mapping
#' @usage data(mapping)
#' @format A data framewith 181 and 4 variables
NULL


#' Marginal Costs
#'
#' Fuel price scaled by fuel efficiency, which is calculated from average heatrates given for a specified
#' technology and fuel in a given year.
#'
#' See 'Marginal Costs' section of data-raw/generatedata.R for this calculation. Heatrates included at
#' data-raw/costs/fuel/avghr.csv.
#'
#' Source of avghr.csv??
#'
#' \describe {
#' \item{yr}{Year of reported data}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{marginal.cost}{Marginal cost of producing electricity using specified technology and fuel, in $/MWh}
#' }
#' @name marginalcosts
#' @usage data(marginalcosts)
#' @format A data framewith 190 and 4 variables
NULL


