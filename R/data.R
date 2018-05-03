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

#' Electrical Capacity, Unmapped
#'
#' U.S. electrical capacity compiled from EIA form 860 for 1990-2016, using native EIA
#' primemover and fuel codes.
#'
#' See data-raw/generators/1990to2000_utilities.R and 2001to2016_utilities.R for functions which
#' produced the unmapped data set from the raw original data files provided by the EIA.
#'
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{primemover}{Native EIA prime mover code}
#' \item{fuel}{Native EIA fuel code}
#' \item{vintage}{First year capacity is available}
#' \item{capacity}{Reported nameplate capacity, MW}
#' }
#' @name capacity.unmapped
#' @usage data(capacity.unmapped)
#' @format A data framewith 245936 and 7 variables
#' @source \url{https://www.eia.gov/electricity/data/eia860/}

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
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{utilcode}{EIA-assigned utility code}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{primemover}{Native EIA prime mover code}
#' \item{fuel}{Native EIA fuel code}
#' \item{generation}{Reported net electrical output, MWh}
#' }
#' @name generation.unmapped
#' @usage data(generation.unmapped)
#' @format A data framewith 175502 and 7 variables
#' @source \url{https://www.eia.gov/electricity/data/eia923/}

NULL


#' Joined Electrical Capacity and Generation Output, Mapped
#'
#' This is the dataset that reports capacity and generation using the constructed fuel and technological
#' categorical variables, which describe the data more broadly than the native fuel and primemover codes.
#'
#' These two datasets are joined by two methods. First, in 2001-2002, we join by yr, plntcode, and fuel. We
#' do this because the Generation dataset reports primemovers exclusively as NA for these years. In all other years,
#' we join by yr, plntcode, primemove, and fuel. Neither method uses utilcode, as there are significant
#' inconsistencies within both the Generation and Capacity datasets, which prevent adequate joining between
#' the two datasets.
#'
#' We take the inner_join() between these datasets using the keys indicated above. Finally, that join is mapped
#' to the overnightcategory and fuel variables contained in our mapping file. Mapping to these new keys
#' requires aggregation, as multiple pm-f codes map to the same overnightcategory and fuel pair. The resulting
#' dataset is used in the calculation of capacity factor.
#'
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{startyr}{Weighted average (by capacity) of reported start years}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{capacity}{Reported nameplate capacity, MW}
#' \item{generation}{Reported net electrical output, MWh}
#' }
#' @name cap.gen.joined.unmapped
#' @usage data(cap.gen.joined.unmapped)
#' @format A data framewith 121104 and 7 variables
NULL


#' Mapping to GCAM Fuel-Tech Options
#'
#' Takes all unique observations of prime mover and fuel, and provides a technology category and
#' fuel category to describe the data at a courser level than the EIA-native prime mover (56 observed) and fuel
#' codes (31 observed).
#'
#' \describe{
#' \item{primemover}{Native EIA prime mover code}
#' \item{fuel}{Native EIA fuel code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' }
#' @name mapping
#' @usage data(mapping)
#' @format A data framewith 175 and 5 variables
NULL

#' Joined Electrical Capacity and Generation Output, Mapped
#'
#' This is the dataset that reports capacity and generation using the constructed fuel and technological
#' categorical variables, which describe the data more broadly than the native fuel and primemover codes.
#'
#' These two datasets are joined by two methods. First, in 2001-2002, we join by yr, plntcode, and fuel. We
#' do this because the Generation dataset reports primemovers exclusively as NA for these years. In all other years,
#' we join by yr, plntcode, primemove, and fuel. Neither method uses utilcode, as there are significant
#' inconsistencies within both the Generation and Capacity datasets, which prevent adequate joining between
#' the two datasets.
#'
#' We take the inner_join() between these datasets using the keys indicated above. Finally, that join is mapped
#' to the overnightcategory and fuel variables contained in our mapping file. Mapping to these new keys
#' requires aggregation, as multiple pm-f codes map to the same overnightcategory and fuel pair. The resulting
#' dataset is used in the calculation of capacity factor.
#'
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{startyr}{Weighted average (by capacity) of reported start years}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{capacity}{Reported nameplate capacity, MW}
#' \item{generation}{Reported net electrical output, MWh}
#' }
#' @name cap.gen.joined
#' @usage data(cap.gen.joined)
#' @format A data framewith 121104 and 7 variables
NULL


#' Capacity Factors, Form Data
#'
#' Plant-specific capacity factors for specified fuel and technology choices.
#'
#' This calculation can be found in data-raw/costs/capacityfactors.R. Due to data errors,
#' some capacity factors are greater than one. These are set to 1.
#'
#' Calculated capacity factors less than 0.1 are filtered out.
#'
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{capacityfactor}{8760*capacity/generation, unitless}
#' }
#' @name capacityfactors.data
#' @usage data(capacityfactors.data)
#' @format A data framewith 84312 and 5 variables
NULL


#' Capacity Factors, Supplemental
#'
#' Technology-specific capacity factors taken from the EIA Electricity Power Monthly.
#'
#' \describe{
#' \item{overnightcategory}{Technology category}
#' \item{capacityfactor}{8760*capacity/generation, unitless}
#' }
#' @name capacityfactors.sup
#' @usage data(capacityfactors.sup)
#' @format A data framewith 15 and 2 variables
NULL


#' Capital Costs
#'
#' Input assumptions to EIA's National Energy Model System, which is used to produce the Annual
#' Energy Outlook report, for 1996-2015.
#'
#' Tables from each report appear under the name 'Cost and Performance Characteristics of New
#' Central Station Electricity Generating Technologies' and were manually compiled into one table.
#'
#' The processing for this data is contained in an Excel workbook within the data-raw/costs/ directory. The manually
#' compiled data is contained in the sheet named "AEOData". As this data is based on expectations of construction costs,
#' there are some spikes in these numbers; "AEOData_$2010_Smoothed" contains the data with these spikes smoothed out and
#' converted into USD$2010. "Data4GCAM" is the final capital costs dataset reported in this package, with the native AEO
#' technology categories mapped to our own set of technologies.
#'
#'
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{technology}{Technology label, native to EIA}
#' \item{overnight}{Base overnight costs, in $/kW}
#' \item{om.var}{Variable operations & management costs, in $/kW/yr}
#' \item{om.fixed}{Fixed operations & management costs, in $/MWh}
#' \item{heatrate}{Heatrate, BTU/kWh}
#' }
#' @name capitalcosts
#' @usage data(capitalcosts)
#' @format A data framewith 323 and 6 variables
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
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{fuel.general}{Fuel category}
#' \item{fuel.price}{Fuel price, in $/BTU}
#' }
#' @name fuelprices
#' @usage data(fuelprices)
#' @format A data framewith 94 and 3 variables
#' @source \url{https://www.eia.gov/electricity/monthly/backissues.html}
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
#' \describe{
#' \item{yr}{Year of reported data}
#' \item{plntcode}{EIA-assigned plant code}
#' \item{overnightcategory}{Technology category}
#' \item{fuel.general}{Fuel category}
#' \item{LCOE_Capital}{Levelized overnight cost, in $/MWh}
#' \item{LCOE_FOM}{Levelized fixed O&M, in $/MWh}
#' \item{LCOE_VOM}{Variable O&m, in $/MWh}
#' \item{LCOE_wo_Fuel}{Capital + FOM + VOM, in $/MWh}
#' \item{LCOE_Fuel}{Fuel price adjusted to AEO-reported heatrate, in $/MWh}
#' \item{LCOE}{LCOE_wo_Fuel + LCOE_Fuel, in $/MWh}
#' }
#' @name levelizedcosts
#' @usage data(levelizedcosts)
#' @format A data framewith 68506 and 11 variables
NULL
