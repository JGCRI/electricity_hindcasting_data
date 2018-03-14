setwd("C:/users/guti220/desktop/energy.markets")
data(cap.gen.joined, capacity.unmapped, capacityfactors, capacityfactors.clamp,
     capitalcosts, fuelprices, generation.unmapped,
     levelizedcosts, mapping, marginalcosts, master)

df.list <- list(cap.gen.joined, capacity.unmapped, capacityfactors, capacityfactors.clamp,
                capitalcosts, fuelprices, generation.unmapped,
                levelizedcosts, mapping, marginalcosts, master)

datasets <- c("cap.gen.joined",
              "capacity.unmapped",
              "capacityfactors",
              "capacityfactors.clamp",
              "capitalcosts",
              "fuelprices",
              "generation.unmapped",
              "levelizedcosts",
              "mapping",
              "marginalcosts",
              "master")

titles <-  c("Joined Electrical Capacity and Generation Output",
          "Electrical Capacity, Unmapped",
          "Capacity Factors",
          "Capacity Factors, clamped to 1 from above",
          "Capital Costs",
          "Fuel Prices",
          "Electricity Generation Net Output, Unmapped",
          "Levelized Costs",
          "Mapping to GCAM Fuel-Tech Options",
          "Marginal Costs",
          "Master Dataset")

cols <- c(yr = "Year of reported data",
         utilcode = "EIA-assigned utility code",
         plntcode = "EIA-assigned plant code",
         vintage = "First year capacity is available",
         primemover = "Native EIA prime mover code",
         fuel = "Native EIA fuel code",
         overnightcategory = "Technology category",
         fuel.general = "Fuel category",
         nameplate = "Reported nameplate capacity, MW",
         generation = "Reported net electrical output, MWh",
         capacityfactor = "8760*capacity/generation, unitless",
         overnight.lev = "Levelized overnight cost, in $/MWh",
         om.fixed.lev = "Levelized fixed O&M, in $/MWh",
         om.var = "Variable O&m, in $/MWh",
         marginal.cost = "Marginal cost of producing electricity using specified technology and fuel, in $/MWh")


sink("inst/documentation/data.doc.txt") # log
cat("DATA DOCUMENTATION \n\n\n")

for (i in 1:length(df.list)) {
  df <- df.list[[i]]
  cat("#'", titles[i], "\n")
  cat("#' \n")
  cat("#' \\describe { \n")
  for (col in names(df)) {
    cat("#' \\item{", col, "}", sep="")
    cat("{")
    if (col %in% names(cols)) {
      cat(cols[[col]])
    }
    cat("}")
    cat("\n")
  }
  cat("#' }")
  cat("\n")
  cat("#' @name", datasets[i])
  cat("\n")
  cat("#' @usage data(", datasets[i], ")", sep="")
  cat("\n")
  cat("#' @format A data framewith", nrow(df), "rows and", ncol(df), "variables")
  cat("\n")
  cat("NULL")
  cat("\n\n\n")
}

sink() # end logging
