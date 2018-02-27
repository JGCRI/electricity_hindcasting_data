setwd("C:/users/guti220/desktop/energy.markets")
data(cap.gen.joined, capacity.unmapped, capacityfactors, capacityfactors.clamp,
     capitalcosts, fuelprices, generation.unmapped,
     levelizedcosts, mapping, marginalcosts)

df.list <- list(cap.gen.joined, capacity.unmapped, capacityfactors, capacityfactors.clamp,
                capitalcosts, fuelprices, generation.unmapped,
                levelizedcosts, mapping, marginalcosts)

datasets <- c("cap.gen.joined",
              "capacity.unmapped",
              "capacityfactors",
              "capacityfactors.clamp",
              "capitalcosts",
              "fuelprices",
              "generation.unmapped",
              "levelizedcosts",
              "mapping",
              "marginalcosts")

titles <-  c("Joined Electrical Capacity and Generation Output",
          "Electrical Capacity, Unmapped",
          "Capacity Factors",
          "Capacity Factors, clamped to 1 from above",
          "Capital Costs",
          "Fuel Prices",
          "Electricity Generation Net Output, Unmapped",
          "Levelized Costs",
          "Mapping to GCAM Fuel-Tech Options",
          "Marginal Costs")
cols <- c("Year of reported data",
         "EIA-assigned utility code",
         "EIA-assigned plant code",
         "First year capacity is available",
         "Native EIA prime mover code",
         "Native EIA fuel code",
         "Technology category",
         "Fuel category",
         "Reported nameplate capacity, MW",
         "Reported net electrical output, MWh",
         "8760*capacity/generation, unitless")
names(cols) <- c("yr",
                 "utilcode",
                 "plntcode",
                 "vintage",
                 "primemover",
                 "fuel",
                 "overnightcategory",
                 "fuel.general",
                 "nameplate",
                 "generation",
                 "capacityfactor")
sink("data.doc.txt") # log
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
  cat("#' @format A data framewith", nrow(df), "and", ncol(df), "variables")
  cat("\n")
  cat("NULL")
  cat("\n\n\n")
}

sink() # end logging
