prep.generation.01to16 <- function(startingDir)
{
  # documentation on the layout of each dataset is found in the third tab of each xls file
  filez<-list.files(startingDir,pattern=".xls")

  for (i in seq(1:length(filez)) ) {
    file <- filez[i]

    yr <- str_extract(file, "[1|2][0-9]{3}" ) %>% as.integer()
    if (yr >= 2011) {skiplines <- 5
    } else {skiplines <- 7}

    data.raw <- read_excel(paste(startingDir, file, sep="/"), sheet=1, skip=skiplines)
    names(data.raw) <- names(data.raw) %>%
      toupper() %>%
      gsub("AND", "&", .) %>%
      gsub("_", ".", .) %>%
      gsub("MMBTUS", "MMBTU", .) %>%
      make.names() %>%
      gsub(one_or_more("."), ".", .)

    data.filter <- data.raw %>%
      select(year = YEAR,
             plant_code = PLANT.ID,
             utility_code = OPERATOR.ID,
             chp = COMBINED.HEAT.POWER.PLANT,
             prime_mover = REPORTED.PRIME.MOVER,
             fuel = REPORTED.FUEL.TYPE.CODE,
             generation = NET.GENERATION.MEGAWATTHOURS.,
             consumption = ELEC.FUEL.CONSUMPTION.MMBTU)  %>%
      filter(chp == 'N') %>% # no co-generators
      select(-chp) %>%
      mutate(fuel = ifelse(fuel %in% c("MSB", "MSN"), "MSW", fuel)) %>%
      # at some point, they began delineating types of MSW, but these new codes won't match 860
      mutate(plant_code = as.integer(plant_code)) %>%
      filter(plant_code != 99999) # no estimates of plants that didn't respond

    data.filter$consumption <- data.filter$consumption*1e6 # MMBtu -> Btu
    ## DON'T NEED TO REMAP CODES BECAUSE THIS FORM MATCHES FORM860 FUEL/MOVER CODES

    ## RECORD NATIVE MISMAPPING
    # two movers entirely unused (CE and FC)
    # about 25% of all rows are missing a mover code
    mapping.pm <- data.filter %>%
      select(prime_mover) %>%
      group_by(prime_mover) %>%
      summarise(n=n()) %>%
      arrange(prime_mover)
    # a couple of erroneous fuel codes have been recorded.
    # only rows with codes matching 860 will be combined into new dataset
    mapping.fuel <- data.filter %>%
      select(fuel) %>%
      group_by(fuel) %>%
      summarise(n=n()) %>%
      arrange(fuel)

    ## SAVE/APPEND
    if (i == 1) {
      generation.01to16 <- data.filter
      #write.table(data.filter, file=paste(startingDir, "../aggregation_01_16.csv", sep="/"), sep=",",  append=FALSE, row.names=FALSE)
      write.table(mapping.pm, file=paste(startingDir, "movermismapping.csv", sep="/"), sep=",",  append=FALSE, row.names=FALSE)
      write.table(mapping.fuel, file=paste(startingDir, "fuelmismapping.csv", sep="/"), sep=",",  append=FALSE, row.names=FALSE)
    } else {
      generation.01to16 <- rbind(generation.01to16, data.filter)
      #write.table(data.filter, file=paste(startingDir, "../aggregation_01_16.csv", sep="/"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE)
      write.table(mapping.pm, file=paste(startingDir, "movermismapping.csv", sep="/"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE )
      write.table(mapping.fuel, file=paste(startingDir, "fuelmismapping.csv", sep="/"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE )
    }

    ## DISTINCT MAPPING CODES ON LAST RUN
    if (file == filez[length(filez)]) {
      # prime mover
      read.csv(paste(startingDir, "movermismapping.csv", sep="/")) %>%
        group_by(prime_mover) %>%
        summarise(n=sum(n)) %>%
        write.csv(file=paste(startingDir, "movermismapping.csv", sep="/"), row.names=FALSE)
      # fuel
      read.csv(paste(startingDir, "fuelmismapping.csv", sep="/")) %>%
        group_by(fuel) %>%
        summarise(n=sum(n)) %>%
        write.csv(file=paste(startingDir, "fuelmismapping.csv", sep="/"), row.names=FALSE)
    }
  }
  generation.01to16
}
