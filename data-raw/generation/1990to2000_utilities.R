prep.generation.90to00 <- function(startingDir)
{
  filez<-list.files(startingDir,pattern=".xls")
  generation.90to00 <- data.frame()
  for (i in seq(1:length(filez)) ) {
    file <- filez[i]
    yr.ind <- str_extract(file, "[1|2][0-9]{3}" )

    data.raw <- read_excel(paste0(startingDir, file), sheet=1) %>%
      mutate(yr = as.numeric(yr.ind) ) %>%     # modify in place (originally 2 digits)
      filter(is.na(STATUS) | STATUS == "A" ) %>%     # active (" ") or new addition ("A")
      select(-starts_with("STK")) %>%
      mutate(FUELTYP = ifelse(is.na(FUELTYP), "G", FUELTYP))

    ## ELECTRICITY GENERATION
    if (yr.ind >= 1996) {
      # after 1996, some had monthly reporting req's -- others annual. the monthly reports use estimates
      # for those with annual reporting reqs.
      # for this reason, we just use the annual reports after 1996.
      # monthly vs annual reports name "generation" col differently
      gencols <- "NETGEN"
    } else {
      # sum over 12 GEN## columns (12 monthly reports contained in one workbook)
      gencols <- "GEN"
    }
    data.raw <- mutate_at(data.raw, vars(starts_with(gencols)), as.numeric )
    data.raw$total.generation <- data.raw %>%
      select( starts_with(gencols) ) %>%
      rowSums(.)
    data.raw <- data.raw %>%
      select(-starts_with(gencols) ) %>%
      rename(generation=total.generation)

    ## FUEL CONSUMPTION
    # "consumption" col isn't renamed after 1996
    data.raw <- mutate_at(data.raw, vars(starts_with("CON")), as.numeric )
    data.raw$total.consumption <- data.raw %>%
      select( starts_with("CON") ) %>%
      rowSums(.)
    data.raw <- data.raw %>%
      select(-starts_with("CON") ) %>%
      rename(consumption=total.consumption)

    ## TRUNCATE
    data.filter <- data.raw %>%
      select(PMOVER, PMDESC, FUELTYP, FUELDESC, yr, generation, consumption, UTILCODE, PCODE ) %>%
      mutate(PMOVER = as.integer(PMOVER)) %>%
      mutate(PMDESC = as.character(PMDESC)) %>%
      mutate(FUELTYP = as.character(FUELTYP)) %>%
      mutate(FUELDESC = as.character(FUELDESC)) %>%
      rename(utilcode = UTILCODE) %>%
      mutate(utilcode = str_replace(as.character(utilcode), "^[0]+", "")) %>%
      rename(plntcode = PCODE) %>%
      mutate(plntcode = str_replace(as.character(plntcode), "^[0]+", ""))

    ## MAP FROM FORM759 TO FORM860 CODES
    # many entries have a mismatch between the two native cols used to identify PM and FUEL
    # inner_join() filters through only rows where both cols match the mapping given in the form's documentation
    # then replaces the two cols with a col whose codes correspond to the form860 mapping files
    mapping.pm <- read.csv(paste0(startingDir, "movermapping.csv")) %>%
      select(PMOVER, PMDESC, primemover) %>%
      mutate(PMDESC = as.character(PMDESC))
    mapping.fuel <- read.csv(paste0(startingDir, "fuelmapping.csv")) %>%
      select(FUELTYP, FUELDESC, fuel, toBtu) %>%
      mutate(FUELTYP = as.character(FUELTYP)) %>%
      mutate(FUELDESC = as.character(FUELDESC))
    convfuel <- mapping.fuel$fuel[!is.na(mapping.fuel$toBtu)] # convert toBtu fuels with conversion factor
    data.filter <- data.filter %>%
      inner_join( mapping.pm, by=c("PMOVER", "PMDESC") ) %>%
      select(-PMOVER, -PMDESC) %>%
      mutate(primemover = as.character(primemover)) %>%
      inner_join( mapping.fuel, by=c("FUELTYP", "FUELDESC")) %>%
      select(-FUELTYP, -FUELDESC) %>%
      mutate(fuel = as.character(fuel)) %>%
      mutate(consumption = ifelse(fuel %in% convfuel, consumption*toBtu, consumption)) %>%
      select(-toBtu)

    ## RECORD NATIVE MISMAPPING  AND FUELS
    mapping.pm <- data.raw %>%
      select(PMOVER, PMDESC) %>%
      group_by(PMOVER, PMDESC) %>%
      summarise(n=n()) %>%
      arrange(PMOVER)
    mapping.fuel <- data.raw %>%
      select(FUELTYP, FUELDESC) %>%
      group_by(FUELTYP, FUELDESC) %>%
      summarise(n=n()) %>%
      arrange(FUELTYP)

    ## SAVE/APPEND
    if (i == 1) {
      generation.90to00 <- data.filter
      #write.table(data.filter, file=paste0(startingDir, "../aggregation_90_00.csv"), sep=",",  append=FALSE, row.names=FALSE)
      write.table(mapping.pm, file=paste0(startingDir, "movermismapping.csv"), sep=",",  append=FALSE, row.names=FALSE)
      write.table(mapping.fuel, file=paste0(startingDir, "fuelmismapping.csv"), sep=",",  append=FALSE, row.names=FALSE)
    } else {
      generation.90to00 <- rbind (generation.90to00, data.filter)
      #write.table(data.filter, file=paste0(startingDir, "../aggregation_90_00.csv"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE)
      write.table(mapping.pm, file=paste0(startingDir, "movermismapping.csv"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE )
      write.table(mapping.fuel, file=paste0(startingDir, "fuelmismapping.csv"), sep=",", append=TRUE, row.names=FALSE, col.names= FALSE )
    }
    ## DISTINCT MAPPING CODES ON LAST RUN
    if (file == filez[length(filez)]) {
      # prime mover
      read.csv(paste0(startingDir, "movermismapping.csv")) %>%
        group_by(PMOVER, PMDESC) %>%
        summarise(n=sum(n)) %>%
        write.csv(file=paste0(startingDir, "movermismapping.csv"), row.names=FALSE)
      # fuel
      read.csv(paste0(startingDir, "fuelmismapping.csv")) %>%
        group_by(FUELTYP, FUELDESC) %>%
        summarise(n=sum(n)) %>%
        write.csv(file=paste0(startingDir, "fuelmismapping.csv"), row.names=FALSE)
    }
  }
  generation.90to00
}
