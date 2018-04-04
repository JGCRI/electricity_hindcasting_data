# returns all rows in cap, inserts NA where no match found
leftjoin.cap.gen.unmap <- function(cap, gen)
{
  # merge for ! yr %in% c(2001, 2002)
  # join includes primemover
  join.rest <- cap %>%
    filter(! yr %in% c(2001, 2002)) %>%
    left_join( gen, # join unmapped datasets W/O UTILCODE
          by = c("yr", "plntcode", "primemover", "fuel") ) %>%
    select(-starts_with("utilcode"), -consumption) # drop ORIG CAP/GEN utilcodes, & consumption

  # merge for yr %in% c(2001, 2002)
  # join excludes primemover, so we opt to use CAP pm column in later aggregation
  join.01.02 <- cap %>%
    filter( yr %in% c(2001, 2002)) %>%
    left_join( gen, # join unmapped datasets by DROPPING PRIMEMOVER
          by = c("yr", "plntcode", "fuel") ) %>%
    dplyr::rename(primemover = primemover.x) %>%  #use ORIG CAP pm column
    select(-primemover.y, -starts_with("utilcode"), -consumption) # drop ORIG GEN pm column & consumption, ORIG CAP/GEN utilcodes

  # bind joined data sets together
  join.unmapped <- rbind(join.rest, join.01.02)
}

# returns rows in cap that don't find match in gen
antijoin.cap.gen.unmap <- function(cap, gen)
{
  # merge for ! yr %in% c(2001, 2002)
  # join includes primemover
  join.rest <- cap %>%
    filter(! yr %in% c(2001, 2002)) %>%
    anti_join( gen, # join unmapped datasets W/O UTILCODE
          by = c("yr", "plntcode", "primemover", "fuel") ) %>%
    select(-starts_with("utilcode")) # drop ORIG CAP/GEN utilcodes, & consumption

  # merge for yr %in% c(2001, 2002)
  # only columns from cap so don't have to worry about .x & .y
  join.01.02 <- cap %>%
    filter( yr %in% c(2001, 2002)) %>%
    anti_join( gen, # join unmapped datasets by DROPPING PRIMEMOVER
          by = c("yr", "plntcode", "fuel") ) %>%
    select(-starts_with("utilcode")) # drop ORIG GEN pm column & consumption, ORIG CAP/GEN utilcodes

  # bind joined data sets together
  join.unmapped <- rbind(join.rest, join.01.02)
}

# returns rows in gen that don't find match in cap
antijoin.gen.cap.unmap <- function(gen, cap)
{
  # merge for ! yr %in% c(2001, 2002)
  # join includes primemover
  join.rest <- gen %>%
    filter(! yr %in% c(2001, 2002)) %>%
    anti_join( cap, # join unmapped datasets W/O UTILCODE
               by = c("yr", "plntcode", "primemover", "fuel") ) %>%
    select(-starts_with("utilcode"), -consumption) # drop ORIG CAP/GEN utilcodes, & consumption

  # merge for yr %in% c(2001, 2002)
  # only columns from gen so don't have to worry about .x & .y
    join.01.02 <- gen %>%
    filter( yr %in% c(2001, 2002)) %>%
    anti_join( cap, # join unmapped datasets by DROPPING PRIMEMOVER
               by = c("yr", "plntcode", "fuel") ) %>%
    select(-utilcode, -consumption)

  # bind joined data sets together
  join.unmapped <- rbind(join.rest, join.01.02)
}


leftjoin.cap.gen <- function(cap, gen, join) {

  # map to oc-fg, and aggregate
  join.cap.gen.mapped <- join.cap.gen.unmap(cap, gen, join) %>%
      left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
      select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
      # no longer grouping by utilcode b/c two versions (.x, .y)
      group_by(yr, plntcode, overnightcategory, fuel.general) %>%
      summarise(capacity = sum(capacity),
                generation = sum(generation)) %>%
      ungroup()
}
