# how many plants' heat_rate is replaced with ca.elec.almanac?
test <- form860processed %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>%
  mutate(heat_rate = ifelse(heat_rate==0, NA, heat_rate)) %>%
  filter(is.na(heat_rate) & !is.na(heat.rate) & heat.rate!=0)
nrow(test)



summ <- function(df, col) {
  print(paste('Total rows: ', nrow(df)))

  na <- subset(df, is.na(df[,col]))
  if (nrow(na) == 0 ) {
    print('No NA years/rows')
  } else {
    print(paste('NA years: ', paste(levels(factor(na$year)), collapse=' ')))
    print(paste('Num NA rows: ', nrow(na)))
  }

  zer <- subset(df, df[,col]==0)
  if (nrow(zer) == 0 ) {
    print('No zero years/rows')
  } else {
    print(paste('Zero years: ', paste(levels(factor(zer$year)), collapse=' ')))
    print(paste('Num Zero rows: ', nrow(zer)))
  }

  num <- subset(df, !is.na(df[,col]) & df[,col]!=0)
  if (nrow(num) == 0 ) {
    print('No Data years/rows')
  } else {
    print(paste('Data years: ', paste(levels(factor(num$year)), collapse=' ')))
    print(paste('Num Data rows: ', nrow(num)))
  }
}


summ(form860processed, 'heat_rate')


summ(ca.elec.almanac, 'heat.rate')


summ(form860CAsupplemented, 'heat_rate')
