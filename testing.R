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
