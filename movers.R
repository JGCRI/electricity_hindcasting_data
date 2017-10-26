form860raw <- read.delim("data-raw/form860raw.tsv")
form860raw <- as.vector(unique(form860raw[,'prime_mover']))
# 37 movers
# has '' & 'ic'
write.csv(form860raw, 'raw.csv')


form860CAsupplemented <- as.vector(unique(form860CAsupplemented[,'prime_mover']))
# 34 movers
write.csv(form860CAsupplemented, 'sup.csv')

form860CAsupplemented[ ! form860CAsupplemented %in% form860raw]

movers <- read.csv('data-raw/form860movers.csv')
movers <- as.vector(unique(movers[,'prime_mover']))

movers[ ! movers %in% form860CAsupplemented] # none in reverse
form860raw[ ! form860raw %in% form860CAsupplemented] # none in reverse

movers[! movers %in% form860raw] # NG, OC, VR never show up
form860raw[! form860raw %in% movers] # '', and lowercase 'ic'
