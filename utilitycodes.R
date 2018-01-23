## c(DGT, "")
# p <- c(1241, 2078, 2079, 2080, 2081, 6065)
# p <- c(3594, 3595, 3597:3601, 4937, 6179)

## C(DGT, DGT) & c(DGT, DGT, "")
p <- c(899, 1079, 1081, 6664, 1082, 1084, 1091, 1097, 6063, 7145, 8029)
nad <- c(51406)

# look at ucodes assigned to (pcode, nad)-keys
test <- generation.90to00 %>%
  mutate(utilcode = ifelse(utilcode=="", '(BLANK)', utilcode)) %>%
  group_by(plntcode) %>%
  summarise(num.ucodes = length(unique(utilcode)),
            ucodes = paste0(sort(unique(utilcode)), collapse=", ")) %>%
  ungroup() %>%
  #filter(NAD != 0)
  # filter(! str_detect(ucodes, "(BLANK)"))
  # filter(plntcode %in% p & NAD == nad)
  filter(plntcode == 1082)
View(test)

# OUTPUT - look at spread of codes across yrs
test2 <- generation.90to00 %>%
  filter(plntcode %in% c(6063, 7145, 8029)) %>%
  #select(yr, plntcode, utilcode, NAD) %>%
  #distinct() %>%
  mutate(plntcode = as.numeric(plntcode))
View(test2)

# CAPACITY - look at spread of codes across yrs
test3 <- generators %>%
  #filter(plntcode %in% p) %>%
  group_by(plntcode, yr) %>%
  summarise(cap=sum(nameplate)) %>%
  ungroup()  %>%
  mutate(plntcode = as.numeric(plntcode)) %>%
  spread(key=yr, value=cap, -utilcode)
View(test3)

test3a <- generators.01to16 %>%
  rbind(generators.90to00) %>%
  group_by(plntcode, yr) %>%
  summarise(cap=sum(nameplate)) %>%
  ungroup() %>%
  mutate(plntcode = as.numeric(plntcode)) %>%
  spread(key=yr, value=cap)
View(test3a)

# look at NAD assigned to (pcode)-key
test4 <- generation.90to00 %>%
  mutate(NAD = ifelse(NAD=="", '(BLANK)', NAD)) %>%
  group_by(plntcode) %>%
  summarise(num.nads = length(unique(NAD)),
            nads = paste0(sort(unique(NAD)), collapse=", ")) %>%
  ungroup() %>%
  filter(num.nads == 2 & str_detect(nads, "0,")) %>%
  mutate(NAD.new = str_split(nads, ", ", simplify=TRUE)[,2]) %>%
  select(pcode=plntcode, NAD.new)
View(test4)

# for all plants assigned NADs=(0, some #), fill in # instead of 0
p <- unique(test4$plntcode)
test5 <- generation.90to00 %>%
  mutate(NAD = ifelse(plntcode %in% test4$pcode, as.character(test4[test4$pcode==plntcode, "NAD.new"]), NAD)) %>%
  filter(plntcode %in% p)
View(test5)
