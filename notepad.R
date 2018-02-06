

# package data  -----------------------------------------------------------

data(generators)
generators %>%
  group_by(yr) %>%
  summarise(nameplate=mean(nameplate)) %>%
  ungroup() %>%
  View("mapped yearly avg")

generators %>%
  group_by(yr) %>%
  summarise(nameplate=max(nameplate)) %>%
  ungroup() %>%
  View("unmapped yearly max")

generators %>%
  group_by(yr) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup() %>%
  View("unmapped yearly total")

# pre-id.swap -------------------------------------------------------------
data(generators.unmapped)
b <- generators.unmapped %>%
  group_by(yr) %>%
  summarise(nameplate=mean(nameplate)) %>%
  ungroup() #%>%
  # View("unmapped yearly avg")

generators.unmapped %>%
  group_by(yr) %>%
  summarise(nameplate=max(nameplate)) %>%
  ungroup() %>%
  View("unmapped yearly max")

generators.unmapped %>%
  group_by(yr) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup() %>%
  View("unmapped yearly total")



# sent to wonjun ----------------------------------------------------------

df <- read.csv("C:/Users/guti220/Downloads/generators.csv")

a <- df %>%
  group_by(yr) %>%
  summarise(nameplate=mean(nameplate)) %>%
  ungroup() #%>%
  # View("(sent) yearly avg")

df %>%
  group_by(yr) %>%
  summarise(nameplate=max(nameplate)) %>%
  ungroup() %>%
  View("(sent) yearly max")

df %>%
  group_by(yr) %>%
  summarise(nameplate=sum(nameplate)) %>%
  ungroup() %>%
  View("(sent) yearly total")

# create mapping ----------------------------------------------------------
# get prime mover codes' text exp
pm.text <- read.delim("data-raw/mappingfiles/overnight_categories.tsv") %>%
  select(primemover=prime_mover, text=prime_mover_text) %>%
  distinct()

# get unique pm-f comobs
data(generators.unmapped)
combos <- generators.unmapped %>%
  select(primemover, fuel) %>%
  distinct()

# attach text to codes that appear in combos
pm.text <- combos %>%
  select(primemover) %>% unique() %>%
  inner_join(pm.text, by=c("primemover"))
write.csv(pm.text, "data-raw/generators/movers.txt", row.names=F)
# add tech column by inspecting that file

# now can combine
fuels <- read.csv("data-raw/generators/fuels.csv", stringsAsFactors=FALSE) %>%
  select(fuel, fuel.general)

tech <- read.csv("data-raw/generators/movers.csv", stringsAsFactors=FALSE) %>%
  select(primemover, tech)

## COMBINE
mapping <- combos %>%
  left_join(fuels, by="fuel") %>%
  left_join(tech, by="primemover") %>%
  mutate(tech = paste0(fuel.general, " (", tech, ")"))

## ADDED TO GENERATEDATA.R AND SAVED AS .RDA


# map merged data ---------------------------------------------------------

merged <- read.delim("C:/Users/guti220/Downloads/merged.tsv")
data(mapping.full)
merged.map <- merged %>%
  left_join(mapping.full, by=c("fuel", "primemover"))
write.csv(merged.map, "merged.map.csv", row.names=FALSE)
write.csv(mapping.full, "mapping.csv", row.names=FALSE)
