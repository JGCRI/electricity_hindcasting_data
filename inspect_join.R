capacity.unmapped %>% arrange(plntcode, primemover, fuel, yr) %>% View()

cap.gen.joined.unmapped %>% arrange(plntcode, primemover, fuel, yr) %>% View()

my_summarise <- function(df) {
  print(names(df))
  print(dim(df))
}
my_summarise(capacity.unmapped)
my_summarise(generation.unmapped)
my_summarise(cap.gen.joined.unmapped)




# inspect CAP/GEN join ----------------------------------------------------

# case determines join cols
na_case <- c("yr", "plntcode", "fuel")
not_na_case <- c("yr", "plntcode", "primemover", "fuel")
case <- not_na_case

# case determines GEN subset
if (identical(case, na_case)) {
  gen <- generation.unmapped.na
} else {
  gen <- generation.unmapped.notna
}

# check what's dropped in join
cap_not_gen <- anti_join(capacity.unmapped, gen,
                         by = case)
gen_not_cap <- anti_join(gen, capacity.unmapped,
                         by = case)
# what's dropped from GEN
gen_not_cap %>%
  arrange(plntcode, primemover, fuel, yr) %>% View("gen_not_cap")
# what does that plnt look like to CAP
capacity.unmapped %>%
  filter(plntcode == 10) %>%
  arrange(plntcode, primemover, fuel, yr) %>% View("CAP 10")
