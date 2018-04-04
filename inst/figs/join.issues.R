data(capacity.unmapped, generation.unmapped, cap.gen.joined.unmapped)

capacity.unmapped <- capacity.unmapped %>%
  group_by(yr, utilcode, plntcode, primemover, fuel) %>%
  summarise(capacity=sum(capacity)) %>%
  ungroup()


# nrow() ------------------------------------------------------------------


capacity.unmapped %>% select(yr, plntcode, primemover, fuel) %>% distinct() %>% nrow()
generation.unmapped %>% select(yr, plntcode, primemover, fuel) %>% distinct() %>% nrow()
cap.gen.joined.unmapped %>% select(yr, plntcode, primemover, fuel) %>% distinct() %>% nrow()


capacity %>% select(yr, plntcode, overnightcategory, fuel.general) %>% distinct() %>% nrow()
generation %>% select(yr, plntcode, overnightcategory, fuel.general) %>% distinct() %>% nrow()
cap.gen.joined %>% select(yr, plntcode, overnightcategory, fuel.general) %>% distinct() %>% nrow()





# UNMAP JOIN --------------------------------------------------------------
data(capacity.unmapped, generation.unmapped, cap.gen.joined.unmapped)
capacity.unmapped <- capacity.unmapped %>%
  group_by(yr, utilcode, plntcode, primemover, fuel) %>%
  summarise(capacity=sum(capacity)) %>%
  ungroup()
cap.gen.leftjoin <- leftjoin.cap.gen.unmap(capacity.unmapped, generation.unmapped)
gen.cap.antijoin <- antijoin.gen.cap.unmap(generation.unmapped, capacity.unmapped)
cap.gen.antijoin <- antijoin.cap.gen.unmap(capacity.unmapped, generation.unmapped)

# check that join keeps 100% of ORIG CAP rows (should)
r <- nrow(cap.gen.leftjoin)
if (r == nrow(capacity.unmapped)) {
  print("# rows in LEFT_JOIN = # rows in ORIG CAP")

  # check if two joins are consistent with ORIG CAP (might)
  na <- sum(is.na(cap.gen.leftjoin$generation))
  if (na + nrow(cap.gen.joined.unmapped) == nrow(capacity.unmapped)) {
    print("# rows in LEFT_JOIN missing GEN + # rows in INNER_JOIN = # rows in ORIG CAP ")

    # LEFT_JOIN passed the nested test
    # nrow(ORIG CAP) - nrow(INNER_JOIN.CAP.GEN) = nrow(LEFT_JOIN.CAP.GEN[GEN=NA])
    # this can also be confirmed w/ the following test
    # nrow(LEFT_JOIN.CAP.GEN[GEN=NA]) = nrow(ANTI_JOIN.GEN.CAP)
    nrow(cap.gen.antijoin) == na

  } else {
    print("# rows in LEFT_JOIN missing GEN + # rows in INNER_JOIN != # rows in ORIG CAP ")

  }
} else {
  print("# rows in LEFT_JOIN != # rows in ORIG CAP")
}



# MAP JOIN ----------------------------------------------------------------


cap.gen.left_join <- join.cap.gen(capacity.unmapped, generation.unmapped, left_join) %>%
  arrange(yr, plntcode, overnightcategory, fuel.general, capacity, generation)
# check that join keeps 100% of ORIG CAP rows
r <- nrow(cap.gen.left_join)
if (r == nrow(capacity)) {
  print("# rows in MAP LEFT_JOIN = # rows in MAP CAP")

  # check that two joins are consistent with ORIG CAP
  na <- sum(is.na(cap.gen.left_join$generation))
  if (na + nrow(cap.gen.joined) == nrow(capacity)) {
    print("# rows in MAP LEFT_JOIN missing GEN + # rows in MAP INNER_JOIN = # rows in MAP CAP ")
  } else {
    print("# rows in MAP LEFT_JOIN missing GEN + # rows in MAP INNER_JOIN != # rows in MAP CAP ")
  }


} else {
  print("# rows in LEFT_JOIN != # rows in ORIG CAP")
}




