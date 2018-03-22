total <- cap.gen.joined %>%
  group_by(yr) %>%
  summarise(total.cap = sum(nameplate),
            total.gen = sum(generation)) %>%
  ungroup()

cap.summary <- cap.gen.joined  %>%
  group_by(yr, fuel.general) %>%
  summarise(part.cap = sum(nameplate)) %>%
  ungroup() %>%
  full_join(total, by=c("yr")) %>%
  mutate(share.cap = round(100 * part.cap / total.cap, 2)) %>%
  select(-part.cap, -total.cap, -total.gen)

cap.summary.wide <- cap.summary %>%
  spread(key=fuel, value=share.cap)




# summary: fuel-share of annual cap/gen -----------------------------------

ggplot(cap.summary, aes(x=yr, y=share.cap, color=fuel.general)) +
  geom_point() +
  geom_line() +
  ylab("% Annual Capacity")




# summary: total cap/gen --------------------------------------------------

cap.summary <- cap.gen.joined %>%
  group_by(yr) %>%
  summarise(cap.total = sum(nameplate),
            gen.total = sum(generation)) %>%
  ungroup()

cap.avg <- mean(cap.summary$cap.total)
ggplot(cap.summary, aes(x=yr, y=cap.total/cap.avg)) +
  geom_line() +
  geom_point() +
  ylab("annual capacity / average average annual capacity") +
  geom_abline(intercept = 1, slope = 0)


gen.avg <- mean(cap.summary$gen.total)
ggplot(cap.summary, aes(x=yr, y=gen.total/gen.avg)) +
  geom_line() +
  geom_point() +
  ylab("annual generation / average average annual generation") +
  geom_abline(intercept = 1, slope = 0)
