mer3.cap.total <- mer3 %>%
  group_by(yr) %>%
  summarise(total.cap = sum(nameplate),
            total.gen = sum(generation)) %>%
  ungroup()

mer3.cap.summary <- mer3  %>%
  group_by(yr, fuel) %>%
  summarise(part.cap = sum(nameplate)) %>%
  ungroup() %>%
  full_join(mer3.cap.total, by=c("yr")) %>%
  mutate(share.cap = round(100 * part.cap / total.cap, 2)) %>%
  select(-part.cap, -total.cap, -total.gen)

mer3.cap.summary.wide <- mer3.cap.summary %>%
  spread(key=fuel, value=share.cap)




# summary: fuel-share of annual cap/gen -----------------------------------

ggplot(mer3.cap.summary, aes(x=yr, y=share.cap, color=fuel)) +
  geom_point() +
  geom_line() +
  ylab("% Annual Capacity")




# summary: total cap/gen --------------------------------------------------

mer3.summary <- mer3 %>%
  group_by(yr) %>%
  summarise(cap.total = sum(nameplate),
            gen.total = sum(generation)) %>%
  ungroup()

cap.avg <- mean(mer3.summary$cap.total)
ggplot(mer3.summary, aes(x=yr, y=cap.total/cap.avg)) +
  geom_line() +
  geom_point() +
  ylab("annual capacity / average average annual capacity") +
  geom_abline(intercept = 1, slope = 0)


gen.avg <- mean(mer3.summary$gen.total)
ggplot(mer3.summary, aes(x=yr, y=gen.total/gen.avg)) +
  geom_line() +
  geom_point() +
  ylab("annual generation / average average annual generation") +
  geom_abline(intercept = 1, slope = 0)
