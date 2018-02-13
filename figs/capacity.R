library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
# Functions ---------------------------------------------------------------

# display & save stacked bar plot
plot <- function(df, ptitle) {

  p <- ggplot(df, aes(x=yr, y=nameplate)) +
    ggtitle(ptitle) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ylab("MW") +
    scale_y_continuous(labels = scales::scientific)

  # geom_bar()
  if (grepl("fuel", ptitle)) {
    p <- p +
      geom_bar(aes(fill=fuel), stat="identity")
  } else if (grepl("overnight", ptitle)) {
    p <- p +
      geom_bar(aes(fill=overnight), stat="identity")
  }

  # save
  # fn <- paste0("figs/", ptitle, ".png")
  # ggsave(filename=fn, plot=p, device="png", width=11, height=8, units="in")

  return(p)
}

# aggregate by fuel/overnight, then plot
plot.agg <- function(df, ptitle, group) {
  df.grp <- df %>%
    group_by_at(vars(yr, matches(group))) %>%
    summarise(nameplate=sum(nameplate)) %>%
    ungroup()

  df.grp$yr <- factor(df.grp$yr, levels=seq(from=1990, to=2016, by=1))
  if (group=="fuel") {
    df.grp <- df.grp %>%
      group_by(yr, fuel) %>%
      complete(fuel, yr, fill=list(nameplate=0))
  } else if (group=="overnight") {
    df.grp <- df.grp %>%
      group_by(yr, overnight) %>%
      complete(overnight, yr, fill=list(nameplate=0))
  } else {
    stop("Group not recognized!")
  }

  ptitle <- paste0(ptitle, " by ", group)
  p <- plot(df.grp, ptitle)
  return(p)
}

# multiplot w/ one legend function
source("figs/sharedlegend.R")
# 1 2
# 3 4

# multiplot w/ multiple legends
source("figs/multiplot.R")
# 1 3
# 2 4



# Original Dataset --------------------------------------------------------
data(capacity)
# full fleet
orig <- capacity %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr))

orig.fuel <- plot.agg(orig, "ORIG Fleet", "fuel")
orig.oc <- plot.agg(orig, "ORIG Fleet", "overnight")

# new additions
orig.new <- orig %>%
  filter(yr == vintage)

orig.new.fuel <- plot.agg(orig.new, "ORIG Additions",  "fuel")
orig.new.oc <- plot.agg(orig.new, "ORIG Additions", "overnight")



# Wonjun's 'merged' Dataset ---------------------------------------------------------
data(mapping)
merged.tsv <- read.delim("C:/Users/guti220/Desktop/merged.tsv")
mer1 <- merged.tsv %>%
  left_join(mapping, by=c("primemover", "fuel")) %>%
  select(-primemover, -fuel) %>%
  dplyr::rename(fuel = fuel.general,
                overnight = overnightcategory,
                vintage = startyr) %>%
  mutate(overnight = gsub("conventional ", "", overnight)) %>%
  mutate(yr = as.factor(yr)) %>%
  filter(!is.na(generation)) %>%
  group_by(yr, utilcode, plntcode, overnight, fuel, vintage) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup()


# aggregate over vintage for full fleet
mer1.full <- mer1 %>% 
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup() 

# filter for new additions only
mer1.new <- mer1 %>%
  filter(yr == vintage)

mer1.fuel <- plot.agg(mer1.full, "MER1 Fleet", "fuel")
mer1.new.fuel <- plot.agg(mer1.new, "MER1 Additions",  "fuel")
mer1.oc <- plot.agg(mer1.full, "MER1 Fleet", "overnight")
mer1.new.oc <- plot.agg(mer1.new, "MER1 Additions", "overnight")


# yr.utilcode.plntcode.oc.fg ----------------------------------------------
data(capacity, generation)

mer2 <- inner_join( capacity, generation,
                      by = c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general") ) %>% 
  rename(overnight = overnightcategory,
         fuel = fuel.general) 

# aggregate over vintage for full fleet
mer2.full <- mer2 %>% 
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup() 

# filter for new additions only
mer2.new <- mer2 %>%
  filter(yr == vintage)

mer2.fuel <- plot.agg(mer2.full, "MER2 Fleet", "fuel")
mer2.new.fuel <- plot.agg(mer2.new, "MER2 Additions",  "fuel")
mer2.oc <- plot.agg(mer2.full, "MER2 Fleet", "overnight")
mer2.new.oc <- plot.agg(mer2.new, "MER2 Additions", "overnight")



# yr.utilcode.plntcode.pm.f -----------------------------------------------
data(capacity.unmapped, generation.unmapped, mapping)

# merge
mer3 <- inner_join( capacity.unmapped, generation.unmapped, # join unmapped datasets
                            by = c("yr", "utilcode", "plntcode", "primemover", "fuel") ) %>% 
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, utilcode, plntcode, vintage, overnightcategory, fuel.general) %>%
  summarise(nameplate = sum(nameplate) ) %>%
  ungroup() %>% 
  rename(overnight = overnightcategory, # rename for plotting functions
         fuel = fuel.general) 

# aggregate over vintage for full fleet
mer3.full <- mer3 %>% 
  group_by(yr, utilcode, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup() 

# filter for new additions only
mer3.new <- mer3 %>%
  filter(yr == vintage)

mer3.fuel <- plot.agg(mer3.full, "MER3 Fleet", "fuel")
mer3.new.fuel <- plot.agg(mer3.new, "mer3 Additions",  "fuel")
mer3.oc <- plot.agg(mer3.full, "mer3 Fleet", "overnight")
mer3.new.oc <- plot.agg(mer3.new, "mer3 Additions", "overnight")


# yr.plntcode.pm.f --------------------------------------------------------
data(capacity.unmapped, generation.unmapped, mapping)

# merge
mer4 <- inner_join( capacity.unmapped, generation.unmapped, # join unmapped datasets
                    by = c("yr", "plntcode", "primemover", "fuel") ) %>% 
  left_join(mapping, by=c("primemover", "fuel")) %>% # map datasets to oc-fg
  select(-primemover, -fuel) %>% # aggregate redundant mappings (pm, f) -> (oc, fg)
  group_by(yr, plntcode, vintage, overnightcategory, fuel.general) %>% # no longer grouping by utilcode b/c two versions (.x, .y)
  summarise(nameplate = sum(nameplate) ) %>%
  ungroup() %>% 
  rename(overnight = overnightcategory, # rename for plotting functions
         fuel = fuel.general) 


# aggregate over vintage for full fleet
mer4.full <- mer4 %>% 
  group_by(yr, plntcode, overnight, fuel) %>%
  summarise(nameplate = sum(nameplate)) %>%
  ungroup() 

# filter for new additions only
mer4.new <- mer4 %>%
  filter(yr == vintage)

mer4.fuel <- plot.agg(mer4.full, "MER4 Fleet", "fuel")
mer4.new.fuel <- plot.agg(mer4.new, "MER4 Additions",  "fuel")
mer4.oc <- plot.agg(mer4.full, "MER4 Fleet", "overnight")
mer4.new.oc <- plot.agg(mer4.new, "MER4 Additions", "overnight")




# figs/outfile.txt --------------------------------------------------------

sink("figs/outfile.txt") # log

cat("Mapped Capacity Dataset : orig \n")
cat("DF size: ", dim(orig), "\n")
cat(names(orig), "\n")
cat("\n\n")

cat("Wonjun's Merged Dataset: mer1", "\n")
cat("Merged df size: ", dim(mer1), "\n")
cat("Retention: unsure \n")
cat("\n\n")

fulljoin <- full_join(capacity, generation,
                          by = c("yr", "utilcode", "plntcode", "overnightcategory", "fuel.general")) %>% 
  select(yr, utilcode, plntcode, overnightcategory, fuel.general) %>% 
  distinct()
cat("Merge on yr.utilcode.plntcode.oc.fg: mer2 \n")
cat("Merged df size (agg over vintage): ",dim(mer2.full), "\n")
cat("Retention: ", 100 * nrow(mer2.full)/ nrow(fulljoin), "% \n" )
cat("\n\n")

fulljoin.unmapped <- full_join(capacity.unmapped, generation.unmapped,
                               by = c("yr", "utilcode", "plntcode", "primemover", "fuel")) %>% 
  select(yr, utilcode, plntcode, primemover, fuel ) %>% 
  distinct()
cat("Merge on yr.utilcode.plntcode.pm.f: mer3 \n")
cat("Merged df size (agg over vintage): ",dim(mer3.full), "\n")
cat("Retention: ", 100 * nrow(mer3.full)/ nrow(fulljoin.unmapped), "% \n" )
cat("\n\n")

cat("Merge on yr.plntcode.pm.f: mer4 \n")
cat("Merged df size (agg over vintage): ",dim(mer4.full), "\n")
cat("Retention: ", 100 * nrow(mer4.full)/ nrow(fulljoin.unmapped), "% \n" )
cat("\n\n")

cat("Mapped Generation Dataset \n")
cat("DF size: ", dim(generation), "\n")
cat(names(generation), "\n")
cat("\n")

cat("Unmapped Capacity Dataset \n")
cat("DF size: ", dim(capacity.unmapped), "\n")
cat(names(capacity.unmapped), "\n")
cat("\n")

cat("Unmapped Generation Dataset \n")
cat("DF size: ", dim(generation.unmapped), "\n")
cat(names(generation.unmapped), "\n")
cat("\n")

sink() # end logging
# PLOTS -------------------------------------------------------------------

## SAVE ORIG PLOT
fn <- "figs/ORIG.png"
print(paste0("saving ", fn))
png(fn, width=11, height=8.5, units="in", res=250)
multiplot(orig.new.fuel, orig.fuel, orig.new.oc, orig.oc, cols=2)
dev.off()

## SAVE FUEL PLOT
fn <- "figs/Capacity by fuel.png"
print(paste0("saving ", fn))
png(fn, width=11, height=17, units="in", res=250)
grid_arrange_shared_legend(orig.fuel, orig.new.fuel,
                           mer1.fuel, mer1.new.fuel,
                           mer2.fuel, mer2.new.fuel,
                           mer3.fuel, mer3.new.fuel,
                           mer4.fuel, mer4.new.fuel,
                           position="right", ncol=2, nrow=5)
dev.off()


