library(readxl)
mortality <- read_excel("CCE2022pred.xlsx", sheet = "ADPImortality")
mortality <- mortality %>%
  rename(total_alive = 5, total_dead = 6, bag_no = 4) %>%
  mutate(total_alive = as.numeric(total_alive)) %>%
  mutate(total_dead = as.numeric(total_dead)) %>%
  mutate(first = 1-(total_dead/total_alive)) 
mort <- mortality %>%
  filter(site != "Mattituck") %>%
  group_by(site, bag_no) %>%
  mutate(S_t = cumprod(first)) %>%
  ungroup() %>%
  group_by(site,time) %>%
  mutate(mean_survival = mean(S_t)) 
#getting average mortality in bags of ADPI scallops (august trial time
#point 6, september trial timepoint 9)
means <- mort %>% group_by(time, site) %>% 
  summarize(avg = mean(first), n = n(), 
            sd = sd(first), se = sd/sqrt(n))

predtimes <- means %>%
  filter(time == 6 | time == 9)

#population

population <- read_excel("CCE2022pred.xlsx", sheet = "population2")

unique(population$species)
preds <- population %>% group_by(species, site, date) %>% 
  filter(species!="Argopecten irradians")%>%
  summarize(n = n())
ggplot(preds, aes(x=species, y=n)) + geom_bar(stat = "identity")
#so many more overall predators in orient and nw than cy. mattituck 
#hard to say bc only 2 time periods


#looking at all times

fullpop <- read_excel("CCE2022new.xlsx", sheet = "population")
longpop <- fullpop %>%
  pivot_longer(cols = -c(date, site, transect, direction, diver, bottom_type, perc_SAV, `crepidula shell`))

#how many scallops
ggplot(longpop, aes(x=species, y=n)) + geom_bar(stat = "identity")

#how many predators
predsall <- longpop %>% filter(!str_detect(name, "clucker")) %>%
           filter(!str_detect(name, "count")) %>%
  filter(site != "BullheadBay") %>%
  mutate(month = month(date)) %>%
  mutate(MonthName = month.name[month]) %>%
  mutate(MonthName = factor(MonthName, levels = c('May', 'June', 'July', 'August', 'September', 'October')))

predsall <- as.data.frame(predsall)

predsall$name <- recode(predsall$name, "blue_crab" = "Blue crab", "channelled_whelk" = "Channeled whelk",
         "flatclaw" = "Flat-clawed hermit crab", "fluke" = "Summer flounder", "horseshoe crab" = "Horseshoe crab", 
         "knobbed_whelk" = "Knobbed whelk", "osyter drill" = "Oyster drill", "scup" = "Scup", 
         "sea robin" = "Sea robin", "spider crab" = "Spider crab", "tatuog" = "Tautog")

#graph grouped bar plot by date faceted by site

ggplot(predsall, aes(x=MonthName, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_rep_wrap(~site, scales = 'free_x', repeat.tick.labels = 'left') +
  theme_minimal() +  scale_fill_brewer(palette="RdYlBu") + xlab("Month") +
  ylab("Number observed") + labs(fill = "Predator species") + ggtitle("Monthly predator surveys by site") +
  theme(axis.text.x = element_text(angle = 45))
