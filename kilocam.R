KC <- read.csv("Kilocam_aug.csv")

N <- KC %>%
  group_by(site, species) %>%
  summarize(n = n())
  