library(tidyverse)
library(lubridate)
setwd("C:/Users/macgr/Documents/Stony_Brook_Masters/Scallop_project/predation/predation")

pred <- read.csv("2022_mortality.csv")
shell_heights <- read.csv("shell_heights.csv")
str(shell_heights)
shell_heights$shell_height <- as.numeric(shell_heights$shell_height)
head(pred)
str(pred1)

#shell heights
shell_heights %>% group_by(month) %>% 
  summarize(avg = mean(shell_height), n = n(), 
            sd = sd(shell_height), se = sd/sqrt(n))
Aug <- shell_heights %>% filter(month == "August")
Sep <- shell_heights %>% filter(month == "September")
hist(Aug$shell_height)
hist(Sep$shell_height)

t.test(Aug$shell_height, Sep$shell_height, paired = FALSE)
#shell heights diff between aug and september!
#graph this
h <- ggplot(shell_heights, aes(month, shell_height)) + geom_boxplot() + ggtitle("Shell heights by month") +
  xlab("Trial month") + ylab("Shell height (mm)")

h + scale_fill_brewer(palette="Paired") + theme_classic() + 
  theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=12),
        plot.title = element_text(size=22), axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))

#get averages for treatment, compare by treatment and site
#r sd is square root of the variance 
pred1 <- pred %>%
  mutate(Treatment = replace(Treatment, Treatment == "Cage", "Corral")) %>%
  rename("date" = "Date.retrieved") %>%
  mutate(date = mdy(date), month = month(date), "monthname" = month.abb[month]) %>%
  mutate(prop_dead = Dead.after.24.hr/10, prop_alive = Alive.after.24hr/10) %>%
  group_by(Site, Treatment, monthname) %>%
  mutate(mean_proportion_dead = mean(prop_dead), 
         sddead = sd(prop_dead), mean_proportion_alive = mean(prop_alive), sdalive = sd(prop_alive)) %>%
  ungroup() %>%
  group_by(Treatment, monthname) %>%
  mutate(mean_proportion_deadMT = mean(prop_dead), 
         sddeadMT = sd(prop_dead), mean_proportion_aliveMT = mean(prop_alive), sdaliveMT = sd(prop_alive)) %>% 
  ungroup() %>%
  group_by(Site, monthname) %>%
  mutate(mean_proportion_deadMS = mean(prop_dead), 
         sddeadMS = sd(prop_dead), mean_proportion_aliveMS = mean(prop_alive), sdaliveMS = sd(prop_alive)) 
#pulling out some quick stats
meansMS <- unique(pred1$mean_proportion_aliveMS)
meansMS

#plot
dummy2 <- data.frame(monthname = c("Aug", "Sep"), Z = c(0.9133, 0.9467))

p <- ggplot(pred1, aes(x=Site, y=mean_proportion_alive, fill=Treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_proportion_alive-sdalive, ymax=mean_proportion_alive+sdalive), width=.2,
                position=position_dodge(.9)) + ggtitle("Survival by treatment, month, and site") +
  xlab("Site") + ylab("Mean proportion scallops alive after 24 hrs") + facet_wrap("monthname") +
  geom_abline(data = dummy2, aes(intercept = Z, slope = 0), col = "darkred",)
  

p + scale_fill_brewer(palette="Paired") + theme_classic() + 
  theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=12),
        plot.title = element_text(size=22), axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))

#run a 3 way ANOVA? or some other analysis

#first lets check data
hist(pred1$mean_proportion_alive)
library(ggpubr)
ggqqplot(pred1$mean_proportion_alive) #interesting little pattern here 
shapiro.test(pred1$mean_proportion_alive) #failed
#I think beta distribution because response variable (proportion survived)
#is restricted between 0 and 100

#ANOVA for shits and gigs
model <- aov(prop_alive ~ Site * monthname * Treatment, data=pred1)
#view summary of three-way ANOVA
#saying sig interaction be site and month is only one
summary(model)
TukeyHSD(model)
#CY Sep MT Aug 0.07
#OP Sep NW Aug 0.056
#OP Sep MT Sep 0.009
#OP Sep control CY Sep Control 0.052

#this is pretty meaningless. steve says glmm

#bar plots from talking with brad

#Effect of treatment separated by month
#survival slightly higher in corral than control in aug
#survival slightly higher in control than corral in sept
t <- ggplot(pred1, aes(x=monthname, y=mean_proportion_alive, fill=Treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_proportion_aliveMT-sdaliveMT, ymax=mean_proportion_aliveMT+sdaliveMT), width=.2,
                position=position_dodge(.9)) + ggtitle("Survival by treatment") +
  xlab("Month") + ylab("Mean proportion scallops alive after 24 hrs")  +
  geom_abline(data = dummy2, aes(intercept = Z, slope = 0), col = "darkred",)


t + scale_fill_brewer(palette="Paired") + theme_classic() + 
  theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=12),
        plot.title = element_text(size=22), axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))


#Effect of site separated by month
#At CY and Mattituck they did a little better in sept
#At NW and OP they did a little better in august
#OVerall in sept trend is higher survival west to east
#OVerall in aug trend doesnt fall out but MT does the worst 
s <- ggplot(pred1, aes(x=Site, y=mean_proportion_alive, fill=monthname)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_proportion_aliveMS-sdaliveMS, ymax=mean_proportion_aliveMS+sdaliveMS), width=.2,
                position=position_dodge(.9)) + ggtitle("Survival by site") +
  xlab("Site") + ylab("Mean proportion scallops alive after 24 hrs")  +
  labs(fill = "Month") +
  geom_abline(data = dummy2, aes(intercept = Z, slope = 0), col = "darkred",)

s + scale_fill_brewer(palette="Paired") + theme_classic() + 
  theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=12),
        plot.title = element_text(size=22), axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))


# predator marks
marks <- read.csv("2022_post_processing.csv") 
summary <- marks %>%
  rename("date" = "Date.deployed") %>% 
  mutate(date = mdy(date), month = month(date), "monthname" = month.abb[month]) %>%
  rename(predator = Post.processing.predation.analysis) %>%
  mutate(predator = str_replace(predator, "spider crap", "spider crab")) %>%
  mutate(predator = str_replace(predator, "uknown", "unknown")) %>%
  mutate(predator = str_replace(predator, "OD", "oyster drill")) %>%
  mutate(predator = ifelse(predator == "crab", "large crab", predator)) %>%
  mutate(monthname = str_replace(monthname, "Aug", "August")) %>%
  mutate(monthname = str_replace(monthname, "Sep", "September")) %>%
  group_by(Site, monthname)%>%
  count(predator)

#get rid of some shite and reorder factor levels
summary <- summary %>% filter(!is.na(predator))


summary$predator <- fct_relevel(summary$predator, "whelk")

#plot with unknown and unsure
ggplot(summary, aes(x=Site, y = n, fill = predator)) +
  geom_bar(stat = "identity", position = "dodge", color="black") + facet_wrap("monthname",  scales = 'free_y') +
  theme_classic() +  scale_fill_brewer(palette="RdYlBu") +
  ggtitle("Predator marks by site and month") + ylab("No. scallops predated") +
  geom_col(position = position_dodge2(width = 0.9, preserve = c("total", "single"))) 


#get rid of unknown and unsure
knownmarks <- summary %>%
  filter(predator != "unknown") %>%
  filter(predator != "unsure") 

#other graph option
install.packages('lemon')
library(lemon)
ggplot(knownmarks, aes(x=Site, y = n, fill = predator)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_rep_wrap(~ interaction(Site,monthname), scales = 'free_y', repeat.tick.labels = 'left')

ggplot(knownmarks, aes(x=Site, y = n, fill = predator)) +
  geom_bar(stat = "identity", position = "dodge", color="black") + facet_wrap("monthname",  scales = 'free_y') +
  theme_classic() +  scale_fill_brewer(palette="RdYlBu") +
  ggtitle("Predator marks by site and month") + ylab("No. scallops predated") +
  geom_col(position = position_dodge2(width = 0.9, preserve = c("total", "single"))) 

