Sys.setlocale(category="LC_ALL", locale = "ru_RU")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  plyr,
  ggplot2,
  ggthemes,
  lme4,
  sjstats,
  stringr
)
theme_set(theme_bw())

cap <- data.table::fread("/Users/nikolaevaanna/Desktop/capslock_new/capitalization-interests.csv")
cap <- cap[!duplicated(cap$from_id), ]
nrow(cap)
psych::describeBy(cap$caps_mean_frac, cap$class)

ggplot(cap,aes(x = as.factor(class), y = caps_mean_frac, fill = as.factor(class))) +
  geom_violin() +
  geom_boxplot(width=0.05, alpha = .3) 

kruskal.test(caps_mean_frac ~ as.factor(class), data = cap)

# take only observations with high probs
cap_filtered <- cap[cap$class_probs > 0.5,]
table(cap_filtered$class)
cap_filtered <- cap_filtered[!(cap_filtered$class %in% c(0,8))]
table(cap_filtered$class)

nrow(cap_filtered)
ggplot(cap_filtered,
       aes(x = as.factor(class), 
           y = caps_mean_frac, 
           fill = as.factor(class))) +
  geom_violin() +
  geom_boxplot(width=0.05, alpha = .3) 

kruskal.test(caps_mean_frac ~ as.factor(class), data = cap_filtered)

psych::describeBy(cap_filtered$caps_mean_frac, cap_filtered$class)
