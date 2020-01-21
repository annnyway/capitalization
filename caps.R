Sys.setlocale(category="LC_ALL", locale = "ru_RU")
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  plyr,
  ggplot2,
  sjstats,
  stringr
)
theme_set(theme_bw())

cap <- data.table::fread("capitalization.csv")
cap <- cap[!duplicated(cap$from_id), ]
nrow(cap)

cap_m <- cap[cap$n_mean_caps > 0,]
nrow(cap_m)

hist(cap_m$caps_mean_frac, 
     col="pink", 
     main="Capslock fraction distribution",
     xlab="fraction") # dependent variable distribution
nrow(cap_m[cap_m$n_tokens > 100,])
hist(cap_m[cap_m$n_tokens < 100,]$n_tokens, 
     main="Number of tokens in a comment", 
     breaks=50, 
     col="cyan", 
     xlab="number of tokens")

max(cap_m$n_tokens)
psych::describe(cap_m$caps_mean_frac)

# sex
cap_m$sex <- factor(cap_m$sex)
cap_m$sex <- revalue(cap_m$sex, c("1"="female", "2"="male")) # rename factor levels
cap_m <- cap_m[cap_m$sex != 0,]
table(cap_m$sex)  
table(cap_m[cap_m$source=="yajemati"]$sex)  

psych::describeBy(cap_m$caps_mean_frac,cap_m$sex)
bartlett.test(cap_m$caps_mean_frac ~ cap_m$sex) #check homogeneity of variance 
# => different variances

ggplot(cap_m,aes(x = sex, y = caps_mean_frac, fill = sex)) +
  geom_violin() + 
  geom_boxplot(width=0.05, alpha = .3) +
  labs(
    caption = paste0('n female = ',length(cap_m$sex[cap_m$sex == 'female']),', 
                     n male = ',length(cap_m$sex[cap_m$sex == 'male']))
  )


t.test(cap_m[cap_m$sex == "male"]$caps_mean_frac, 
       cap_m[cap_m$sex == "female"]$caps_mean_frac, 
       var.equal = FALSE)

wilcox.test(cap_m[cap_m$sex == "male"]$caps_mean_frac, 
            cap_m[cap_m$sex == "female"]$caps_mean_frac, 
            alternative = "greater")


pvals <- c()
for(i in 1:1000){
  male_sample <- sample(cap_m[cap_m$sex == "male"]$caps_mean_frac, 100) #71.6 %, 300 - 95-100%
  female_sample <- sample(cap_m[cap_m$sex == "female"]$caps_mean_frac, 100)
  pvals <- c(wilcox.test(male_sample, 
                         female_sample, 
                         alternative = "greater")
             $p.value,pvals)
}
cat(length(pvals[pvals < 0.05])/1000*100,"%")


# onlu yajemati - SEX
only_yajemati <- cap_m[cap_m$source=="yajemati"]
nrow(only_yajemati)
psych::describeBy(only_yajemati$caps_mean_frac, only_yajemati$sex)
wilcox.test(only_yajemati[only_yajemati$sex == "male"]$caps_mean_frac, 
            only_yajemati[only_yajemati$sex == "female"]$caps_mean_frac, 
            alternative = "greater")
# мужчины чаще используют капслок, чем женщины
ggplot(cap_m,aes( x = caps_mean_frac, fill = sex)) +
  geom_density(alpha = .3)

# ВЫВЕСТИ САМЫЕ ЧАСТОТНЫЕ СЛОВА

# city
cap_city <- filter(cap_m, country == "Russia")
nrow(cap_city) # 13322 users mentioned their current city

n_city <- nrow(cap_city$city)
cap_city$n_city <- 1
# next count the number of observations in every city
city_counts <- aggregate(cap_city$n_city, by=list(city=cap_city$city), FUN=sum)

city_list <- city_counts[city_counts$x > 100,]$city # filter cities where more than 100 observations
cap_city_filtered <- cap_city[cap_city$city %in% city_list,]
nrow(cap_city_filtered) # 8023 observations left for the analysis

psych::describeBy(cap_city_filtered$caps_mean_frac,cap_city_filtered$city)
# средняя доля капслока меньше всего в Красноярске (0.26); больше всего в Уфе (0.36) и Казани 

ggplot(cap_city_filtered,aes(x = city, y = caps_mean_frac, fill = city)) +
  geom_violin() +
  geom_boxplot(width=0.05, alpha = .3) 

wilcox.test(cap_city_filtered[cap_city_filtered$city == "Moscow",]$caps_mean_frac,
            cap_city_filtered[cap_city_filtered$city == "Saint Petersburg",]$caps_mean_frac)
# нет разницы между жителями Москвы и Санкт-Петербурга

wilcox.test(cap_city_filtered[cap_city_filtered$city %in% c("Moscow", "Saint Petersburg"),]$caps_mean_frac,
            cap_city_filtered[!cap_city_filtered$city %in% c("Moscow", "Saint Petersburg"),]$caps_mean_frac)
# есть разница в доле капслока между Москвой и Питером и другими городами

wilcox.test(cap_city_filtered[cap_city_filtered$city %in% c("Moscow", "Saint Petersburg"),]$caps_mean_frac,
            cap_city_filtered[!cap_city_filtered$city %in% c("Moscow", "Saint Petersburg"),]$caps_mean_frac,
            alternative="less")
# есть разница в доле капслока между Москвой и Питером и другими городами, 
# причем в Москве и Питере капслоком пишут меньше
# p-value = 5.033e-05

wilcox.test(cap_city[cap_city$city %in% city_list,]$caps_mean_frac,
            cap_city[!cap_city$city %in% city_list,]$caps_mean_frac,
            alternative="greater")

# вообще в остальных городах доля капслока меньше
psych::describe(cap_city[!cap_city$city %in% city_list,]$caps_mean_frac)
psych::describe(cap_city_filtered$caps_mean_frac)


# VK GROUP
ggplot(cap_m,aes( x = caps_mean_frac, fill = source)) +
  geom_density(alpha = .3)

ggplot(cap_m,aes(x = source, y = caps_mean_frac, fill = source)) +
  geom_violin() +
  geom_boxplot(width=0.05, alpha = .3) 

kruskal.test(caps_mean_frac ~ as.factor(source), data = cap_m)
# есть зависимость от тематики! 
psych::describeBy(cap_m$caps_mean_frac,cap_m$source)

vdud_sssr <- rbind(cap_m[cap_m$source == "vdud",],
                   cap_m[cap_m$source == "sssr",])
other_source <- rbind(cap_m[cap_m$source == "lentach",],
                      cap_m[cap_m$source == "sci",],
                      cap_m[cap_m$source == "yajemati",])
wilcox.test(vdud_sssr$caps_mean_frac,
            other_source$caps_mean_frac,
            alternative="greater")

# AGE 

caps_age <- cap_m[cap_m$age <= 60]
hist(caps_age$age, col = "yellow", breaks=70)
nrow(caps_age)
qplot(data = caps_age, age, caps_mean_frac, colour = source)

# l <- lm(caps_age$caps_mean_frac ~ caps_age$age)
# summary(l)

# age as factor - grouping
caps_age$age_rounded <- round(as.integer(as.character(caps_age$age)),-1)
caps_age_avg <-
  caps_age %>%
  dplyr::group_by(age_rounded) %>%
  dplyr::summarize(avg_caps_frac = mean(caps_mean_frac))

ggplot(caps_age,aes(x=caps_mean_frac)) + 
  geom_histogram(aes(y=..density..),col = "darkgreen", fill = "darkgreen", alpha = .5) +
  geom_text(data = caps_age_avg, 
            mapping = aes(x = avg_caps_frac, 
                          y = 8, 
                          label = round(avg_caps_frac,2)), 
            angle = 90, 
            vjust = 1.3) +
  facet_grid(~age_rounded) +
  geom_vline(caps_age_avg, 
             mapping = aes(xintercept = avg_caps_frac), 
             colour = 'red', 
             linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

psych::describeBy(caps_age$caps_mean_frac,caps_age$age_rounded)

kruskal.test(caps_mean_frac ~ as.factor(age_rounded), data = caps_age)

ggplot(caps_age, aes(x = as.factor(age_rounded), 
                     y = caps_mean_frac, 
                     fill = age_rounded)) +
  geom_violin() +
  geom_boxplot(width=0.05, alpha = .3)


sssr_caps <- cap_m[cap_m$source == "sssr",]
sssr_caps <- sssr_caps[sssr_caps$age < 80]
plot(sssr_caps$age, sssr_caps$caps_mean_frac)

vdud_caps <- cap_m[cap_m$source == "vdud",]
vdud_caps <- vdud_caps[vdud_caps$age < 80]
plot(vdud_caps$age, vdud_caps$caps_mean_frac, col=cols_t1)
nrow(vdud_caps)


# regression experiments 

caps_for_log_reg <- caps_age[caps_age$city %in% city_list]
nrow(caps_for_log_reg)

table(caps_for_log_reg$source)
table(caps_for_log_reg$sex)
table(caps_for_log_reg$city)
var(caps_for_log_reg$caps_mean_frac)

library(caret)
set.seed(43657)
Train <- createDataPartition(caps_for_log_reg$caps_mean_frac, p=0.9, list=FALSE)
training <- caps_for_log_reg[ Train, ]
testing <- caps_for_log_reg[ -Train, ]
nrow(training)
nrow(testing)
training$age <- as.numeric(age)

caps.fit <- glm(caps_mean_frac ~ as.numeric(age) + sex + source + city, data=training, family=quasibinomial, )
summary(caps.fit)
caps.prob <- predict(caps.fit, testing, type="response")
caps.prob

#mod_fit <- train(caps_mean_frac ~ age + city + sex + source, data=training, method="glm", family="quasibinomial")
#summary(mod_fit)
#predict(mod_fit, newdata=testing)

# testing
library(ResourceSelection)
caps.prob

h1 <- hoslem.test(testing$caps_mean_frac, caps.prob, g=10) # ???
h1
cbind(h1$observed,h1$expected)
testing$caps_mean_frac

training# predicting
testing %>% 
  select(age, city, sex, source, caps_mean_frac) -> predict_caps

predict_caps$preds <- caps.prob
View(predict_caps)

h1 <- hoslem.test(predict_caps$caps_mean_frac, predict_caps$preds, g=10)
h1
cbind(h1$observed,h1$expected)

install.packages("blorr")
library(blorr)
blr_regress(caps.fit)
blr_model_fit_stats(caps.fit)

training <- training[training$sex != 0]
training <- 
  training %>%
  mutate(
    sex_level = case_when(
      sex %in% c('female') ~ 0,
      sex %in% c('male') ~ 1,
    )
  )

testing <- 
  testing %>%
  mutate(
    sex_level = case_when(
      sex %in% c('female') ~ 0,
      sex %in% c('male') ~ 1,
    )
  )

# LOG REGRESSION for sex
s <- glm(sex_level ~ caps_mean_frac + age + source + city, data=training, family=binomial(link = 'logit'))
summary(s)
sex.prob <- predict(s, testing)
sex.prob <- round(sex.prob, 0)
# 

sex.prob
testing$sex
library(blorr)
blr_regress(s)
blr_model_fit_stats(s)
blr_confusion_matrix(sex.prob, cutoff=0.5, data=testing$sex_level)

blr_test_hosmer_lemeshow(s)


sex.prob
# JUST REGRESSION
summary(lm(
  round(caps_mean_frac*100, digits=0) ~ as.numeric(age) + sex + source + city, 
  data=training
  ))$adj.r.squared

testing$sex

