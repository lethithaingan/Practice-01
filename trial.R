install.packages(c("tidyverse","dplyr","titanic","dslabs"))
library(tidyverse)
library(titanic)
library(dplyr)
library(dslabs)
head(titanic_train)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


##ngan xynh dep
##QQPLOT AGE DISTRIBUTION
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age)) 

titanic %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) +    geom_abline()
