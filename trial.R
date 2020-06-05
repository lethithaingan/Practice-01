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
