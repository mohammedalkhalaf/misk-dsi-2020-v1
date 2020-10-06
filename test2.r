#this is an other test file to pull from git bash

library(tidyverse)

summary(mtcars)

mtcars %>% 
  group_by(gear) %>% 
  filter(hp>150,cyl==6)
