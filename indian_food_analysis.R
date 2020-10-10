# Indian food analysis 
# Mohammed Alkhalaf
# 10.10.2020
# DSI Misk class 2020 

library(tidyverse)

indian_food <- read.csv("indian_food.csv")

# splitting the ingredients into rows to make the data tidy for future analysis 

indian_food_tidy <-  indian_food %>% 
   mutate(ingredients = strsplit(as.character(ingredients), ", ")) %>% 
   unnest(ingredients)

# what is the most common diet in indian food?

tail(names(sort(table(indian_food$diet))), 1)

# what is the most common ingredients?

tail(names(sort(table(indian_food_tidy$ingredients))), 1)

# what is the average perp time?

indian_food[ indian_food == -1 ] <- NA # removing -1 values into NA in both tables
indian_food_tidy[ indian_food_tidy == -1 ] <- NA # removing -1 values into NA

mean(indian_food$prep_time , na.rm = TRUE)

# why the following not working?
#indian_food %>% 
#  mean(prep_time , na.rm = TRUE)

# what is the average cooking time?

mean(indian_food$cook_time , na.rm = TRUE)

# average cooking time total time (perp+cook)

mean(indian_food$cook_time , na.rm = TRUE) + mean(indian_food$prep_time , na.rm = TRUE)

# range of prep time 

#indian_food %>% #pipe is not working!!!
range( indian_food$prep_time, na.rm = TRUE)

# range of cooking time

range( indian_food$cook_time, na.rm = TRUE)

# what is the most common course type?



# what is the most common flavor profile?



#what is the most common ingredients in a course type?



#what is the most common ingredients in a flavor type?



#what is the most common course in a state/region?
# state



# reign



#what is the most common ingredients in a state/region?
# state



# reign



#what is the most common flavor in a state/region?
# state



# reign



#what is the most common course in a state/region?
# state



# reign



#what is the average prep time / cooking time in a state/region?
# time / state



# prep time / region



# cooking time / state 



# cooking time / region



