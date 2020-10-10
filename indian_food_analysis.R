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

# removing -1 values from the data into NA values 


indian_food[ indian_food == -1 ] <- NA # removing -1 values into NA in both tables
indian_food_tidy[ indian_food_tidy == -1 ] <- NA # removing -1 values into NA

# what is the most common diet in indian food?

tail(names(sort(table(indian_food$diet))), 1) # i found this online i have no idea how it works 

# what is the most common ingredients?

tail(names(sort(table(indian_food_tidy$ingredients))), 1)

# what is the average perp time?

indian_food %>% 
  summarise(avg =mean(prep_time , na.rm = TRUE)) # why it did not work without summarise ??

# what is the average cooking time?

indian_food %>% 
  summarise( avg = mean( cook_time , na.rm = TRUE ))

# average cooking time total time (perp+cook)

indian_food %>% 
  summarise( avg = mean( cook_time , na.rm = TRUE ) + mean(prep_time , na.rm = TRUE) )

# range of prep time 

indian_food %>% 
  summarise( range(prep_time, na.rm = TRUE)) # what is the difference if i assigned it to variable such as r=

# range of cooking time

indian_food %>% 
  summarise( range(cook_time, na.rm = TRUE))

# what is the most common course type?

tail(names(sort(table(indian_food_tidy$course))), 1)

# what is the most common flavor profile?

tail(names(sort(table(indian_food_tidy$flavor_profile))), 1)

#what is the most common ingredients in a course type?

indian_food_tidy %>% 
  group_by(course) %>%
  summarise( mode(ingredients) )  # mode not working here 

#what is the most common ingredients in a flavor type?



#what is the most common course in a state/region?
# state



# region



#what is the most common ingredients in a state/region?
# state



# region



#what is the most common flavor type in a state/region?
# state



# region



#what is the most common course in a state/region?
# state



# reigon



#what is the average prep time / cooking time in a state/region?
# prep time / state

indian_food %>% 
  group_by(state) %>% 
  summarise(avg = mean(prep_time, na.rm = TRUE))# %>% 
# slice_max(1) # if i want to find the top but it is not working 

# prep time / region

indian_food %>% 
  group_by(region) %>% 
  summarise(avg = mean(prep_time, na.rm = TRUE)) # %>% 
  #slice_max(1) # to find the min but it is not working too 


# cooking time / state 

indian_food %>% 
  group_by(state) %>% 
  summarise(avg = mean(cook_time, na.rm = TRUE))


# cooking time / region

indian_food %>% 
  group_by(region) %>% 
  summarise(avg = mean(cook_time, na.rm = TRUE))

