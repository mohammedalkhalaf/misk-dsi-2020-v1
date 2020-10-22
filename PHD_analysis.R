#Mohammed Alkhalaf
#PHD analysis
#21.10.2020
#Misk DSI 2020

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(DT)
library(ggpubr)
phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")


# Total Number of PHDs by Year 

phd_field %>% 
  group_by(year) %>% 
  summarise(sum(n_phds,na.rm = TRUE), .groups = 'drop') %>% 
  datatable()

# Total Number of PHDs by the broad field

phd_field %>% 
  group_by(broad_field) %>% 
  summarise(sum(n_phds,na.rm = TRUE), .groups = 'drop') %>% 
  datatable()

# Total number of PHDs by the sub field 
phd_field %>% 
  group_by(major_field) %>% 
  summarise(sum(n_phds,na.rm = TRUE), .groups = 'drop') %>% 
  datatable()

# Total Number of PHDs in a broad field by year
phd_field %>%
  group_by(broad_field,year) %>% 
  summarise(Total=sum(n_phds,na.rm = TRUE), .groups = 'drop') %>% 
  ggplot(aes(x=year,y=Total,fill = broad_field ))+geom_area(position = 'stack')+
  labs(title = "Phds numbers by broad major by year", 
       x = "Year",
       y = "Number of Phds",
       fill = "Broad Field") +
  theme_classic(10) +
  theme(rect = element_blank(),
        aspect.ratio = 0.75)+
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0,60000),
                     breaks = seq(0, 60000, 5000),
                     expand = c(0,1000))+ 
  scale_x_continuous( limits=c(2008, 2017),
                      breaks = seq(2008, 2017, 1))


# Percentage change in number of Phds by broad field 
phd_new <- phd_field %>% #preparing the values to be ploted 
  group_by(broad_field,year) %>% 
  summarise(total = sum(n_phds,na.rm = TRUE))
phd_new$broad_year <- paste(phd_new$broad_field, "-", phd_new$year)

phd_new <- phd_new %>% 
  group_by(broad_field) %>% 
  mutate(per_change = ((total - lag(total))/lag(total)*100))

# box plot
phd_new %>% 
  group_by(broad_field) %>% 
  summarise(per_change = ((total - lag(total))/lag(total)*100), .groups = 'drop') %>% 
  ggplot(aes(x=broad_field,y=per_change))+ geom_boxplot() +
  labs(title = "Percentage of change", 
       x = "Broad field",
       y = "Percentage") +
  theme_classic(10) +
  theme(rect = element_blank(),
        aspect.ratio = 0.3)+
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(-20,17),
                     breaks = seq(-25, 25, 5),
                     expand = c(0,0)) 

 
# line plot 
phd_new %>% 
  ggplot(aes(x=year,y=per_change, group=broad_field, color=broad_field))+ geom_line() +
  labs(title = "Percentage of change", 
       x = "Year",
       y = "Percentage of change",
       group= "brpad field") +
  theme_classic(10) +
  theme(rect = element_blank(),
        aspect.ratio = 1)+
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(-20,17),
                     breaks = seq(-25, 25, 5),
                     expand = c(0,0))+ 
  scale_x_continuous( limits=c(2008, 2017),
                      breaks = seq(2009, 2018, 1))+
  grids(linetype = "dashed")


