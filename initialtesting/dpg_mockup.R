library(tidyverse)

#loading dataset
areachart <- read_csv("FTFE_look.csv")

#generating yearly totals
areachart_total <- areachart %>% 
  group_by(ACADEMICYEAR) %>% 
  summarise(total = sum(enrolments)) 

#matching back on to original dataset and additional wrangling
areachart$total <- areachart_total$total[match(areachart$ACADEMICYEAR, areachart_total$ACADEMICYEAR)]

areachart<- areachart %>% 
  filter(dprog != "No program group recorded") %>% 
  mutate(percent = round(enrolments/total*100, 2)) %>% 
  group_by(ACADEMICYEAR) %>% 
  mutate(ordering = min_rank(ordering)*1.0) %>% 
  ungroup()

plot <- areachart %>% 
  filter(ordering <6) 

#SOME FORMATTING TO ACCOUNT FOR THE PHASED INTRODUCTION OF NEW DRPOGS
dprog <- c("Engineering", "Social Work", "Art & Design", "Special Programmes")
ACADEMICYEAR <- c("2012-13", "2014-15", "2015-16", "2013-14")
enrolments <- c(0,0,0,0)
total <- c(0,0,0,0)
percent <- c(0,0,0,0)
ordering <- c(0,0,0,0)

plot_b <- data.frame(dprog, ACADEMICYEAR, enrolments, total, percent, ordering)

plot_concatenated <- rbind(plot, plot_b) 
############################
#FIRST VISUALISATION

ggplot(plot_concatenated, aes(x=ACADEMICYEAR, y=percent, group = reorder(dprog,-ordering), fill = dprog)) +
  geom_area() +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal() +
  labs(fill = "Dominant Programme Group", 
       y="%", 
       x="Academic Year", 
       title = "Most popular subject areas for full-time FE students, 2009-10 to 2018-19") 


##################
##################
##################
##################
##################
#GGANIMATE TEST###
library(gganimate)

test <- plot_concatenated %>% 
  mutate(test = as.numeric(substr(ACADEMICYEAR, 1, 4)))
    

ggplot(test, aes(x=test, y=percent, group = reorder(dprog,-ordering), fill = dprog)) +
  geom_area() +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal() +
  labs(fill = "Dominant Programme Group", 
       y="%", 
       x="Academic Year", 
       title = "Most popular subject areas for full-time FE students, 2009-10 to 2018-19",
       subtitle = "Year: {frame_along}") +
  transition_reveal(test) +
  ease_aes('cubic-in-out') 
