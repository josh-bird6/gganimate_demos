library(tidyverse)
library(gganimate)
library(gifski)
library(transformr)

####################################

basedata <- read.csv("basedata.csv") 
  
###################
#COLLEGE VIZ#######
###################

collegestats <- ggplot(data=basedata, aes(x=year, y=total, group = classification, color = classification)) +
  geom_line(size=1.5) +
  geom_segment(aes(xend=2018, yend=total), linetype =4) +
  geom_point(size=5, aes(shape = classification)) +
  theme_minimal() +
  geom_text(aes(x=2018.1, 
                label = scales::comma(total)), 
            hjust=0,
            lineheight =.5) +
  coord_cartesian(clip = 'off') +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        plot.margin = margin(50, 50, 50, 50),
        axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 500000)) +
  scale_x_discrete(limits=2008:2017, labels = c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")) +
  scale_color_manual(values = c("#873694", "#00A2AE", "#af8dc3")) +
  labs(title = "Activity in Scotland's colleges, 2009-10 to 2017-18", 
       y="Total", 
       x="Year") +
  transition_reveal(year) +
  ease_aes('cubic-in-out') +
  enter_fade() +
  exit_shrink() 

animate(collegestats, duration = 13, end_pause = 30, width = 700, height = 700) 

anim_save(filename = 'collegestats.gif')

################################################################################################################################

##################
###CLD VIZ########
##################

year <- c(2013, 2013, 2013, 2013, 2014, 2014, 2014, 2014, 2015, 2015, 2015, 2015, 2016, 2016, 2016, 2016)
total <- c(81.7, 3.4, 0.9, 14.1, 82.6, 3.1, 0.7, 13.6, 82.8, 3.4, 1.0, 12.8, 84.5, 3.3, 1.2, 11.1) 
Classification <- c("Positive", "Negative", "Other", "Unconfirmed", "Positive", "Negative", "Other", "Unconfirmed", "Positive", "Negative", "Other", "Unconfirmed", "Positive", "Negative", "Other", "Unconfirmed")

basedata_cld <- data.frame(year, total, Classification)

basedata_cld$Classification <- factor(basedata_cld$Classification, levels=c("Positive", "Negative", "Other", "Unconfirmed"), labels=c("Positive", "Negative", "Other", "Unconfirmed"))


cldviz <- ggplot(data = basedata_cld, aes(x=year, y=total, group = Classification, color = Classification)) +
  geom_line(size=1.5) +
  geom_segment(aes(xend=2016.4, yend=total), linetype =4) +
  geom_point(size=5, aes(shape = Classification)) +
  theme_minimal() +
  geom_text(aes(x=2016.5, 
                label = paste0(total, '%')), 
            hjust=0,
            lineheight =.5) +
  coord_cartesian(clip = 'off') +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 12.5),
        legend.title = element_text(size=15),
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        plot.margin = margin(50, 50, 50, 50)) +
  guides(color=guide_legend(ncol=2, byrow=T)) +
  scale_color_manual(values = c("#31a354", "red", "orange", "grey")) +
  scale_x_discrete(limits = 2013:2016, labels = c("2013-14", "2014-15", "2015-16", "2016-17", "2013-14", "2014-15", "2015-16", "2016-17", "2013-14", "2014-15", "2015-16", "2016-17", "2013-14", "2014-15", "2015-16", "2016-17")) +
  labs(title = "College Leaver Destinations by category,\n 2013-14 to 2016-17", 
       y="%", 
       x="Year",
       caption = "Note: see CLD publication for definition of classifications") +
  transition_reveal(year) +
  ease_aes('cubic-in-out', interval = .5) +
  enter_fade() +
  exit_shrink() 

animate(cldviz, width = 700, height = 700, res = 100, nframes = 600, duration = 6, fps = 10, end_pause=10)

anim_save(filename = 'cldvis.gif')

################################################################################################################################

##################
###Student satisfaction?########
##################

basedata_satisfaction <- read.csv("satisfaction.csv") %>% 
  mutate(satisfaction = round(satisfaction * 100, digits = 2),
         color = ifelse(region %in% "Scotland", "red", "blue")) %>% 
  group_by(year) %>% 
  mutate(ordering = min_rank(satisfaction)*1.0) %>% 
  ungroup()


theme_set(theme_minimal())

satisfaction_gif <- basedata_satisfaction %>%
  ggplot(aes(x=ordering, y=satisfaction, group = region, fill = color)) +
  geom_tile(aes(height = satisfaction,
                width = 0.9), 
            alpha = .9) +
  scale_fill_manual(values = c("#00A2AE", "#873694")) +
  geom_text(aes(y = 0, label = paste(region, " ")), vjust= 0.2, hjust=1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1, size = 22),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        plot.margin = margin(1,1,1,5, "cm"),
        plot.subtitle = element_text(size = 15)) +
  labs(title = "Student Satisfaction in Scotland by College Region, 2015-16 to 2017-18", 
       y="Satisfaction(%)", 
       subtitle = '{closest_state}', 
       x = "") +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

animate(satisfaction_gif, fps = 25, duration = 10, width = 800, height = 600)

anim_save(filename = 'satisfaction.gif')

################################################################################################################################

##################
###Grand Exit#####
##################

grand_exit <- read.csv("grand_exit.csv") %>% 
  mutate(color=ifelse(letter %in% c("T", "H", "A", "N","K"), "#00A2AE", "#873694"))

exit <- ggplot(data=grand_exit, aes(x=x_axis, y=y_axis, color = color)) +
  geom_point(size = 6) +
  theme_void() +
  scale_color_manual(values = c("#00A2AE", "#873694")) +
  scale_y_continuous(limits = c(0,100)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        legend.position = "none") +
  labs(x="", y="") +
  transition_states(phase, transition_length=2, state_length=1) +
  ease_aes('cubic-in-out') 
  
animate(exit, width = 800, height = 600)

anim_save(filename = 'exit.gif')
