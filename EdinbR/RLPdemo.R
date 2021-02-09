######################
#########GIF##########
######################

library(tidyverse)
library(gganimate)
library(gifski)
library(transformr)

######################
#Data prep - THIS TIME NOT BY QUALIFICATION

basedata2 <- read.csv("MOCKUP_overall2.csv")

basedata_real3 <- basedata2 %>% 
  filter(SFC_quintiles %in% (1:10),
         qual %in% c("HNC or equivalent", "HND or equivalent")) %>% 
  group_by(SCHLNAME, yearz, SFC_quintiles) %>% 
  summarise(total =n()) %>% 
  mutate(totals = sum(total),
         percent = as.numeric(str_sub(total/totals *100, 1, 4)),
         year = ifelse(yearz %in% "2013-14", 2013,
                       ifelse(yearz %in% "2014-15", 2014,
                              ifelse(yearz %in% "2015-16", 2015,
                                     ifelse(yearz %in% "2016-17", 2016, 
                                            2017))))) %>% 
  filter(SFC_quintiles == 1,
         SCHLNAME != "Newbattle Abbey College") %>% 
  select(SCHLNAME, yearz, year, everything())

######################


RLP_gif <- ggplot(data = basedata_real3, aes(x=year, y=percent, group = SCHLNAME, color = SCHLNAME)) +
  geom_line(size = 1.5) +
  geom_segment(aes(xend=2017, yend=percent), linetype = 4) +
  geom_point(size = 5, aes(shape = SCHLNAME)) +
  theme_minimal() +
  geom_text(aes(x=2017.1, 
                label = str_c(round(percent, 1), '%')), 
            hjust=0,
            lineheight =.5) +
  coord_cartesian(clip = 'off') +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        plot.margin = margin(50, 50, 50, 50)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_color_manual(values = c("#762a83", "#af8dc3", "#5ab4ac", "#67a9cf", "#2166ac")) +
  labs(title = "Proportion of MD20 students on an HNC/HND course in select Scottish colleges, \n2013-14 to 2017-18", 
       y="", 
       x="Year", 
       caption = "note: does not include missing SIMD data") +
  transition_reveal(year) +
  ease_aes('cubic-in-out', interval = .5) 
 

animate(RLP_gif, duration = 10, end_pause = 30, width = 1000, height = 1000)

anim_save(filename = 'RLP_animation.gif')

# RLP_gif_animated <- animate(RLP_gif, width = 650, height = 600, res = 100, nframes = 600, 
#                      duration = 15, fps = 10, end_pause = 10)
