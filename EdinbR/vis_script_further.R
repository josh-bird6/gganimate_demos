################Bar chart!

popdata_select <- read.csv("pop_timeseries_overall.csv", check.names = F) %>% 
  gather(`0-4`:`85+`, key="age", value = "total") %>% 
  select(Year, age, total, overall) %>% 
  filter(Year %in% c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2018),
         age %in% c("15-19", "20-24")) %>% 
  mutate(percent = as.numeric(str_sub(total/overall *100, 1, 4))) %>% 
  group_by(Year) %>% 
  summarise(total = sum(percent))

#####################################

pop_select <- ggplot(data = popdata_select, aes(x=Year, y = total, fill = total)) +
  geom_col() +
  labs(title = "15-24 year-olds as a proportion of the Scottish population, 1980-2018", y = "%") +
  coord_flip() +
  scale_fill_distiller(palette = "Purples", direction = 1) +
  theme_minimal() +
  geom_text(aes(label = ifelse(Year %in% c(1980, 2018), paste0(total, '%'), ""), size = 20), hjust = 5) +
  theme(plot.title = element_text(size = 20),
        legend.position = "none",
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  transition_states(Year, wrap = F) +
  shadow_mark(colour = 'black') +
  enter_grow() 
  
animate(pop_select, end_pause = 15, width = 700, height = 700)

anim_save(filename = 'popthird.gif')

##################################################

Year <- c(2020, 2025, 2030, 2035, 2040)3
total <- c(11.25, 10.93, 11.33, 11.08, 10.86)

popdata_select2 <- data.frame(Year, total)

popdata_final <- rbind(popdata_select, popdata_select2) %>% 
  mutate(transtate = ifelse(Year %in% c(1980:2018), 1, 2))

library(ggstance)
#USING GGSTANCE PACKAGE AND SWITCHING UP X/Y AESTHETIC TO ENSURE COORD_FLIP WORKS CORRECTLY

pop_select <- ggplot(data = popdata_final, aes(x=total, y = Year, fill = Year)) +
  # geom_bar(stat="identity") +
  geom_barh(stat="identity") +
  labs(title = "15-24 year-olds as a proportion of the Scottish population, 1980-2040", x = "%") +
  # coord_flip() +
  geom_text(aes(label = ifelse(Year %in% c(1980, 2018, 2040), paste0(total, '%'), ""), size = 5), hjust = 3) +
  scale_fill_distiller(palette = "Purples", direction = -1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        legend.position = "none",
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  transition_states(transtate, transition_length = 2, state_length = 1, wrap = F) +              #REFERS TO THE DATA!!!!
  shadow_mark(colour = 'black') +
  enter_grow() +
  view_step(pause_length = 2, step_length = 1, nsteps = 2, wrap = F)                             #REFERS TO THE VIEW

animate(pop_select, end_pause = 25, width = 700, height = 700)

anim_save(filename = 'popfourth.gif')
