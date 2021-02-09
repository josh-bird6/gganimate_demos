######
#Population smooooooooth


popdata <- read.csv("pop_timeseries.csv", check.names = F) %>% 
  gather(`0-4`:`85+`, key="age", value = "total") %>% 
  mutate(percent=as.numeric(str_sub(total/overall*100, 1,4)),
         gender = as.character(Gender)) %>% 
  select(Year, gender, age, total, overall) 

#Reording factors
popdata$age <-
  factor(
    popdata$age,
    levels = c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85+"
    )
  )

###############################



popgif2 <- popdata %>% 
  filter(Year  >1910, Year <2000) %>% 
  ggplot(aes(x = age, fill = gender,
             y = ifelse(gender == "Male", yes = -total, no = total))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = max(popdata$total) * c(-1,1), labels=abs) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  labs(y = "Population", title = 'Scottish population by age, 1911-1999', subtitle = 'Year: {frame_time}') +
  transition_time(Year) +
  ease_aes('linear')

animate(popgif2, end_pause = 10, width = 700, height = 700)

anim_save(filename = 'popfirst.gif')

############################
########Exporting!#########
#Static image
popdata2 %>% 
  filter(Year == 1999) %>% 
  ggplot(aes(x = age, fill = gender,
                            y = ifelse(gender == "Male", yes = -total, no = total))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = max(popdata$total) * c(-1,1), labels=abs) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  labs(y = "Population", title = 'Scottish population by age, 1911-1999', subtitle = 'Year: 1999')

ggsave("1999.png", width = 7, height = 7)
  
############################
#Example from 2000-2019

popgif3 <- popdata %>% 
  filter(Year  >1999, Year <2019) %>% 
  ggplot(aes(x = age, fill = gender, 
             y = ifelse(gender == "Male", yes = -total, no = total))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = max(popdata$total) * c(-1,1), labels = abs) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12.5,
                                 color = 'black'),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  labs(y = "Population", title = 'Scottish population by age, 2000-2019', subtitle = 'Year: {frame_time}') +
  transition_time(Year) +
  ease_aes('linear')

animate(popgif3, nframes = 200, fps = 20, duration = 10, end_pause = 10, width = 700, height = 700)

anim_save(filename = 'popsecond.gif')

#############################

popdata %>% 
  filter(Year %in% c(1980, 2000, 2018)) %>% 
  ggplot(aes(x = age, fill = gender,
             y = ifelse(gender == "Male", yes = -total, no = total))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = max(popdata$total) * c(-1,1), labels=abs) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(~Year, nrow=3) +
  labs(y = "Population", title = 'Scottish population by age in select years, 1980-2018')

ggsave("comparison.png", width = 7, height = 7)

