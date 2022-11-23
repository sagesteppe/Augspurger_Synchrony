library(here)
library(tidyverse)
library(lubridate)

#set_here(path='.')
setwd('/hdd/Assorted_R_functions/Augspurger_Synchrony/scripts')
dummy_dat <- read.csv("../data/dummy_data.csv")

test_dummy <- augs_synchrony(dataset = dummy_dat,
                             frst_day = flower_start, lst_day = flower_end,
                             year_samp = year, species )



test_dummyp <- test_dummy |>
  dplyr::group_by(species) |> 
  dplyr::mutate(ID = 1:dplyr::n(),
                flower_start_DOY = lubridate::yday(flower_start),
                flower_end_DOY   = lubridate::yday(flower_end))

library(ggplot2)

ggplot(test_dummyp, aes(y = ID, x = median_flowers)) +
  geom_point() +
  facet_wrap('species', scales = 'free', nrow = 1) +
  theme_bw() +
  geom_segment(aes(x = flower_start_DOY, y = ID, xend = flower_end_DOY, yend = ID), lty = 3) +
  geom_segment(aes(x = lower_sd, y = ID, xend = upper_sd, yend = ID),
               arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
  theme(strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(title = 'Ficticious Floral Synchonry', x = 'DOY', y = 'Plant ID')
  

ggplot(plot14a, aes(x=nest_start, xend=nest_end, y=rowid, colour=indiv_syn)) +
  geom_dumbbell(colour_xend = "gray90", colour_x = "gray90", size=1.5, size_x = .5, size_xend = .5) +
  scale_color_gradient(low="yellow", high="red", limits = c(0,0.4)) +
  theme_classic()+
  #xlab("Day of Year")+
  #ylab("Individual Nest")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.spacing = unit(1, "lines")) +
  facet_wrap(~year)+
  force_panelsizes(cols = unit(c(5, 5), "cm"),
                   TRUE)