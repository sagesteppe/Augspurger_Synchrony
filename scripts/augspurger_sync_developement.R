library(here)
library(tidyverse)
library(lubridate)

#set_here(path='.')
dummy_dat <- read_csv(paste0(here(), "/data/dummy_data.csv"))


test_dummy <- augs_synchrony(dataset = dummy_dat,
                             frst_day = flower_start, 
                             lst_day = flower_end,
                             year_samp = year, 
                             species
                             )
