

library(tidyverse)

filtered_clean<-read_csv("https://dl.dropbox.com/s/rdjhkz4pmoksi9c/filtered_clean.csv")

augs_synchrony_2012 <- function(x){
  y <- x %>%
    # TERM A FOR THE INDIVIDUAL
    group_by(species) %>% # the year of analysis, and the population variables if on multiyear data.
    mutate(at1 = 1/(n() -1)) %>% # term one population - 1 under 1.
    mutate(across(c(frst_day,lst_day), function(x) as_date(x, origin = "2011-12-31"))) %>% 
    mutate(interval_obs = interval(ymd(frst_day), ymd(lst_day))) %>%
    mutate(at2 = 1/(as.numeric(as.duration(lst_day - frst_day))/86400))  %>%
    # term two, duration of event for individual. note duration returns seconds, these data are at resolution of day, hence we convert, if you are using high temporal resolution change the division term at the end.
    mutate(at3 =  
             map(interval_obs, ~ sum(lubridate::as.duration(lubridate::intersect(.x, interval_obs) %>% 
                                                              replace_na(0)/86400)))) %>%
    mutate(at3 = as.numeric(at3)) %>% # term three the sum of overlap between all individuals in pop.
    mutate(augs.indx.indiv. = at1 * at2 * at3) %>%
    ## TERM B FOR THE POPULATION
    mutate(bt1 = 1/n()) %>%
    mutate(bt2 = sum(augs.indx.indiv.)) %>%
    mutate(augs.index.pop = bt1 * bt2)  %>%
    dplyr::select(-at1, -at2, -at3, -bt1, -bt2, -interval_obs)
  
}

#  This code chunk was primarily used to change column names to match the function and filter out not relevant columns

# syn_prep<-filtered_clean %>% 
#   group_by(individual, year, species, nest_duration, nest_start, nest_end, repro_output) %>% 
#   distinct(individual) %>% 
#   ungroup()%>% 
#   rename("frst_day"="nest_start", "lst_day"="nest_end", "year_samp"="year") #These names match the names in the function


# A function was created for each year (there were augs_synchrony_2012, 2013, etc) and I filtered the dataframe by the relevant year using the hashed out chunk below and then merged all years together after I ran the functions. 

# syn_2012<- syn_prep %>% 
#   filter(year_samp=="2012") %>% 
#   mutate(across(where(is.numeric), function(x) as.integer(x))) %>% 
#   na.omit()  %>% 
#   augs_synchrony_2012()




# This chunk was not annotated and I am not sure why it is here. 
# span <- interval(ymd("2009-01-01"), ymd("2009-08-01")) #interval
# as.duration(span)
# as.duration(10)








