library(here)
library(tidyverse)
library(lubridate)


filtered_clean <- read_csv(paste0(here(), "/data/filtered_clean.csv"))

augs_synchrony_2012 <- function(dataset, frst_day, lst_day, year_samp, grp_var1){
  
  # Dataset a data frame containing values to perform calculations on 
  # year_samp = column holding information on year, 
  # frst_day = column containing the INITIATION DATE of a phenological event
  # lst_dat = column containing the FINAL DATE of a phenological event
  # grp_var1 = one level of grouping to have the function focus on. 
  
  frst_day <- enquo(frst_day)
  lst_day <- enquo(lst_day)
  grp_var1 <- enquo(grp_var1)
  year_samp <- enquo(year_samp)
  
  dataset <- dataset %>% 
    mutate(across(c(!!frst_day, !!lst_day), function(x) as.integer(x)))  

  results <- dataset %>% 
    group_by(!!year_samp, !!grp_var1) %>% # the year of analysis, and the population variables if on multiyear data.
    
    # TERM A FOR THE INDIVIDUAL
    
    # term one population - 1 under 1.
    mutate(at1 = 1/(n() -1)) %>% 
    mutate(across(c(!!frst_day, !!lst_day),
                  function(x) as_date(x, origin = paste0(year-1, "-12-31")))
           ) %>% 
    
    mutate(interval_obs = interval(ymd(!!frst_day), ymd(!!lst_day))) %>%
    
    mutate(at2 = 1/
             (as.numeric(
        lubridate::as.duration(!!lst_day - !!frst_day)
        )
          /86400
        )
      ) %>% 
    
    # term two, duration of event for individual. note duration returns seconds, 
    # these data are at resolution of day, hence we convert,
    # if you are using high temporal resolution 
    # change the division constant (86400) at the end.
    
    mutate(at3 = map(interval_obs, ~ sum(as.duration(intersect(as.numeric(.x), interval_obs) %>% 
                                                       as.numeric(replace_na(0)/86400))))) %>%
    mutate(at3 = as.numeric(at3)/86400) %>% 
    
  # term three the sum of overlap between all individuals in pop.
    mutate(augs.indx.indiv. = at1 * at2 * at3) %>%
    
    ## TERM B FOR THE POPULATION
    mutate(bt1 = 1/n()) %>%
    mutate(bt2 = sum(augs.indx.indiv.)) %>%
    mutate(augs.index.pop = bt1 * bt2)  %>%
    dplyr::select(-at1, -at2, -at3, -bt1, -bt2, -interval_obs)
  
  return(results)
  
}



##################################### Running it


#  This code chunk was primarily used to change column names to match the function and filter out not relevant columns

 syn_prep<-filtered_clean %>% 
   group_by(individual, year, species, nest_duration, nest_start, nest_end, repro_output) %>% 
   distinct(individual) %>% 
   ungroup() 
   
   # SHOULD BE ACCOMODATED BY LISTED NAMES
   rename("frst_day"="nest_start", "lst_day"="nest_end", "year_samp"="year") #These names match the names in the function


# A function was created for each year (there were augs_synchrony_2012, 2013, etc) and I filtered the dataframe by the relevant year using the hashed out chunk below and then merged all years together after I ran the functions. 

syn_prep <- syn_prep %>% 
   mutate(across(where(is.numeric), function(x) as.integer(x))) %>% 
   na.omit() 



testing <- augs_synchrony_2012(dataset = syn_prep, 
                    frst_day = nest_start, 
                    lst_day = nest_end,
                    year_samp = year,
                    grp_var1 = species
                    )


example <- 
