library(here)
library(tidyverse)
library(lubridate)


filtered_clean <- read_csv(paste0(here(), "/data/filtered_clean.csv"))
dummy_dat <- read_csv(paste0(here(), "/data/dummy_data.csv"))


syn_prep<-filtered_clean %>% 
  group_by(individual, year, species, nest_duration, nest_start, nest_end, repro_output) %>% 
  distinct(individual) %>% 
  ungroup() 


augs_synchrony <- function(dataset, frst_day, lst_day, year_samp, grp_var1){
  
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
    
    # term one population 
    mutate(at1 = 1/(n() -1)) %>% 
    mutate(across(c(!!frst_day, !!lst_day),
                  function(x) as_date(x, origin = paste0(year-1, "-12-31")))
           ) %>% 
    mutate(interval_obs = interval(ymd(!!frst_day), ymd(!!lst_day))) %>%
    
    # term two, duration of event for individual. note duration returns seconds, 
    # these data are at resolution of day, hence we convert
    mutate(at2 = 1/
             (as.numeric(
        lubridate::as.duration(!!lst_day - !!frst_day)
        )
          /86400
        )
      ) %>% 
    
    # term 3 
    mutate(at3 = map(interval_obs, ~ sum(as.duration(lubridate::intersect(as.numeric(.x), interval_obs) %>% 
                                                       as.numeric(replace_na(0)/86400))))) %>%
    mutate(at3 = as.numeric(at3)/86400) %>% 
    
  # term three the sum of overlap between all individuals in pop.
    rowwise() %>% 
    mutate(augs.indx.indiv. = at1 * at2 * at3) %>%
    group_by(!!year_samp, !!grp_var1) %>%
    
    ## TERM B FOR THE POPULATION
    mutate(bt1 = 1/n()) %>%
    mutate(bt2 = sum(augs.indx.indiv.)) %>%
    mutate(augs.index.pop = bt1 * bt2)  #%>%
#    dplyr::select(-at1, -at2, -at3, -bt1, -bt2, -interval_obs)
  
  return(results)
  
}

testing <- augs_synchrony_2012(dataset = syn_prep, 
                    frst_day = nest_start, 
                    lst_day = nest_end,
                    year_samp = year,
                    grp_var1 = species
                    )

testing_dum <- augs_synchrony(dataset = dummy_dat, 
                               frst_day = flower_start, 
                               lst_day = flower_end,
                               year_samp = year,
                               grp_var1 = species
)



dummy_dat <- dummy_dat %>% filter(species == 'tauschia') # the first row of this data frame should 
# have 201 intersected days (not 301)


abet <- dummy_dat %>% 
  mutate(at1 = 1/(n() -1)) %>% 
  mutate(across(c(flower_start, flower_end),
                function(x) as_date(x, origin = paste0(year-1, "-12-31")))
  ) %>% 
  mutate(interval_obs = interval(ymd(flower_start), ymd(flower_end))) %>%
  
  # term two, duration of event for individual. note duration returns seconds, 
  # these data are at resolution of day, hence we convert
  mutate(at2 = 1/
           (as.numeric(
             lubridate::as.duration(flower_end - flower_start)
           )
           /86400
           )
  )# %>% 


abet1 <- abet %>% 
  mutate(at3 = map(interval_obs, ~ sum(lubridate::intersect(as.numeric(.x), interval_obs) %>% 
                                                   as.numeric(replace_na(0)/86400)))) %>%
  mutate(at3 = as.numeric(at3)/86400) %>% 
  mutate(interval_obs2 = interval_obs)


abet2 <- abet1 %>% 
  rowwise() %>% 
  mutate(to_subtract = as.numeric(intersect(interval_obs, interval_obs2))) %>% 
  ungroup() %>% 
  #mutate(tester = as.numeric(intersect(interval_obs, abet1 %>% pull(interval_obs))))
  
  mutate(Total = map_dbl(.x = .$interval_obs, # this code at least runs
              .f = ~{results = sum(as.numeric(intersect(.x, abet1$interval_obs2)))}))









AS_overlap <- function(x){
  
  identified <- x %>% mutate(observation_id = 1:nrow(.))
  contrasts <- identified %>% dplyr::select(interval_obs, observation_id)
  
  targets <- contrasts
  third_term <- targets
  third_term$interval_obs <- ""
  
  for (i in nrow(targets)){
    third_term[i,] <- sum(lubridate::intersect(i, contrasts[-i,]))
  }
  return(third_term)
}

out <- AS_overlap(abet)

a <- lubridate::intersect(targets[i,], contrasts)
lubridate::intersect(targets[1,1], contrasts[2,1])

