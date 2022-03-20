augs_synchrony <- function(dataset, frst_day, lst_day, year_samp, grp_var1){
  
  # Dataset a data frame containing values to perform calculations on 
  # year_samp = column holding information on year, 
  # frst_day = column containing the INITIATION DATE of a phenological event
  # lst_dat = column containing the FINAL DATE of a phenological event
  # grp_var1 = one level of grouping to have the function focus on. 
  
  frst_day <- enquo(frst_day)
  lst_day <- enquo(lst_day)
  
  # grouping variables too
  year_samp <- enquo(year_samp)
  grp_var1 <- enquo(grp_var1)
  #grp_var2 <- enquo(grp_var2)
  
  dataset <- dataset %>% 
    mutate(across(c(!!frst_day, !!lst_day), function(x) as.integer(x)))  
  
  results <- dataset %>% 
    group_by(!!year_samp, !!grp_var1, 
             #!!grp_var2
    ) %>% # the year of analysis, and the population variables if on multiyear data.
    
    # TERM A FOR THE INDIVIDUAL $X_{i}$
    
    # term one population (\frac{1}{n-1})
    mutate(at1 = 1/(n() -1)) %>% 
    mutate(across(c(!!frst_day, !!lst_day),
                  function(x) as_date(x, origin = paste0(year-1, "-12-31")))
    ) %>% 
    mutate(interval_obs = interval(ymd(!!frst_day), ymd(!!lst_day))) %>%
    
    # term two, (\frac{1}{f_{i}}) duration of event for individual. 
    # note duration returns seconds, but these data are at resolution of day. 
    mutate(at2 = 1/
             (as.numeric(
               lubridate::as.duration(!!lst_day - !!frst_day)
             )
             /86400
             )
    ) %>% 
    
    # term 3: all days individual i is flowering with other individuals
    # \sum_{j = i}^{n} e_{j != i}
    mutate(at3 = map_dbl(interval_obs, # @ shs on SO
                         \(x) x %>%  
                           intersect(interval_obs) %>%  
                           int_length() %>% 
                           sum(na.rm = T) - int_length(x)
    ) / 86400) %>% 
    
    # the index of synchrony for individual i
    rowwise() %>% 
    mutate(augs.indx.indiv. = at1 * at2 * at3) %>%
    group_by(!!year_samp, !!grp_var1) %>%
    
    ## TERM B FOR THE POPULATION
    mutate(bt1 = 1/n()) %>% # $\frac{1}{n}$ 1/sample size
    mutate(bt2 = sum(augs.indx.indiv.)) %>% # \sum_{j = 1}^{n}X_{i}
    mutate(augs.index.pop = bt1 * bt2)  %>% 
    
    # clean return data
    dplyr::select(-at1, -at2, -at3, -bt1, -bt2, -interval_obs)
  
  return(results)
  
}