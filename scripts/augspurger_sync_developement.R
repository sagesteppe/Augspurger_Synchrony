setwd('~/Documents/assoRted/Augspurger_Synchrony/data')
pena_flrs <- read.csv("../data/dummy_data.csv")
save('pena_flrs', file = '~/Documents/assoRted/AugspurgerIndex/data/flowering_data.RData')


head(pena_flrs)
pena_synchrony <- augs_synchrony(dataset = pena_flrs,
                             frst_day = flower_start, lst_day = flower_end,
                             year_samp = year, species )
head(pena_synchrony)


test_dummyp <- pena_synchrony |>
  dplyr::group_by(species) |>
  dplyr::mutate(ID = 1:dplyr::n(),
                flower_start_DOY = lubridate::yday(flower_start),
                flower_end_DOY   = lubridate::yday(flower_end))

library(ggplot2)

ggplot(test_dummyp, aes(y = ID, x = median_flowers)) +
  geom_point(aes(fill = 'Median')) +
  facet_wrap('species', scales = 'free', nrow = 1) +
  theme_bw() + 
  geom_segment(aes(x = flower_start_DOY, y = ID, 
                   xend = flower_end_DOY, yend = ID,
                   linetype = 'Range')) + 
  geom_segment(
    aes(x = lower_sd, y = ID, xend = upper_sd, yend = ID, linetype = 'mean SD'),
               arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +

  theme(strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank()) +
  labs(title = 'Fictitious Floral Synchonry', x = 'Day of Year', y = 'Individual',
       caption = 'Dashed lines indicate the entire time (range) an event is occurring - the sole value used in the index.\nMedian date is the peak of an event on the plant, such as when 50% of all flowers have been produced.\nThe Standard deviation reflects the uncertainty of the sampling on a subset of the individual.', fill = NULL) +
  scale_linetype_manual("Phenohase\nDuration",
                        values=c("Range"= 3, 'mean SD' = 1)) + 
  scale_color_manual(values = c("Median" = 'black')) + 
  scale_y_continuous(breaks = scales::breaks_pretty()) + 
  scale_x_continuous(breaks = scales::breaks_pretty())

ggsave(path = '../data', filename = 'AugsPanel.png', height = 7, width = 7, units = 'in')

test_dummyp |>
  dplyr::select(species, ID, augs.index.pop, augs.indx.indiv.) |>
  ggplot(aes(y = ID, x = augs.indx.indiv.)) + 
  geom_point() +  
  geom_line(aes(x = augs.index.pop, colour="-"),  lty = 2) + 
  facet_wrap('species', scales = 'free', nrow = 1) + 
  theme_bw() + 
  scale_y_continuous(breaks = scales::breaks_pretty()) + 
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  xlim(0, 1.01) + 
  labs(x = 'Individual Synchrony', y = 'Individual', 
       title = 'Fictitious Synchrony Index', 
       caption = 'The leftmost panels reflect the total and total lack of overlap for the event range.\nThe third panel shows that ca, 2/3 of individuals are flowering at any one time.\nThe final panel displays a real world scenario, discussed below.') + 
  
  theme(strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank())  + 
  scale_color_manual("Population\nSynchrony", values = c('-' = 'grey50')) 
  
ggsave(path = '../data', filename = 'IndexPanel.png', height = 7, width = 7, units = 'in')
