library(fredr)
source('analyses/supporting/fred_api_key.R')


# labor force participation -----------------------------------------------

lfp_women <- fredr(
  series_id = "LNS11300002"
) %>% 
  mutate(sex = 'Women')

lfp_men <- fredr(
  series_id = "LNS11300001"
) %>% 
  mutate(sex = 'Men')

bind_rows(lfp_women, lfp_men) %>% 
  filter(date > as.Date('2015-01-01')) %>%
  ggplot(aes(x = date, y = value, color = sex)) +
  geom_line() +
  scale_x_date(date_breaks = '1 year',
               date_label = '%Y') +
  labs(title = 'Labor force participation by sex',
       caption = 'Data: Bureau of Labor Statistics via FRED API',
       x = NULL,
       y = "Percent",
       color = NULL) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0),
        legend.position = 'bottom')
# ggsave('analyses/supporting/plots/labor_force_participation.png', height = 5, width = 8)
