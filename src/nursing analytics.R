library('ProjectTemplate')
load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

#### Nursing Analytics ####

daily_btl <- bottle %>% group_by(date) %>% summarize(ttl_btl = sum(quantity))
daily_pmp <- pumping %>% group_by(date) %>% summarize(ttl_pmp = sum(quantity_oz))

df <- left_join(daily_btl, daily_pmp) %>% left_join(weight)

# impute missing weight values
df <- df %>% 
  group_by(tmp = cumsum(!is.na(wt))) %>% 
  mutate(wt_est = wt[1] + .08*(0:(length(wt)-1))) %>% 
  ungroup() %>% 
  select(-tmp)

# add variables of interest
df <- df %>% mutate(daily_intake = (wt_est*2.5), 
                    nursing = daily_intake - ttl_btl, 
                    nursing_fraction = nursing/daily_intake, 
                    production = ttl_pmp + nursing)

# hold daily_intake steady at 24.5 (sd 2.75)
df$daily_intake[df$daily_intake >= 24.5] <- 24.5

#### Plots ####

df %>% 
  select(-wt, -wt_est, -nursing_fraction) %>% 
  filter(date < today()) %>% 
  melt(id.vars = c("date")) %>% 
  mutate(metric_type = ifelse(variable %in% c("ttl_btl", "ttl_pmp"), "measured", "derived")) %>% 
  ggplot(aes(x = date, y = value, colour = variable, shape = metric_type)) + 
  geom_point(alpha = .4, size = 2) + 
  geom_smooth(alpha = .2) + 
  theme_minimal() +
  labs(x = NULL, y = "Ounces", title = "Feeding Metrics Over Time", subtitle = "Measured and derived metrics to track Mom & Baby's progress") +
  theme(legend.title = element_blank())

ggsave("plots/feeding_metrics.png")

df %>% 
  filter(date < today()) %>% 
  ggplot(aes(x = date, y = nursing_fraction)) + 
  geom_point() + 
  geom_smooth() + 
  theme_minimal() +
  labs(x = NULL, y = "Nursing Fraction", title = "Fraction of Daily Required Intake from Nursing") +
  theme(legend.title = element_blank())

ggsave("plots/nursing_fraction.png")

