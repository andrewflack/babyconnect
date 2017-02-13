library('ProjectTemplate')
load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

avg_by_hr <- pumping %>% 
  filter(date >= today()-21) %>% 
  mutate(hr = hour(hm(time))) %>% 
  group_by(date, hr) %>% 
  summarize(session_ttl = sum(quantity_oz)) %>% 
  ungroup() %>% 
  group_by(hr) %>% 
  summarize(n = n(), 
            avg_quantity = mean(session_ttl), 
            SE = sd(session_ttl)/sqrt(n), 
            LCB = avg_quantity - 1.96*SE, 
            UCB = avg_quantity + 1.96*SE)

avg_by_hr %>% 
  ggplot(aes(x = hr, y = avg_quantity)) + 
  geom_point(position = position_dodge(width = .6)) + 
  geom_linerange(aes(x = hr, ymin = LCB, ymax = UCB), position = position_dodge(width = 0.6)) +
  theme_minimal()

avg_by_day_part <- pumping %>% 
  filter(date >= today()-21) %>% 
  mutate(hr = hour(hm(time)), 
         day_part = ifelse(hr %in% c(23, 0:6), "overnight", 
                           ifelse(hr %in% c(7:11), "morning", 
                                  ifelse(hr %in% c(12:17), "afternoon", "evening")))) %>% 
  group_by(date, hr, day_part) %>% 
  summarize(session_ttl = sum(quantity_oz)) %>% 
  ungroup() %>% 
  group_by(day_part) %>% 
  summarize(n = n(), 
            avg_quantity = mean(session_ttl), 
            SE = sd(session_ttl)/sqrt(n), 
            LCB = avg_quantity - 1.96*SE, 
            UCB = avg_quantity + 1.96*SE)

avg_by_day_part$day_part <- factor(avg_by_day_part$day_part, levels = c("overnight", "morning", "afternoon", "evening"))

avg_by_day_part %>% 
  ggplot(aes(x = day_part, y = avg_quantity)) + 
  geom_point(position = position_dodge(width = .6)) + 
  geom_linerange(aes(x = day_part, ymin = LCB, ymax = UCB), position = position_dodge(width = 0.6)) +
  labs(title = "Avg. Quantity Pumped by Time of Day", subtitle = "Last 3 weeks", x = NULL, y = "Ounces") +
  theme_minimal()
  
# pumping %>% 
#   ggplot(aes(x = date, y = quantity_oz, colour = side)) + 
#   geom_jitter() + 
#   geom_smooth() + 
#   theme_minimal()

pumping %>% 
  filter(date >= today()-21) %>% 
  mutate(hr = hour(hm(time)), 
         day_part = ifelse(hr %in% c(23, 0:6), "overnight", 
                           ifelse(hr %in% c(7:11), "morning", 
                                  ifelse(hr %in% c(12:17), "afternoon", "evening")))) %>% 
  group_by(date, hr, day_part) %>% 
  summarize(session_ttl = sum(quantity_oz)) %>% 
  ggplot(aes(x = factor(day_part, levels = c("overnight", "morning", "afternoon", "evening")), y = session_ttl)) + 
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Quantity Pumped by Time of Day", subtitle = "Last 3 weeks", x = NULL, y = "Ounces")