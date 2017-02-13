#### Clean up weight data ####

weight <- data %>% filter(activity == "Weight") %>% select(datetime, quantity)
weight <- weight %>% separate(datetime, into = c("date", "time"), " ") %>% select(-time)
weight$date <- as.Date(weight$date, format = "%m/%d/%y")
weight <- weight %>% rename(wt = quantity)
