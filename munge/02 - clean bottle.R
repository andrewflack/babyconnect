#### Clean up bottle data ####

bottle <- data %>% filter(activity == "Bottle") %>% select(-activity, -datetime_end, -notes, -caregiver, -text)
bottle <- bottle %>% rename(type = extra_data)
bottle <- bottle %>% separate(datetime, into = c("date", "time"), " ")
bottle$date <- as.Date(bottle$date, format = "%m/%d/%y")
