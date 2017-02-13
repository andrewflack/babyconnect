#### Clean up pumping data ####

pumping <- data %>% filter(activity == "Pumping") %>% select(-activity, -datetime_end, -duration, -notes, -caregiver)
pumping <- pumping %>% rename(units = extra_data)
pumping <- pumping %>% separate(datetime, into = c("date", "time"), " ")
pumping$date <- as.Date(pumping$date, format = "%m/%d/%y")
pumping <- pumping %>% mutate(quantity_oz = ifelse(units == "ml", quantity*0.033814, quantity)) %>% select(-quantity, -units)
pumping$text <- as.character(pumping$text)

# if side is not specified, split evenly between sides
pumping[!grepl("\\(|\\)", pumping$text), "text"] <- sapply(pumping[!grepl("\\(|\\)", pumping$text), "quantity_oz"], function(x) paste0(x/2, " Expressed (R), ", x/2, " Expressed (L)"))

# remove quantities from text column (they are contained in quantity_oz now)
pumping[!grepl(",", pumping$text), "text"] <- gsub(".*\\d+\\.*\\d* oz |\\d+\\.*\\d* ml ", "", pumping[!grepl(",", pumping$text), "text"])

# take the rows where quantity is split evenly and split into new rows
pumping <- pumping %>% mutate(text = strsplit(as.character(text), ", ")) %>% unnest(text)
pumping[grep("[0-9]", pumping$text), "quantity_oz"] <- as.numeric(str_extract(pumping[grep("[0-9]", pumping$text), "text"], "\\d+\\.*\\d*"))

# extract right/left and make a factor
pumping <- pumping %>% mutate(side = as.factor(str_extract(pumping$text, "R|L"))) %>% select(-text)
