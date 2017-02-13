#### Clean up nursing data ####

colnames(data) <- c("datetime", "datetime_end", "duration", "activity", "quantity", "extra_data", "text", "notes", "caregiver")

nursing <- data %>% filter(activity == "Nursing") %>% select(-activity, -datetime_end, -quantity, -extra_data, -notes, -caregiver)
nursing <- nursing %>% separate(datetime, into = c("date", "time"), " ")
nursing$date <- as.Date(nursing$date, format = "%m/%d/%y")
nursing$text <- gsub("Simon nursed ", "", nursing$text)
nursing$text <- gsub("\\(|\\)", "", nursing$text)
nursing$text <- gsub("m", "", nursing$text)

# if no side is provided, assume total time is evenly divided
nursing[!grepl("right|left", nursing$text), "text"] <- sapply(as.numeric(nursing[!grepl("right|left", nursing$text), "text"]), function(x) paste0(x/2, " right, ", x/2, " left"))

# if only one side nursed, mark the other side as 0 minutes
nursing[!grepl(",", nursing$text), "text"] <- sapply(nursing[!grepl(",", nursing$text), "text"], function(x) ifelse(gsub("[0-9]{1,2} ", "", x) == "right", paste0(x, ", 0 left"), paste0(x, ", 0 right")))

nursing <- nursing %>% separate(text, c("dur1", "side1", "dur2", "side2"), sep = "\\s|, ")