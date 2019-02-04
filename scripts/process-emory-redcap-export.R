#------------------------------------------------------------------------------*
# Load and prepare data from C4 using the most recent RedCap export
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare analysis ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")
library(package = "patchwork")

# Helper functions
as_time <- . %>% hms::parse_hm(tz = "UTC-6")




#------------------------------------------------------------------------------*
# Get data ----
#------------------------------------------------------------------------------*

# Get most recent export file
file <- list.files(
  path = "data/exports", pattern = "*.csv", full.names = TRUE
) %>%
  grep("Guatemala", ., ignore.case = TRUE, value = TRUE) %>%
  tibble(
    file = .,
    export_date = lubridate::ymd_hm(
      gsub("[^0-9]+_([-0-9_]+).csv", "\\1", .)
    )
  ) %>%
  slice(which.max(export_date))


# Read in data
forms <- read_csv(file = file$file)

c4 <- forms %>%
  select(record_id, redcap_event_name, matches("c4_")) %>%
  filter(
    # Keep only those with "service visit id"
    grepl("^34", record_id)
  ) %>%
  set_names(gsub("^c4_", "", names(.)))


c4 %>%
  filter(
    !is.na(oxy_60)
  ) %>%
  select(record_id, dist, age, matches("^oxy"), matches("^rr")) %>%
  select(-record_id) %>%
  group_by(dist) %>%
  summarize_if(
    is.numeric,
    funs(min = min, mean = mean, max = max)
  ) %>%
  gather(key = variable, value = value, -dist)



lus_data_emory <- forms %>%
  select(record_id, redcap_event_name, matches("c34_")) %>%
  filter(
    # Keep only those with "service visit id"
    !grepl("^[123]", record_id)
  ) %>%
  select(
    record_id, c34_date, lus_by = c34_by, c34_normal, c34_interpretable,
    c34_pneumonia, c34_atelectasis, c34_interstitial
  ) %>%
  set_names(gsub("^c34_", "", names(.)))
