library(jsonlite)
library(lubridate)
library(tidyverse)


# source data: https://data.nasa.gov/resource/eva.json (with modifications)
input_file  <-  "eva-data.json"
output_file <-  "eva-data.csv" # to have a tabular output file
graph_file <-  "cumulative_eva_graph.png"

spacewalk_data <- jsonlite::fromJSON(input_file) |>
  as_tibble()

# Comment out this bit if you don't want the spreadsheet
write.csv(x = spacewalk_data,
          file = output_file)

# create cumulative duration of spacewalks
spacewalk_data <- spacewalk_data |>
  filter(!is.na(date)) |>
  mutate(
    date = ymd_hms(date),
    duration_hours = as.numeric(str_extract(duration, "\\d+")),
    duration_minutes = as.numeric(str_extract(duration, "(?<=:)\\d+")),
    duration_duration = duration(hours = duration_hours,
                                 minutes = duration_minutes)
  ) |>
  filter(!is.na(duration_duration)) |>
  arrange(date) |>
  mutate(cumulative_duration = cumsum(duration_duration))


p <-   spacewalk_data |>
  ggplot(aes(date, cumulative_duration / 60)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Total time spend in space to date (hours)") +
  theme_minimal()

ggsave(filename = graph_file, plot = p)

p
