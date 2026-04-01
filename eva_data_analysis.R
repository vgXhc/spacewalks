
library(tidyverse) #tidyverse "contains" ggplot2
library(jsonlite)
library(lubridate)


input_file  <- "./eva-data.json"
output_file <- "./eva-data.csv"
graph_file  <- "./cumulative_eva_graph.png"

# load data
eva_tbl <- jsonlite::fromJSON(input_file) |>
  as_tibble()

# basic variable transformation
eva_tbl <- eva_tbl |>
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE) ) |>
  filter(!is.na(duration), duration != "", !is.na(date))

# write tabular output file
readr::write_csv(eva_tbl, output_file)

# need to arrange by date to allow the cumulative sum to be calculated
eva_tbl <- eva_tbl |>
  arrange(date)

# duration variable is a character of hours:minutes and we need to split it
eva_tbl <- eva_tbl |>
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE) # simplify to return character matrix, 2 is max length of each element
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60 # convert to hours
    },
    cumulative_time = cumsum(duration_hours)
  )


cumulative_spacetime_plot <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)
print(cumulative_spacetime_plot)