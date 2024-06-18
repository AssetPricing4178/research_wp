install.packages(c("tidyverse", "gganimate", "ggrepel"))
library(tidyverse)
library(gganimate)
library(ggrepel)

# Install and load fredr package
install.packages("fredr")
library(fredr)

# Set your FRED API key (replace 'your_api_key_here' with your actual API key)
fredr_set_key("your_api_key_here")

# Fetch data for GS10 and GS2
gs10 <- fredr(series_id = "GS10", frequency = "m", observation_start = "2000-01-01")
gs2 <- fredr(series_id = "GS2", frequency = "m", observation_start = "2000-01-01")

# Merge data and calculate spread
treasury_spread <- merge(gs10, gs2, by = "date") %>%
  mutate(spread = GS10 - GS2)

# Plotting the spread using ggplot2
p <- treasury_spread %>%
  ggplot(aes(x = date, y = spread)) +
  geom_line(color = "#0072B2", size = 1) +
  geom_label_repel(data = subset(treasury_spread, date == max(date)),
                   aes(label = sprintf("Spread: %.2f%%", spread)),
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"),
                   segment.color = "transparent",
                   size = 6,
                   color = "#0072B2",
                   fill = "white",
                   nudge_y = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Spread between GS10 and GS2",
       x = "Date", y = "Spread (%)",
       caption = "Data source: FRED") +
  theme_minimal()

# Animate the plot (optional, requires gganimate)
animate(p, nframes = 100, duration = 10, end_pause = 20)


