library(tidyverse)
library(nycflights13)
library(patchwork)
library(scales)

# ------------------------------------------------------------
# 1. Prepare the data
# ------------------------------------------------------------

flights_clean <- flights %>%
  left_join(airlines, by = "carrier") %>%
  rename(carrier_name = name)

# ------------------------------------------------------------
# 2. Okabe-Ito color palette
# ------------------------------------------------------------

okabe_ito <- c(
  black          = "#000000",
  orange         = "#E69F00",
  sky_blue       = "#56B4E9",
  bluish_green   = "#009E73",
  yellow         = "#F0E442",
  blue           = "#0072B2",
  vermilion      = "#D55E00",
  reddish_purple = "#CC79A7"
)

# ------------------------------------------------------------
# 3. Publication-quality theme
# ------------------------------------------------------------

theme_publication <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      color = "grey15",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey35",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9,
      color = "grey45",
      hjust = 0
    ),
    axis.title = element_text(
      face = "bold",
      color = "grey20"
    ),
    axis.text = element_text(
      color = "grey25"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.35
    ),
    legend.position = "bottom",
    plot.margin = margin(12, 16, 12, 16)
  )

# ------------------------------------------------------------
# 4. Scatterplot data
# ------------------------------------------------------------

set.seed(123)

scatter_data_all <- flights_clean %>%
  filter(
    !is.na(dep_delay),
    !is.na(arr_delay)
  )

# Use a reproducible sample to reduce overplotting
scatter_data <- scatter_data_all %>%
  slice_sample(
    n = min(25000, nrow(scatter_data_all))
  )

# Correlation for annotation
delay_correlation <- cor(
  scatter_data$dep_delay,
  scatter_data$arr_delay,
  use = "complete.obs"
)

# Percentage of flights that arrived late after departing late
late_departure_late_arrival <- scatter_data %>%
  filter(dep_delay > 0) %>%
  summarise(
    percentage = mean(arr_delay > 0) * 100
  ) %>%
  pull(percentage)

# ------------------------------------------------------------
# 5. Scatterplot
# ------------------------------------------------------------

p_scatter <- ggplot(
  scatter_data,
  aes(
    x = dep_delay,
    y = arr_delay
  )
) +
  geom_hline(
    yintercept = 0,
    color = "grey55",
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = 0,
    color = "grey55",
    linewidth = 0.5
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey65",
    linewidth = 0.6
  ) +
  geom_point(
    color = okabe_ito["blue"],
    alpha = 0.16,
    size = 1.1
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = okabe_ito["vermilion"],
    linewidth = 1.1
  ) +
  annotate(
    "label",
    x = 105,
    y = -20,
    label = paste0(
      round(late_departure_late_arrival),
      "% of late departures\nalso arrived late"
    ),
    hjust = 1,
    vjust = 0,
    size = 3.5,
    color = "grey20",
    fill = "white",
    label.size = 0.25
  ) +
  coord_cartesian(
    xlim = c(-30, 180),
    ylim = c(-30, 180)
  ) +
  scale_x_continuous(
    breaks = seq(-30, 180, by = 30)
  ) +
  scale_y_continuous(
    breaks = seq(-30, 180, by = 30)
  ) +
  labs(
    title = "Late departures usually mean late arrivals",
    subtitle = paste0(
      "Departure and arrival delays are strongly related ",
      "(correlation = ", round(delay_correlation, 2), ")"
    ),
    x = "Departure delay (minutes)",
    y = "Arrival delay (minutes)",
    caption = "Dashed line represents equal departure and arrival delays. Display limited to −30 to 180 minutes."
  ) +
  theme_publication

# ------------------------------------------------------------
# 6. Histogram data
# ------------------------------------------------------------

histogram_data <- flights_clean %>%
  filter(!is.na(dep_delay))

percent_on_time_or_early <- histogram_data %>%
  summarise(
    percentage = mean(dep_delay <= 0) * 100
  ) %>%
  pull(percentage)

median_departure_delay <- median(
  histogram_data$dep_delay,
  na.rm = TRUE
)

# ------------------------------------------------------------
# 7. Histogram
# ------------------------------------------------------------

p_histogram <- ggplot(
  histogram_data,
  aes(x = dep_delay)
) +
  geom_histogram(
    binwidth = 10,
    boundary = 0,
    closed = "left",
    fill = okabe_ito["orange"],
    color = "white",
    linewidth = 0.25
  ) +
  geom_vline(
    xintercept = 0,
    color = okabe_ito["blue"],
    linewidth = 1
  ) +
  geom_vline(
    xintercept = median_departure_delay,
    color = okabe_ito["vermilion"],
    linewidth = 1,
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = 150,
    y = Inf,
    label = paste0(
      round(percent_on_time_or_early),
      "% departed on time or early"
    ),
    hjust = 1.05,
    vjust = 1.5,
    size = 3.5,
    color = "grey20",
    fill = "white",
    label.size = 0.25
  ) +
  annotate(
    "text",
    x = median_departure_delay,
    y = Inf,
    label = "Median",
    vjust = -0.5,
    hjust = -0.1,
    angle = 90,
    size = 3.2,
    color = okabe_ito["vermilion"]
  ) +
  coord_cartesian(
    xlim = c(-30, 180)
  ) +
  scale_x_continuous(
    breaks = seq(-30, 180, by = 30)
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Most flights depart close to schedule",
    subtitle = "Departure delays are concentrated near zero, with a long tail of severe delays",
    x = "Departure delay (minutes)",
    y = "Number of flights",
    caption = "The display is limited to −30 to 180 minutes to emphasize the main distribution."
  ) +
  theme_publication

# ------------------------------------------------------------
# 8. Boxplot data
# ------------------------------------------------------------

carrier_summary <- flights_clean %>%
  filter(!is.na(arr_delay)) %>%
  group_by(carrier_name) %>%
  summarise(
    median_arr_delay = median(arr_delay),
    .groups = "drop"
  ) %>%
  arrange(median_arr_delay) %>%
  mutate(
    carrier_name = factor(
      carrier_name,
      levels = carrier_name
    ),
    median_label = paste0(
      round(median_arr_delay),
      " min"
    )
  )

boxplot_data <- flights_clean %>%
  filter(!is.na(arr_delay)) %>%
  mutate(
    carrier_name = factor(
      carrier_name,
      levels = levels(carrier_summary$carrier_name)
    )
  )

best_carrier <- carrier_summary %>%
  slice_min(median_arr_delay, n = 1) %>%
  pull(carrier_name) %>%
  as.character()

worst_carrier <- carrier_summary %>%
  slice_max(median_arr_delay, n = 1) %>%
  pull(carrier_name) %>%
  as.character()

median_range <- range(
  carrier_summary$median_arr_delay,
  na.rm = TRUE
)

# ------------------------------------------------------------
# 9. Boxplot
# ------------------------------------------------------------

p_boxplot <- ggplot(
  boxplot_data,
  aes(
    x = arr_delay,
    y = carrier_name
  )
) +
  geom_vline(
    xintercept = 0,
    color = "grey50",
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  geom_boxplot(
    fill = okabe_ito["sky_blue"],
    color = okabe_ito["blue"],
    alpha = 0.85,
    width = 0.65,
    outlier.color = okabe_ito["vermilion"],
    outlier.alpha = 0.2,
    outlier.size = 0.9
  ) +
  geom_point(
    data = carrier_summary,
    aes(
      x = median_arr_delay,
      y = carrier_name
    ),
    color = okabe_ito["vermilion"],
    size = 2.4
  ) +
  geom_text(
    data = carrier_summary,
    aes(
      x = median_arr_delay,
      y = carrier_name,
      label = median_label
    ),
    hjust = -0.2,
    color = "grey25",
    size = 3.1
  ) +
  coord_cartesian(
    xlim = c(-40, 180),
    clip = "off"
  ) +
  scale_x_continuous(
    breaks = seq(-40, 180, by = 40)
  ) +
  labs(
    title = "Arrival-delay performance differs by carrier",
    subtitle = paste0(
      "Median arrival delays range from ",
      round(median_range[1]),
      " to ",
      round(median_range[2]),
      " minutes; ",
      best_carrier,
      " has the lowest median"
    ),
    x = "Arrival delay (minutes)",
    y = NULL,
    caption = "Dots and labels show each carrier's median arrival delay. The dashed line marks on-time arrival."
  ) +
  theme_publication +
  theme(
    axis.text.y = element_text(
      face = "bold"
    ),
    plot.margin = margin(12, 65, 12, 16)
  )

# ------------------------------------------------------------
# 10. Display plots
# ------------------------------------------------------------

p_scatter
p_histogram
p_boxplot

# ------------------------------------------------------------
# 11. Combine plots
# ------------------------------------------------------------

combined_plot <-
  (p_scatter | p_boxplot) /
  p_histogram +
  plot_layout(
    heights = c(1, 1.15)
  ) +
  plot_annotation(
    title = "Where flight delays come from",
    subtitle = "Delays are interconnected, concentrated among a minority of flights, and unevenly distributed across carriers",
    caption = "Source: nycflights13 package. Analysis covers flights departing New York City airports in 2013."
  ) &
  theme(
    plot.title = element_text(
      face = "bold",
      size = 19,
      color = "grey15"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey35"
    ),
    plot.caption = element_text(
      size = 9,
      color = "grey45",
      hjust = 0
    )
  )

combined_plot

# ------------------------------------------------------------
# 12. Save the visualizations
# ------------------------------------------------------------

# ggsave(
#   "nycflights_scatterplot.png",
#   p_scatter,
#   width = 8,
#   height = 5.5,
#   dpi = 320,
#   bg = "white"
# )
# 
# ggsave(
#   "nycflights_histogram.png",
#   p_histogram,
#   width = 8,
#   height = 5.5,
#   dpi = 320,
#   bg = "white"
# )
# 
# ggsave(
#   "nycflights_boxplot.png",
#   p_boxplot,
#   width = 9,
#   height = 6,
#   dpi = 320,
#   bg = "white"
# )
# 
# ggsave(
#   "nycflights_combined_figure.png",
#   combined_plot,
#   width = 12,
#   height = 11,
#   dpi = 320,
#   bg = "white"
# )
# 
# ggsave(
#   "nycflights_combined_figure.pdf",
#   combined_plot,
#   width = 12,
#   height = 11,
#   device = cairo_pdf,
#   bg = "white"
# )