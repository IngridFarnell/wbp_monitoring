# Cumulative mortality
# This script graphs the monitoring data cumulative survival through time for each site

# Ingrid Farnell
# Sept 10, 2026

#-- Load Libraries
library(data.table)
library(readxl) # reading excel files
library(fs) # seting directories
library(ggplot2) # graphs
library(plotly) # interactive plots

#-- Set folders
in_dir <- path("01_clean_data")
out_dir <- path("02_outputs")

#-- Load data
mortality <- fread(path(in_dir, "mortality.csv"))
setnames(mortality, "name", "Site")

#-- Clean data
# Reshape mortality data to long format
mortality_long <- melt(
  mortality,
  id.vars = c("Site", "YearPlanted"),
  measure.vars = c(
    "Year1_mortality",
    "Year5_mortality",
    "Year10_mortality",
    "Year15_mortality"
  ),
  variable.name = "Monitoring",
  value.name = "Mortality"
)

# Extract years since planting
mortality_long[, YearsSincePlanting := fifelse(
  Monitoring == "Year1_mortality", 1,
  fifelse(
    Monitoring == "Year5_mortality", 5,
    fifelse(
      Monitoring == "Year10_mortality", 10,
      15
    )
  )
)]

# Remove observations that haven't been monitored yet
mortality_long <- mortality_long[!is.na(Mortality)]


#-- Plot cumulative mortality for each site
ggplot(
  mortality_long,
  aes(
    x = YearsSincePlanting,
    y = Mortality,
    group = Site,
    colour = Site
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Cumulative mortality (%)"
  ) +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15),
    name = "Years since planting"
  ) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

# interactive
mortality_p <- ggplot(
  mortality_long,
  aes(
    x = YearsSincePlanting,
    y = Mortality,
    group = Site,
    colour = Site,
    text = paste0(
      "Site: ", Site,
      "<br>Years since planting: ", YearsSincePlanting,
      "<br>Cumulative mortality: ", round(Mortality, 1), "%"
    )
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Cumulative mortality (%)"
  ) +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15),
    name = "Years since planting"
  ) +
  theme_classic() +
  theme(
    legend.position = "right"
  )

ggplotly(mortality_p, tooltip = "text")


# Variation among sites and overall pattern
cum.mort <- ggplot(
  mortality_long,
  aes(
    x = YearsSincePlanting,
    y = Mortality,
    group = Site
  )
) +
  geom_line(
    colour = "grey70",
    linewidth = 0.7,
    alpha = 0.7
  ) +
  geom_point(
    colour = "grey50",
    size = 2,
    alpha = 0.7
  ) +
  stat_summary(
    aes(group = 1),
    fun = median,
    geom = "line",
    linewidth = 1.5
  ) +
  stat_summary(
    aes(group = 1),
    fun = median,
    geom = "point",
    size = 3
  ) +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15),
    name = "Years since planting"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    name = "Cumulative mortality (%)"
  ) +
  theme_classic(base_size = 16)
cum.mort

# Export figure
ggsave(
  filename = file.path(out_dir, "cum_mortality_plot.png"),
  plot = cum.mort,
  width = 10,
  height = 7,
  units = "in",
  dpi = 300
)
