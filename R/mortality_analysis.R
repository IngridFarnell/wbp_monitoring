# Simple analysis of mortality data
# Does site type and elevation have an effect on mortality

# Ingrid Farnell
# Oct 29, 2026

# Load libraries
library(data.table)
library(fs)


# Set folders
in_dir <- path("01_clean_data")
out_dir <- path("02_outputs")

# load data
dat <- fread(path(in_dir, "mortality.csv"))

#- 5-, 10-, 15-year interval analysis
# Function to prepare data and run a model for each interval
run_mortality_model <- function(dat, dead_col, interval) {
  
  # Select sites with valid mortality counts and elevation
  d <- copy(dat)
  
  d[, `:=`(
    dead = as.numeric(as.character(get(dead_col))),
    planted = as.numeric(as.character(planted_trees)),
    elevation_num = as.numeric(as.character(elevation))
  )]
  
  d <- d[
    !is.na(dead) &
      !is.na(planted) &
      !is.na(elevation_num) &
      dead >= 0 &
      planted > 0 &
      dead <= planted &
      !is.na(Site_type)
  ]
  
  d[, Site_type := droplevels(factor(Site_type))]
  
  cat("\n---", interval, "year mortality ---\n")
  cat("Number of sites:", nrow(d), "\n")
  
  # Summarize mortality by site type
  print(d[, .(
    sites = .N,
    planted = sum(planted),
    dead = sum(dead),
    mortality_pct = round(100 * sum(dead) / sum(planted), 1)
  ), by = Site_type])
  
  # Fit model, if sufficient site-type variation exists
  if (uniqueN(d$Site_type) < 2 || nrow(d) < 4) {
    cat("Insufficient data to fit this model reliably.\n")
    return(NULL)
  }
  
  model <- glm(
    cbind(dead, planted - dead) ~
      Site_type + scale(elevation_num),
    data = d,
    family = quasibinomial(link = "logit")
  )
  
  print(summary(model))
  
  # Overall tests of each predictor
  print(drop1(model, test = "F"))
  
  invisible(model)
}

# Run each monitoring interval
model5 <- run_mortality_model(
  dat, "num_dead_5", 5
)

model10 <- run_mortality_model(
  dat, "num_dead10", 10
)

model15 <- run_mortality_model(
  dat, "num_dead15", 15
)

