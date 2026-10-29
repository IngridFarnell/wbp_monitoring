# Get elevation data

# Load libraries
library(sf)
library(fs)
library(elevatr) # getting elevation data
library(data.table)
library(dplyr)

# Set folders
in_dir <- path("00_raw_data")
out_dir <- path ("01_clean_data")

# Load data
dat <- st_read(file.path(in_dir, "AllPlantingLocations_2026-10-29.gpkg"))
mort_dat <- fread(path(in_dir, "Mortality_AllYears.csv"))
setnames(mort_dat, 
         old = "Site", new = "name")
# Prep data
# Create a point inside each polygon
dat_points <- st_point_on_surface(dat)

# Get elevation
elev_points <- elevatr::get_elev_point(
  locations = dat_points,
  src = "aws",
  z = 10
)

# Attach elevation to mortality data 
# only keep the mortality data rows
dat$elevation <- elev_points$elevation
mort_dat <- mort_dat %>%
  left_join(
    elev_points %>% st_drop_geometry() %>% select(name, elevation),
    by = "name"
  )

# Fill in some manually
# reiseter creek = 1244
# upper sibola = 1226
# nanika falls 2017 = 1100
# mcdonnel lake = 1180
# atna bay 2017 = 1100
# nanika falls 2014 = 950
mort_dat <- mort_dat[name == "Reiseter Creek 2017", elevation := 1244]
mort_dat <- mort_dat[name == "Upper Sibola 2025", elevation := 1226]
mort_dat <- mort_dat[name == "Nanika Falls 2017", elevation := 1100]
mort_dat <- mort_dat[name == "McDonell Lake 2017", elevation := 1180]
mort_dat <- mort_dat[name == "Atna Bay 2017", elevation := 1100]
mort_dat <- mort_dat[name == "Nanika Falls 2014", elevation := 950]
mort_dat <- mort_dat[name == "McBride low elevation 2013", elevation := 1820]
mort_dat <- mort_dat[name == "McBride high elevation 2013", elevation := 1920]

# export data
fwrite(mort_dat, file = file.path(out_dir, "mortality.csv"))
