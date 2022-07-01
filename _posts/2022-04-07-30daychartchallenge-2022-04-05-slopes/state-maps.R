library(sf)
library(readxl)
library(janitor)
library(rmapshaper)
library(tigris)
library(tidycensus)
library(tidyverse)
library(furrr)

plan(multisession)

acs5_vars <- load_variables(2019, "acs5")



# Get data ----------------------------------------------------------------

state_details <- read_rds("data/states_nb_name.rds") %>%
  filter(!is.na(report_type))

counties_sf <- counties() %>%
  clean_names()

contiguous_us <- states() %>%
  clean_names() %>%
  filter(!name %in% c("United States Virgin Islands",
                      "Commonwealth of the Northern Mariana Islands",
                      "Guam",
                      "American Samoa",
                      "Puerto Rico"))

contiguous_state_details <- contiguous_us %>%
  st_drop_geometry() %>%
  select(statefp, name) %>%
  as_tibble()

tract_population <- contiguous_us %>%
  pull(name) %>%
  map_df(~get_acs(geography = "tract", variables = "B01001_001",
                  state = .x)) %>%
  clean_names()

tracts_sf <- contiguous_us %>%
  pull(name) %>%
  map_df(~tracts(state = .x)) %>%
  clean_names()

tract_population_sf <- tracts_sf %>%
  select(statefp, geoid) %>%
  left_join(tract_population)



# mapping states ----------------------------------------------------------

crop_state <- function(state_name){

  bbox_state <- contiguous_us %>%
    filter(name == state_name) %>%
    st_buffer(dist = 0.4) %>%
    st_bbox()

  contiguous_us %>%
    st_crop(bbox_state)

}

bbox_state <- function(state_name){

  contiguous_us %>%
    filter(name == state_name) %>%
    st_buffer(dist = 0.4) %>%
    st_bbox()

}

make_gg_state_map <- function(state_name, census_tracts, crop_state){


  ggplot() +
    geom_sf(data = crop_state,
            fill = "grey90",
            color = "grey70") +
    geom_sf(
      data = crop_state %>%
        filter(name == state_name),
      alpha = 0,
      size = 1,
      color = "black"
    ) +
    geom_sf(
      data = filter(census_tracts,!is.na(estimate)),
      aes(fill = estimate,
          shape = "No population data for 2020"),
      size = 0.05,
      color = "grey70",
    ) +
    scale_fill_viridis_c(
      name = NULL
    ) +
    geom_sf(data = filter(census_tracts, is.na(estimate)),
            fill = "white",
            color = "grey70") +
    scale_shape(name = NULL) +
    guides(
      shape = guide_legend(override.aes = list(fill = "white"),
                           order = 1)
    ) +
    labs(title = str_glue("{state_name}: Tract level population")) +
    theme_void() +
    theme(
      legend.text = element_text(size = 11.5, family = "Calibri"),
      legend.key = element_rect(color = "grey30"),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5,
                                family = "Calibri")
    )

}

generate_state_map <- function(state_name){

  state_fip <- contiguous_state_details %>%
    filter(name == state_name) %>%
    pull(statefp)

  census_tracts_sf <- tract_population_sf %>%
    filter(statefp == state_fip)

  if(state_name %in% c("Alaska", "Texas")){

    census_tracts_sf <- census_tracts_sf %>%
      ms_simplify(keep = 0.3)

  }

  census_tracts_na_sf <- census_tracts_sf %>%
    filter(is.na(estimate))

  cropped_state <- crop_state(state_name)

  make_gg_state_map(state_name, census_tracts_sf, cropped_state)


}

# Test
# generate_state_map("Virginia")


# generate all maps -------------------------------------------------------

save_state_map <- function(gg_state_map, state_name) {

  state_bbox <- as.list(bbox_state(state_name))

  aspect_ratio <-
    {
      state_bbox$xmax - state_bbox$xmin
    } / {
      state_bbox$ymax - state_bbox$ymin
    }

  ggsave(
    here::here(
      "_posts",
      "2022-04-07-30daychartchallenge-2022-04-05-slopes",
      str_glue("state-maps/{state_name}.png")
    ),
    gg_state_map,
    width = 12,
    height = 12 / aspect_ratio
  )

}

# Test
gg_virginia <- generate_state_map("Virginia")
save_state_map(gg_virginia, "Virginia")

contiguous_state_details %>%
  pull(name) %>%
  future_walk(~save_state_map(generate_state_map(.x), .x))

