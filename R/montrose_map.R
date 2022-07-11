
library(sf)
library(tidyverse)
library(mapview)
library(nhdplusTools)

city_limits <- sf::read_sf(here::here("data", "shp", "MontroseCityLimits.shp"))
mapview(city_limits)
sf::st_as_sfc(
  sf::st_bbox(city_limits)
  )
nhd_streams <- nhdplusTools::get_nhdplus(
  AOI = city_limits
  )

mapview(city_limits) + nhd_streams
nhd_streams$gnis_name

streams_u <- 
  nhd_streams %>% 
  dplyr::mutate(
    name = dplyr::case_when(
      comid == "9769076" ~ "Cedar Creek",
      TRUE               ~ gnis_name
    )
  ) %>% 
  dplyr::select(comid, name, geometry) %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarise(geometry = sf::st_union(geometry)) %>% 
  dplyr::ungroup() %>% 
  sf::st_cast("MULTILINESTRING")

mapview(city_limits) + streams_u
RColorBrewer::brewer.pal(10, "Spectral")
city_limits_plot <- 
  ggplot() +
  geom_sf(data = city_limits) +
  geom_sf(data = streams_u, aes(col = name), size = 1.5) +
  labs(
    title = "Montrose City Limits",
    col   = ""
  ) +
  # scale_color_continuous()
  scale_color_manual(values = c("#9E0142", "#D53E4F",  "#F46D43", "#66C2A5", "#3288BD", "#5E4FA2")) +
# scale_color_manual(values = rev(c("#9E0142", "#D53E4F", "#F46D43", "#66C2A5", "#3288BD", "#5E4FA2"))) +
  # viridis::scale_color_viridis(discrete = T, direction = 1, option = "H") +
  ggthemes::theme_map() +
  theme(
    # legend.position = "right",
    plot.title      = element_text(size = 20,
                                   hjust = .5,
                                   face = "bold"
                                   ),
    legend.text     = element_text(size = 12)
  )
city_limits_plot
# RColorBrewer::brewer.pal(10, "Spectral")

ggsave(
  here::here("img", "montrose_city_limits.png"),
  city_limits_plot,
  width  = 14,
  height = 10
)

city_limits_plot2 <- 
  ggplot() +
  geom_sf(data = city_limits) +
  geom_sf(data = streams_u, col = "dodgerblue", size = 1.5) +
  labs(
    title = "Montrose City Limits",
    col   = ""
  ) +
  # scale_color_continuous()
  # scale_color_manual(values = c("#9E0142", "#D53E4F",  "#F46D43", "#66C2A5", "#3288BD", "#5E4FA2")) +
  # scale_color_manual(values = rev(c("#9E0142", "#D53E4F", "#F46D43", "#66C2A5", "#3288BD", "#5E4FA2"))) +
  # viridis::scale_color_viridis(discrete = T, direction = 1, option = "H") +
  ggthemes::theme_map() +
  theme(
    # legend.position = "right",
    plot.title      = element_text(size = 20,
                                   hjust = .5,
                                   face = "bold"
    ),
    legend.text     = element_text(size = 12)
  )
city_limits_plot2
# RColorBrewer::brewer.pal(10, "Spectral")

ggsave(
  here::here("img", "montrose_city_limits_no_stream_names.png"),
  city_limits_plot2,
  width  = 14,
  height = 10
)
