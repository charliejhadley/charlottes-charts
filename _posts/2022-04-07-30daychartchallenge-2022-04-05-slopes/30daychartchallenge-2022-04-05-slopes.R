                  slope = case_when(aspect_ratio > 1.2 ~ 1,
                                    aspect_ratio > 0.8 ~ 0,
                                    aspect_ratio < 0.8 ~ -1))) +
  facet_geo(~ name,
            grid = us_state_contiguous_grid1) +
  theme_minimal() +
  theme(strip.text = element_text(color = "white")) +
  NULL

gg_us_slopes_geo_faceted

gg_legend_slopes <- tribble(
  ~slope_label, ~intercept, ~slope,
  "w > h<br>Ratio > 1.2", 0, 1,
  "w ~ h<br>Ratio 0.8 - 1.2", 0.5, 0,
  "w < h<br>Ratio < 1.2", 1, -1
) %>%
  mutate(slope_label = fct_reorder(slope_label, slope)) %>%
  ggplot() +
  geom_abline(aes(intercept = intercept,
                  slope = slope)) +
  facet_grid(slope_label ~ .,
             switch = "y") +
  coord_fixed() +
  theme_ipsum_rc() +
  theme(strip.text.y.left = element_markdown(angle = 0, hjust = 0.5,
                                             family = "Arvo"))

gg_legend_slopes

gg_constructed <- ( gg_us_slopes_geo_faceted +
  gg_legend_slopes ) +
  plot_layout(widths = c(8, 1)) +
  plot_annotation(title = "#30DayChartChallenge 2022-04-05 - Slopes",
                  subtitle = "Aspect ratios of the contiguous US as slopes",
                  caption = "@charliejhadley",
                  theme = theme(plot.title = element_text(hjust = 0,
                                                          size = 18, margin = margin(b = 10),
                                                          family = "Roboto Condensed", face = "bold"),
                                plot.subtitle = element_text(hjust = 0,
                                                             size = 13, margin = margin(b = 15)))) +
  NULL

gg_constructed

ggsave(here::here("_posts", "2022-04-07-30daychartchallenge-2022-04-05-slopes", "gg-slopes.png"),
       gg_constructed,
       width = 12,
       height = 6)






