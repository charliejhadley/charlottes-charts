library(tidyverse)
library(ggtext)
# library(emojifont)
library(emo)
library(hrbrthemes)
library(ggtext)

ggplot(mtcars, aes(x = 1, y = mpg, fill = factor(cyl))) +
  geom_dotplot(binaxis = "y", stackgroups = TRUE, binwidth = 1, method = "histodot", pch = "22")



data_hist <- {
  set.seed(1)
  tibble(
    x = rnorm(500,
              mean = 100,
              sd = 8)
  )
}

sd_of_data <- sd(data_hist$x)
mean_of_data <- mean(data_hist$x)


gg_hist_dot_plot <- data_hist %>%
  ggplot(aes(x = x)) +
  geom_dotplot(binwidth = 1, method = "dotdensity", dotsize = 1, position = "dodge", binpositions="bygroup")
gg_hist_dot_plot
build_gg <- ggplot_build(gg_hist_dot_plot)



# Could actually use
# msleep %>%
# mutate(emoji = if_else(
#   vore == "carni", as.character(emo::ji("lion")), as.character(emo::ji("tiger"))
# ))
vec_emojis <- c("3+" = emo::ji("scream"), "3" = emo::ji("fearful"), "2" = emo::ji("confused"), "1" = emo::ji("smile"))

data_emoji_positions <- build_gg$data %>%
  as.data.frame() %>%
  as_tibble() %>%
  select(x, xmin, xmax, y, stackpos) %>%
  mutate(sds_from_mean = case_when(
    x < ( mean_of_data - 3 * sd_of_data ) ~ "3+",
    x <= ( mean_of_data - 2 * sd_of_data ) ~ "3",
    x <= ( mean_of_data - 1 * sd_of_data ) ~ "2",
    x <= ( mean_of_data + 1 * sd_of_data ) ~ "1",
    x <= ( mean_of_data + 2 * sd_of_data ) ~ "2",
    x <= ( mean_of_data + 3 * sd_of_data ) ~ "3",
    x > ( mean_of_data + 3 * sd_of_data )~ "3+"
  )) %>%
  mutate(emoji_symbol = vec_emojis[sds_from_mean])

hist_function <- function(x){dnorm(x, mean = 100, sd = 8) * 700}

geom_dnorm_fill <- function(xlim, fill_color, alpha = 0.5){

  geom_area(stat = "function",
            fun = hist_function,
            fill = fill_color,
            alpha = alpha,
            xlim = xlim)

}

data_emoji_positions %>%
  ggplot(aes(x, y = stackpos)) +
  geom_dnorm_fill(c(mean_of_data - 4 * sd_of_data,
                    mean_of_data + 4 * sd_of_data),
                  viridis::viridis(4)[4]) +
  geom_dnorm_fill(c(mean_of_data - 3 * sd_of_data,
                    mean_of_data + 3 * sd_of_data),
                  viridis::viridis(4)[3]) +
  geom_dnorm_fill(c(mean_of_data - 2 * sd_of_data,
                    mean_of_data + 2 * sd_of_data),
                  viridis::viridis(4)[2]) +
  geom_dnorm_fill(c(mean_of_data - 1 * sd_of_data,
                    mean_of_data + 1 * sd_of_data),
                  viridis::viridis(4)[1])


gg_emoji_histogram <- data_emoji_positions %>%
  ggplot(aes(x, y = stackpos)) +
geom_dnorm_fill(c(mean_of_data - 4 * sd_of_data,
                  mean_of_data + 4 * sd_of_data),
                viridis::viridis(4)[4]) +
geom_dnorm_fill(c(mean_of_data - 3 * sd_of_data,
                  mean_of_data + 3 * sd_of_data),
                viridis::viridis(4)[3]) +
geom_dnorm_fill(c(mean_of_data - 2 * sd_of_data,
                  mean_of_data + 2 * sd_of_data),
                viridis::viridis(4)[2]) +
geom_dnorm_fill(c(mean_of_data - 1 * sd_of_data,
                  mean_of_data + 1 * sd_of_data),
                viridis::viridis(4)[1]) +
geom_richtext(data = tibble(label = str_glue("{emo::ji('smile')} represent <span style='color:{viridis::viridis(4)[1]};font-weight:bold'>68% of the data</span>",
                                             "<br>",
                                             "{emo::ji('confused')} represent <span style='color:{viridis::viridis(4)[2]};font-weight:bold'>95% of the data</span>",
                                             "<br>",
                                             "{emo::ji('fearful')} represent <span style='color:{viridis::viridis(4)[3]};font-weight:bold'>99.7% of the data</span>",
                                             "<br>",
                                             "{emo::ji('scream')} represent <span style='color:{viridis::viridis(4)[4]};font-weight:bold'>the rest of the data</span>")),
              aes(label = label),
              family = "DM Sans",
              label.padding = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
              label.margin = unit(c(0, 0, 0, 0), "lines"),
              size = 7,
              x = 110,
              y = 30,
              hjust=0) +
  geom_curve(
    data = tibble(x = 120, y = 15, xend = max(data_emoji_positions$x) - 1, yend = 1),
    aes(x, y, yend = yend, xend = xend),
    # x = 120, y = 10, xend = max(data_emoji_positions$x), yend = 1,
    # data = df,
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = 0.2,
    angle = 90
  ) +
  # geom_point() +
  geom_richtext(data = tibble(x = 115,
                y = 15,
                label = "Yup, that's me. You're probably<br>wondering how I ended up in<br> this situation..."),
                aes(x, y, label = label),
                label.colour = "transparent",
                hjust = 0,
                family = "Comic Sans MS",
                label.padding = unit(c(0, 0.25, 0.25, 0.25), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                size = 6) +
  geom_richtext(aes(label = emoji_symbol),
                size = 5,
                fill = NA,
                label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_y_continuous(expand = expansion(add = c(0, 5))) +
  NULL +
  labs(title = "#30DayChartChallenge 2022-04-02 Pictogram: Emojis and Standard Deviations",
       subtitle = "With apologies to everyone I present the <i>Emoji Standard Deviation Chart</i>",
       x = "",
       y = "") +
theme_ipsum_rc(grid="") +
  theme(plot.title = element_text(family = "Arvo"),
        plot.subtitle = element_markdown(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

gg_emoji_histogram %>%
  ggsave(here::here("_posts", "2022-04-01-30daychartchallenge-2022-04-02-pictogram", "gg_emoji_histogram.png"),
         .,
         width = 18,
         height = 10)


