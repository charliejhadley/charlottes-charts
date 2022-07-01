library(tidyverse)
library(tidytext)
library(gt)

recipes_raw <-
  read_table(
    here::here(
      "_posts",
      "2022-04-20-30daychartchallenge-2022-04-XX-food-pairing",
      "srep00196-s3.csv"
    ),
    skip = 4,
    col_names = c("raw_recipe", "other")
  )

recipes_data <- recipes_raw %>%
  mutate(cuisine = str_extract(raw_recipe, "[A-z]{1,}(?=,)")) %>%
  mutate(
    ingredients_list = str_remove(raw_recipe, "[A-z]{1,},"),
    ingredients_list = str_split(ingredients_list, ",")
  ) %>%
  select(cuisine, ingredients_list) %>%
  mutate(recipe_id = row_number()) %>%
  relocate(recipe_id)

most_popular_ingr_by_cuisine <- recipes_data %>%
  select(-recipe_id) %>%
  unnest(ingredients_list) %>%
  add_count(cuisine, ingredients_list, name = "n_ingredient_by_cuisine") %>%
  unique() %>%
  group_by(cuisine) %>%
  slice_max(n_ingredient_by_cuisine, n = 10) %>%
  ungroup()

most_popular_ingr_by_cuisine %>%
  mutate(ingredients_list = reorder_within(ingredients_list, n_ingredient_by_cuisine, cuisine)) %>%
  ggplot(aes(y = n_ingredient_by_cuisine,
             x = ingredients_list)) +
  geom_col() +
  facet_wrap(~ cuisine,
             scales = "free") +
  scale_x_reordered() +
  coord_flip()


highest_scoring_recipes <- recipes_data %>%
  unnest(ingredients_list) %>%
  add_count(cuisine, ingredients_list, name = "n_ingredient_by_cuisine") %>%
  group_by(recipe_id) %>%
  mutate(score_ingredient_list = sum(n_ingredient_by_cuisine) / n()) %>%
  ungroup() %>%
  group_by(cuisine) %>%
  slice_max(score_ingredient_list, n = 5) %>%
  mutate(ingr = paste(ingredients_list, collapse = ", ")) %>%
  ungroup() %>%
  select(cuisine, score_ingredient_list, ingr)

highest_scoring_recipes %>%
  View()




