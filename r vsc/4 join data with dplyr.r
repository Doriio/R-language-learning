# inner_join
sets %>%
  inner_join(themes, by = c("theme_id" = "id"),
             suffix = c("_set", "_theme")) %>% # themes is another table
  count(name_theme, sort = TRUE)

parts %>% head(3)
part_categories %>% head(3)
inventories %>% head(3)
sets %>% head(3)
inventory_parts %>% head(3)
colors %>% head(3)

sets %>%
  inner_join(inventories, by = "set_num") %>% # same name
  filter(version == 1)

# join three or more tables
sets %>%
  inner_join(inventories, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))

# left_join
batmobile <- inventory_parts_joined %>%
  filter(set_num == "7784-1") %>%
  select(-set_num)

batwing <- inventory_parts_joined %>%
  filter(set_num == "70916-1") %>%
  select(-set_num)

batmobile %>%
  inner_join(batwing, by = c("part_num", "color_id"),
             suffix = c("_batmobile", "_batiwng")) %>%
  left_join(batwing, by = c("part_num", "color_id"),
            suffix = c("_batmobile", "_batiwng"))

sets %>%
  left_join(inventory_version_1, by = c("set_num")) %>%
  # Filter for where version is na
  filter(is.na(version) == TRUE)

# right_join
batmobile %>%
  right_join(batwing, by = c("part_num", "color_id"),
             suffix = c("_batmobile", "_batiwng"))

library(tidyr)
sets %>%
  count(theme_id, sort = TRUE) %>%
  right_join(themes, by = c("theme_id" = "id")) %>%
  replace_na(list(n = 0)) # replace any na with 0 in n column

# join table to themselves
themes %>%
  inner_join(themes, by = c("parent_id" = "id"),
             suffix = c("_child", "_parent")) %>%
  filter(name_parent == "The Lord of the Rings")

themes %>%
  # Inner join the themes table
  inner_join(themes, by = c("id" = "parent_id"),
             suffix = c("_parent", "_child")) %>%
  # Filter for the "Harry Potter" parent name
  filter(name_parent == "Harry Potter")

# the full_join
batmobile %>%
  full_join(batwing, by = c("part_num", "color_id"),
            suffix = c("_batmobile", "_batiwng")) %>%
  replace_na(list(quantity_batmobile = 0, quantity_batwing = 0))

# semi_join in x also in y
batmobile %>%
  semi_join(batwing, by = c("color_id", "part_num"))

# anti_join in x not in y
batmobile %>%
  anti_join(batwing, by = c("color_id", "part_num"))

# visualizing set differences
batmobile_colors <- batmobile %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity))

batwing_colors <- batwing %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity))

colors_joined <- batmobile_colors %>%
  full_join(batwing_colors, by = "color_id",
            suffix = c("_batmobile", "_batwing")) %>%
  replace_na(list(total_batmoile = 0, total_batwing = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>%
  mutate(total_batmoile = total_batmoile / sum(total_batmoile),
         total_batwing = total_batwing / sum(total_batwing),
         difference = total_batmoile - total_batwing)

library(ggplot2)
library(forcats)

color_palette <- setNames(colors_joined$rgb, colors_joined$name)

colors_joined %>%
  mutate(name = fct_reorder(name, difference)) %>%
  ggplot(aes(name, difference, fill = name)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = color_palette, guide = "none")

# Case Study: Joins on Stack Overflow Data
questions_with_tags <- questions %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags, by = c("tag_id" = "id")) %>%
  count(tag_name, sort = TRUE)

questions_with_tags %>%
  # Group by tag_name
  group_by(tag_name) %>%
  # Get mean score and num_questions
  summarize(score = mean(score),
            num_questions = n()) %>%
  # Sort num_questions in descending order
  arrange(desc(num_questions))

questions %>%
  inner_join(answers, by = c("id" = "question_id"))

# bind_rows verb
questions %>%
  bind_rows(answers)

questions_type <- questions %>%
  mutate(type = "question")

answers_type <- answers %>%
  mutate(type = "answer")

posts <- bind_rows(questions_type, answers_type)

posts %>%
  group_by(type) %>%
  summarize(average_score = mean(score))

library(lubridate)

question_answers_year <- posts %>%
  mutate(year = year(creation_date)) %>%
  count(year, type)

ggplot(question_answers_year, aes(year, n, color = type)) +
  geom_line()