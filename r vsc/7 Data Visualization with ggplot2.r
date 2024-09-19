install.packages("ggplot2")
library(ggplot2)

MASS::mammals
ggplot(MASS::mammals, aes(x = body, y = brain)) +
  geom_point(alpha = 0.6) +
  coord_fixed() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(
    method = "lm",
    color = "#C42126",
    se = FALSE,
    size = 1
  )

posn_j <- position_jitter(0.1, seed = 136)
ggplot(iris, aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species)) +
  geom_point(position = posn_j) + # position = "dodge" geom_jitter()
  scale_x_continuous("Sepal Length", limits = c(2, 8),
                     breaks = seq(2, 8, 3), expand = c(0, 0),
                     labels = c("Setosa", "Versicolor", "Virginica")) +
  scale_color_discrete("Species") +
  labs(x = "Sepal Length(cm)", y = "Sepal Width(cm)", color = "Species") +
  theme_classic()

ggplot(mtcars, aes(wt, mpg, fill = fcyl, color = fam)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)

# Use text layer and map fcyl to label
plt_mpg_vs_wt +
  geom_text(aes(label = fcyl))

ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  # Add text layer with label rownames(mtcars) and color red
  geom_text(lable = rownames(mtcars), color = "red")

ggplot(mtcars, aes(mpg, qsec, color = fcyl, shape = fam, size = hp / wt)) +
  geom_point()

ggplot(mtcars, aes(mpg, 0)) +
  geom_jitter() +
  # Set the y-axis limits
  ylim(-2, 2)

# geometries
# scatter plot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point() +
  geom_point(data = iris.summary, shape = 15, size = 5,
             fill = "black", stroke = 2)

# histogram
ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, center = 0.05, position = "fill")

# bar plot
# original data
ggplot(sleep, aes(vore)) +
  geom_bar()

# summarized data
ggplot(iris_summ_long, aes(x = Species, y = avg)) +
  geom_col() +
  geom_errorbar(aes(ymin = avg - stdev, ymax = avg + stdev), width = 0.1)

# Plot education, filled by vocabulary
ggplot(carData::Vocab, aes(education, fill = vocabulary)) +
  # Add a bar layer with position "fill"
  geom_bar(position = "fill") +
  # Add a brewer fill scale with default palette
  scale_fill_brewer(palette = "Set1")

# line plot
ggplot(beaver1, aes(x = time, y = temp, color = factor(activ))) +
  geom_line()

ggplot(fish, aes(x = year, y = Capture, fill = Species)) +
  geom_line(position = "fill") +
  geom_ribbon(aes(ymax = Capture, ymin = 0), alpha = 0.3)

# Plot multiple time-series by grouping by species
ggplot(fish.tidy, aes(Year, Capture)) +
  geom_line(aes(group = Species))

# theme：用于自定义图形的非数据元素的样式和外观，例如标题、坐标轴、网格线、图例、背景等
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter(alpha = 0.6) +
  theme(plot.title = element_text(size = 20)) + # 标题样式
  theme(axis.title = element_text(color = "blue")) + # 坐标轴样式
  theme(line = element_blank(),
        rect = element_blank(),
        text = element_blank())

plt_prop_unemployed_over_time +
  theme(legend.position = "none") + # 图例样式
  theme(legend.position = "bottom") +
  theme(legend.position = c(0.6, 0.1))

plt_mpg_vs_wt_by_cyl +
  theme(axis.ticks.length = grid::unit(2, "lines")) +
  theme(legend.margin = margin(t = 20, r = 30, b = 40, l = 50, unit = "pt")) +
  theme(plot.margin = margin(t = 10, r = 30, b = 50, l = 70, unit = "mm"))

# define theme objects
z <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_jitter(alpha = 0.6) +
  scale_x_continuous("Sepal Length(cm)", limits = c(4, 8), expand = c(0, 0)) +
  scale_y_continuous("Sepal Width(cm)", limits = c(1.5, 5), expand = c(0, 0)) +
  scale_color_brewer("Species", palette = "Dark2",
                     labels = c("Setosa", "Versicolor", "Virginica"))

theme_iris <- theme(text = element_text(family = "serif", size = 14),
                    rect = element_blank(),
                    panel.grid = element_blank(),
                    title = element_text(color = "#8b0000"),
                    axis.line = element_line(color = "black"))

z + theme_iris

m <- ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(binwidth = 0.1, center = 0.05)

m + theme_iris +
  theme(axis.line.x = element_blank()) # remove x axis line

# built-in themes
z +
  theme_classic() +
  theme(text = element_text(family = "serif"))

install.packages("ggthemes")
library(ggthemes)
z +
  theme_tufte() +
  theme_bw() +
  theme_fivethirtyeight()

# update default themes
original <- theme_update(text = element_text(family = "serif", size = 14),
                         rect = element_blank(),
                         panel.grid = element_blank(),
                         title = element_text(color = "#9b0000"),
                         axis.line = element_line(color = "black"))

theme_set(original)

plt_prop_unemployed_over_time +
  theme_tufte() +
  # Add individual theme elements
  theme(
    # Turn off the legend
    legend.position = "none",
    # Turn off the axis ticks
    axis.ticks = element_blank(),
    # Set the axis title's text color to grey60
    axis.title = element_text(color = "grey60"),
    # Set the axis text's text color to grey60
    axis.text = element_text(color = "grey60"),
    # Set the panel gridlines major y values
    panel.grid.major.y = element_line(
      # Set the color to grey60
      color = "grey60",
      # Set the size to 0.25
      size = 0.25,
      # Set the linetype to dotted
      linetype = "dotted"
    )
  )

# effective explanatory plots
install.packages("tidyverse")
library(dplyr)
ggplot(gm2007_full, aes(LifeExp)) +
  geom_histogram()

ggplot(gm2007_full_arranged, aes(index, LifeExp, color = continent)) +
  geom_point()

# Set the color scale
palette <- brewer.pal(5, "RdYlBu")[-(2:4)]

# Add a title and caption
ggplot(gm2007, aes(x = lifeExp, y = country, color = lifeExp)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = country), size = 2) +
  geom_text(aes(label = round(lifeExp, 1)), color = "white", size = 1.5) +
  scale_x_continuous("", expand = c(0, 0), limits = c(30, 90), position = "top") +
  scale_color_gradientn(colors = palette) +
  labs(title = "Highest and lowest life expectancies, 2007",
       caption = "Source: gapminder")

# annotete 注释
# 创建基础图形
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# 在 (4, 25) 位置添加文本 "High MPG"
p + annotate("text", # 添加文本
             x = 4, y = 25, # 添加文本的位置
             label = "High MPG", color = "blue", size = 5) # 添加文本的内容

# 在坐标 (3, 15) 到 (4, 20) 之间绘制一个透明的红色矩形。
p + annotate("rect",
             xmin = 3, xmax = 4, ymin = 15, ymax = 20,
             alpha = 0.2, fill = "red")

# 从 (3, 15) 到 (4, 20) 画一条带箭头的绿线
p + annotate("segment", # 线段
             x = 3, xend = 4, y = 15, yend = 20,
             color = "green", size = 1, arrow = arrow()) # size = 粗细