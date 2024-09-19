# statistics call from geom or independently
# stat_bin() = geom_histogrm / geom_freqpoly()
# stat_count() = geom_bar()
# geom_smooth：method = "loess" y~x
# (se标准误差带 = FAlSE,
# span平滑程度 = 0.4,
# fullrange = TRUE)

# 使用 stat_identity 显示原始数据的条形图，而不是默认的计数
df <- data.frame(category = c("A", "B", "C"), values = c(10, 20, 30))

ggplot(df, aes(x = category, y = values)) +
  geom_bar(stat = "identity")  # 显示实际的 values 数据

ggplot(df, aes(x = category, y = values)) +
  geom_col()  # 显示实际的 values 数据

# 1创建一个示例数据框
df <- data.frame(category = c("A", "B", "A", "C", "B", "B", "A"))

# 绘制柱状图，默认使用 stat_count() 进行计数
ggplot(df, aes(x = category)) +
  geom_bar()

ggplot(df, aes(x = category)) +
  geom_bar(stat = "count")

# dummy variable
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "lm", se = FALSE, aes(group = 1))

# 2geom_count = stat_sum
ggplot(iris, aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species)) +
  geom_count(alpha = 0.4)

# geom_quantile = stat_quantile
ggplot(Vocab, aes(x = education, y = vocabulary)) +
  geom_jitter(alpha = 0.25) +
  stat_quantile(quantiles = c(0.05, 0.50, 0.95))

# stats outside geoms
ggplot(iris, aes(x = Sepal.Length,
                 y = Sepal.Width)) +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1)) + # 画标准差
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar", # errorbars
               width = 0.1)

# stat_function
mam_new <- data.frame(body = log10(MASS::mammals$body))

ggplot(mam_new, aes(x = body)) +
  geom_histogram(aes(y = ..density..)) +
  geom_rug() + # 底部加小刻度线
  stat_function(fun = dnorm, color = "red",
                args = list(mean = mean(mam_new$body), sd = sd(mam_new$body)))
# stat_qq
ggplot(mam_new, aes(sample = body)) +
  stat_qq() +
  geom_qq_line(col = "red")

set.seed(123)
xx <- rnorm(100)

library(Hmisc)
smean.sdl(xx, mult = 1) # mean + (mult)sd
mean_sdl(xx, mult = 1) # convert it to a data.frame

data <- c(10, 12, 14, 15, 16, 20)
# 计算均值及置信区间
smean.cl.normal(data)

# 02 coordinates 坐标
iris_smooth <- ggplot(iris, aes(x = Sepal.Length,
                                y = Sepal.Width,
                                color = Species)) +
  geom_point(alpha = 0.7) +
  geom_smooth() +
  xlim(c(4.5, 5.5)) + # 上下等价
  coord_cartesian(xlim = c(4.5, 5.5))

library(zoo)
sunspot_m <- data.frame(year = index(sunspot.month),
                        value = reshape2::melt(sunspot.month)$value)
ggplot(sunspot_m, aes(x = year, y = value)) +
  geom_line() +
  coord_fixed() + # default to 1:1 aspect ratio
  coord_fixed(0.055)

ggplot(msleep, aes(log10(bodywt), y = 1)) + # 正偏态转换图形
  geom_jitter() + # 防止数据重叠
  scale_x_continuous(limits = c(-3, 4), breaks = -3:4) + # 0 = 1kg 整数
  annotation_logticks(sides = "b") # bottom

ggplot(msleep, aes(bodywt, y = 1)) +
  geom_jitter() +
  scale_x_log10(limits = c(1e-03, 1e+04))

ggplot(msleep, aes(bodywt, y = 1)) +
  geom_jitter() +
  coord_trans(x = "log10")

# flipping axes
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_flip()

# coord_polar
ggplot(mtcars, aes(x = 1, fill = fcyl)) +
  # Reduce the bar width to 0.1
  geom_bar(width = 0.1) +
  coord_polar(theta = "y", start = -pi / 16, direction = 1) +
  # Add a continuous x scale from 0.5 to 1.5
  scale_x_continuous(limits = c(0.5, 1.5))

# 03 facets 分面
p <- ggplot(iris.wide, aes(x = Length, y = Width, ccolorol = Part)) +
  geom_jitter(alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  coord_fixed() +
  facet_grid(cols = vars(Species)) +
  facet_grid(. ~ Species) +
  facet_grid(rows = vars(vore, conservation), labeller = label_context) +
  facet_grid(cols = vars(vs, cyl), labeller = label_context) +
  facet_wrap(vars(vore, conservation), scales = "free", margins = TRUE)

# best practices for bar plots
