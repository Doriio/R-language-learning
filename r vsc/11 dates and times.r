# get dates recognized by r
str(as.Date("2003-02-23"))
library(anytime)
sep_10_2009 <- c("September 10 2009", "2009-09-10", "10 Sep 2009", "09-10-2009")
anytime(sep_10_2009)

# dates act like numbers
as.Date("2003-02-27") > as.Date("2003-02-28")
as.Date("2003-02-27") + 1
as.Date("2004-10-08") - as.Date("2001-03-13")
x <- c(as.Date("2003-02-27"), as.Date("2004-10-08"), as.Date("2001-03-13"))
plot(x, 1:3)
library(ggplot2)
ggplot() +
  geom_point(aes(x = x, y = 1:3))

# Set the x axis to the date column
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major)))

# Limit the axis to between 2010-01-01 and 2014-01-01
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  xlim(as.Date("2010-01-01"), as.Date("2014-01-01"))

# Specify breaks every ten years and labels with "%Y"
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")

Sys.Date() - as.Date("2001-03-13")

# time : compare subtract plot
# POSIXlt: list with named component
# POSIXct: seconds since 1970-01-01
x <- as.POSIXct("1980-01-01 00:01:00")
str(x)
as.POSIXct("2010-10-01 12:12:00", tz = "America/Los_Angeles") # 指定时区

# lubridate
install.packages("lubridate")
library(lubridate)
ymd("2001-03-13")
dmy("13/3/2001")
dmy_hm("27-02-2013 12:12pm")
parse_date_time(c("Feb 27th, 2017", "27th Feb 2017"), order = c("mdy", "dmy"))

# extract components
install.packages("dplyr")
library(dplyr)
akl_daily <- akl_daily %>%
  mutate(year = year(date),
         yday = yday(date),
         month = month(date, label = TRUE))

x <- "Monday June 1st 2010 at 4pm"
parse_date_time(x, orders = "ABdyIp")

# 生成指定日期
make_date(year = 2013, month = 2, day = 27)
make_datetime(year = 2013, month = 2, day = 27, hour = 11, min = 50, sec = 23)

wday() # weekday
yday() # day of year 1/336

library(ggplot2)
library(dplyr)
library(ggridges)

# Add columns for year, yday and month
akl_daily <- akl_daily %>%
  mutate(
    year = year(date),
    yday = yday(date),
    month = month(date, label = TRUE))

# Plot max_temp by yday for all years
ggplot(akl_daily, aes(x = yday, y = max_temp)) +
  geom_line(aes(group = year), alpha = 0.5)

# Examine distribution of max_temp by month
ggplot(akl_daily, aes(x = max_temp, y = month, height = ..density..)) +
  geom_density_ridges(stat = "density")

# round datetimes
head(release_time) %>% hour()
head(release_time) %>% floor_date(unit = "hour")
ceiling_date()
round_date() # nearest value

# 时间计算
Sys.Date() - last_release$date
difftime(Sys.Date(), last_release$date)
difftime(time1, time2, units = "secs")
difftime(time1, time2, units = "weeks")
today() # 现在的日期
now() # 现在的时间点

# time_span
days()
ddays( )
ymd("2011-01-01") + days()
# Sequence of two weeks from 1 to 26
every_two_weeks <- 1:26 * weeks(2)

# intervals
beatles <- interval(time1, time2)
time1 %--% time2
int_start(beatles)
end_start(beatles)
int_length(beatles) # 长度
as.period(beatles)
as.duration(beatles)

# 日期是否在某个时间范围内
time1 %within% beatles
# 两个interval是否有重叠的部分
int_overlaps(interval1, interval2)

# time zone
Sys.timezone()
OlsonNames() # r知道的时区
length(OlsonNames())
# setting and extract tz
mar_11 <- ymd_hms("2017-03-11 12:00:00", tz = "America/Los_Angeles")
mar_11
tz(mar_11)
# force_tz change tz without change the clock time
force_tz(mar_11, tzone = "America/New_York")
# with_tz view the same instant in a different tz
with_tz(mar_11, tzone = "America/New_York")

# Are datetime and date_utc the same moments
table(akl_hourly$datetime - akl_hourly$date_utc)

# import and export datetimes
install.packages("fasttime")
library(fasttime)
fastPOSIXct("2003-02-27")
x <- "2003-02-27"
fast_strptime(x, format = "%Y-%m-%dT%H:%M:%SZ")

library(tidyverse)
akl_hourly %>%
  select(datetime) %>%
  write_csv("tmp.csv")

my_stamp <- stamp("Tuesday October 10 2017")
my_stamp(ymd(x))
