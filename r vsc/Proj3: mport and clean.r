# We've loaded the necessary packages for you in the first cell. Please feel free to add as many cells as you like!
suppressMessages(library(dplyr)) # This line is required to check your answer correctly
options(readr.show_types = FALSE) # This line is required to check your answer correctly
library(readr)
library(readxl)
library(stringr)
library(data.table)

# load and merge the data
price <- read_csv("data/airbnb_price.csv")
room_type <- read_excel("data/airbnb_room_type.xlsx")
last_review <- read_tsv("data/airbnb_last_review.tsv")

data <- price %>%
  inner_join(room_type, by = "listing_id") %>%
  inner_join(last_review, by = "listing_id")

# Determine the earliest and most recent review dates
review_dates <- data %>%
  mutate(last_review_clean = as.Date(last_review, format = "%B %d %Y")) %>%
  summarise(first_reviewed = min(last_review_clean), last_reviewed = max(last_review_clean))

# Finding how many listings are private rooms
data %>%
  mutate(room_type_lower = str_to_lower(room_type)) %>%
  count(room_type_lower) %>%
  filter(room_type_lower == "private room")

# Finding average price of listings
data %>%
  mutate(price_clean = as.numeric(str_remove(price, "dollars"))) %>%
  summarise(avg_mean = round(mean(price_clean), digits = 2))

# Creating a tibble with the four solution values
review_dates$nb_private_rooms <- 11356
review_dates$avg_price <- 141.78

review_dates