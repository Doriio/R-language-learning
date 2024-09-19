# regular expressions
# pattern = "^a" start with a
# pattern = "a$" end with a
#.*: any character
#\\s: Match a space.
#[0-9]+: Match the numbers 0 to 9, at least once (+).
#([0-9]+): make parts of the matching string available

# Use grepl() to match for "edu"
grepl(pattern = "edu", emails)
grepl(pattern = "@.*\\.edu$", emails)
# Use grep() to match for "edu", save result to hits
hits <- grep(pattern = "edu", emails)
# Subset emails using hits
emails[hits]

# sub pattern = a replacement = "o",x = animals 第一个值
sub(pattern = "@.*\\.edu$", replacement = "@datacamp.edu", x = emails)
# gsub 全部替换

# times and dates
today <- Sys.Date()
now <- Sys.time()
# Convert the strings to dates: date1, date2, date3
date1 <- as.Date(str1, format = "%b %d, '%y")
date2 <- as.Date(str2)
date3 <- as.Date(str3, format = "%d/%B/%Y")

# Convert dates to formatted strings
format(date1, "%A")
format(date2, "%d")
format(date3, "%b %Y")

# %Y: 4-digit year (1982)
# %y: 2-digit year (82)
# %m: 2-digit month (01)
# %d: 2-digit day of the month (13)
# %A: weekday (Wednesday)
# %a: abbreviated weekday (Wed)
# %B: month (January)
# %b: abbreviated month (Jan)
