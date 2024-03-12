library(tidyverse)
library(ggplot2)
# library(anytime)
library(scales)

# import
# trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')
trashwheel <- readr::read_csv('c:/R/projects/tidytuesday/trashwheel.csv')

# tidy
# use View() to open an interactive data viewer
View(trashwheel)

# how many trash wheels are there?
unique(trashwheel$Name)
unique(trashwheel$ID)

# is there any data missing?
sapply(trashwheel, function(x) sum(is.na(x)))
# which(is.na(trashwheel$Dumpster))
which(is.na(trashwheel$PlasticBottles))
which(is.na(trashwheel$Polystyrene))
which(is.na(trashwheel$CigaretteButts))

trashwheel[732, ]
# I think it's safe to ignore that one datapoint

# what are the data types?
str(trashwheel)

# it looks like there are rows for the total amounts collected from professor, captain, and gwynnda, but not mister,
# so that is something to be aware of
# the cleaning script only removes empty rows for mister.
# the csv file was updated
# looking at the cleaning script, sportsballs and glass bottles are not counted on each vessel. 

# transform/wrangle

# what are the timeframes of data collected?
# how many dumpsters do each have?
# what are the summary statistics for the rest of the categories, starting with weight and ending with homes powered?
# how long since the start of the project did it take to start powering homes?

# # trashwheel$Date <- anydate(trashwheel$Date)  put in na for dates that didn't have the full year
# # trashwheel$Date <- as.Date(trashwheel$Date, format = "%m/%d/%Y")
# # trashwheel_dates <- trashwheel
# # trashwheel_dates$Date <- as.Date(trashwheel$Date, format = '%m/%d/%Y')
# 
# # adjust_century <- function(date_vector) {
# #   dates <- as.Date(date_vector, format = '%m/%d/%y')
# # # Find dates that were incorrectly assigned to the year 0014
# #   incorrect_dates <- which(format(dates, '%Y') == '0014')
# # # Correct those dates by adding 2000 years
# #   dates[incorrect_dates] <- dates[incorrect_dates] + years(2000)
# #   return(dates)
# # }
# 
# # Apply the function to your date column
# # trashwheel$Date <- adjust_century(trashwheel$Date)
# # 
# # View(trashwheel_dates)
# # 
# # timeframes <- trashwheel_dates %>%
# #   group_by(ID) %>%
# #   summarize(
# #     MinDate = min(Date, na.rm = TRUE),
# #     MaxDate = max(Date, na.rm = TRUE)
# #   )
# # View(timeframes)
# 
# # get the dates into a standard format
# 
# # Identify rows with two-digit years (e.g., 9/24/20)
# # Identifies rows with dates in the format "MM/DD/YY" using regular expressions (str_detect).
# # \\ matches "\",  \ is escape sequence prefix. ANY ONE digit \d. exactly two times {2}, might need to be {1,}?, then / is in the date, end of line $.
# 
# # this gives an error in the function below: Error in ifelse(two_digit_years, paste0(trashwheel$Date, "20"), trashwheel$Date) : 
# # 'list' object cannot be coerced to type 'logical'. Put it directly in function.
# # two_digit_years <- (str_detect(trashwheel$Date, "\\d{1,2}/\\d{1,2}/\\d{2}$")) 
# # View(two_digit_years)
# 
# # Create a new date column with consistent format
# # trashwheel$Date_clean <- as.Date(
# #   ifelse(str_detect(trashwheel$Date, "\\d{1,2}/\\d{1,2}/\\d{2}$"),
# #          paste0("20", trashwheel$Date),  # Add "20" for two-digit years 
# #          trashwheel$Date),  # Use existing format for four-digit years
# #   format = "%m/%d/%Y"  # Specify desired output format (YYYY-MM-DD possible too)
# # )
# 
# # View(trashwheel)
# # 
# # timeframes <- trashwheel %>%
# #   group_by(ID) %>%
# #   summarize(
# #     MinDate = min(Date_clean, na.rm = TRUE),
# #     MaxDate = max(Date, na.rm = TRUE)
# #   )
# # View(timeframes)

# just frickin use excel.

# timeframes <- trashwheel %>%
#   group_by(ID) %>%
#   summarize(
#     MinDate = min(Date, na.rm = TRUE),
#     MaxDate = max(Date, na.rm = TRUE)
#   )
# View(timeframes)
# That's just not a helpful function. goes by the month, not the year. so let's do that for the year.
timeframes <- trashwheel %>%
  group_by(ID) %>%
  summarize(
    MinDate = min(Year, na.rm = TRUE),
    MaxDate = max(Year, na.rm = TRUE)
  )

View(timeframes)

# What type of trash is collected the most? 
# get sums of trash
# Example category labels (replace with your actual labels)
trash_cat <- colnames(trashwheel[,9:15])
trash_cat

# Calculate sum of values in each column
trash_sums <- colSums(trashwheel[, 9:15], na.rm = TRUE)
trash_sums

# create a bar chart of types and counts of trash
# n <- 3
# sampledata <- data.frame(
#   a = rbinom(n, 1, 0.7),
#   b = rbinom(n, 1, 0.6),
#   c = rbinom(n, 1, 0.5),
#   d = rbinom(n, 1, 0.2),
#   e = rbinom(n, 1, 0.3)
# )
# View(sampledata)

### Sum the data using apply & use this for beautiful barplot
# sumdata<-data.frame(value=apply(sampledata,2,sum))
# View(sumdata)
# 
# sumdata$key<-rownames(sumdata)
# 
# ggplot(data=sumdata, aes(x=key, y=value, fill=key)) +
#   geom_bar(colour="black", stat="identity")

png("trashwheel.png")
options(scipen=999999999)
par(mar=c(5, 5, 3, 1))
barplot(colSums(trashwheel[,9:15], na.rm = TRUE),
        cex.names=0.7,
        main = "Types of trash collected",
        ylab = "",
        xlab = "Type of object",
        yaxt = "n",
        ylim = c(0, max(colSums(trashwheel[,9:15], na.rm = TRUE)) * 1.2))


# axis(2, las = 2, at = axTicks(2), labels = scales::comma(axTicks(2)), cex.axis=0.7)
y_range <- range(0, max(colSums(trashwheel[,9:15], na.rm = TRUE)) * 1.2)
axis(2, las = 2, at=seq(0, y_range[2], by=2500000), 
     labels=scales::comma(seq(0, y_range[2], by=2500000)), 
     cex.axis=0.7)
mtext("Count", side=2, line=4)
dev.off()

# Do the different Trash Wheels collect different sets of trash? 
# Are there times of the year when more or less trash is collected?

# so the wheels have been operating for different amounts of time.


# visualization

# model

# communication