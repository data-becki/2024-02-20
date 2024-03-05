library(tidyverse)
library(ggplot2)

# import

# tidy
# already done

# transform/wrangling
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/events.csv')
births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')

View(events)
view(births)
view(deaths)

# visualization
# look at frequency of years recorded in wikipedia article
# count(events, "year") gave the answer 'year   37', not helpful
as.data.frame(table(events$year))
# 29 unique values in events

as.data.frame(table(births$year_birth))
# 44 unique values in births

as.data.frame(table(deaths$year_death))
# 42 unique values in deaths

# since it's leap years, can have a tick mark every 4 years on x-axis, 
# but, that makes the chart hard to distinguish what year corresponds to what count.

plot_events <- ggplot(events, aes(x=year)) + 
  geom_histogram(breaks=seq(884,2024,by=4)) +
  scale_x_continuous(breaks=seq(884,2024,by=16), lim = c(884, 2024)) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
plot_events

# it makes sense the more frequent occurrences of events recorded are in recent history

plot_births <- ggplot(births, aes(x=year_birth)) + 
  geom_histogram(breaks=seq(1464,2024,by=4)) +
  scale_x_continuous(breaks=seq(1464,2024,by=16), lim = c(1464, 2024)) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
plot_births

# it makes sense for births there aren't any notable dates after 2004 (yet) 
# as humans usually take more than 4, 8, 12, 16, and 20 years
# to determine if someone's life is "exceptional".

plot_deaths <- ggplot(deaths, aes(x=year_death)) + 
  geom_histogram(breaks=seq(464,2024,by=4)) +
  scale_x_continuous(breaks=seq(464,2024,by=16), lim = c(464, 2024)) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
plot_deaths

# (modeling)
# it would be interesting to see:
# if there are any overlaps in the data (births and deaths)
# what countries the events recorded tend to represent
# if there is a sentiment analysis tool that can determine if events are positive and negative in sentiment
# (I imagine that would be difficult as some things listed are political in nature or have a social justice aspect.)


# communication
# export plots to .png (happens in code with plots)
# and I will figure that out in the future
