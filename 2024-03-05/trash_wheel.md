---
title: "Trashwheel"
output: html_notebook
---

## Load libraries

```{r}
library(tidyverse)
library(skimr)
library(ggplot2)
library(scales)
```

## Import the dataset
I put this in excel to reformat the date column, because I got frustrated with R so it's stored locally.

```{r}
trashwheel <- readr::read_csv('c:/R/projects/tidytuesday/trashwheel.csv')
```

```{r}
head(trashwheel, 5)
```
```{r}
skim(trashwheel)
```

How many trashwheels are there?

```{r}
unique(trashwheel$Name)
unique(trashwheel$ID)
```

What data is missing?

```{r}
which(is.na(trashwheel$PlasticBottles))
which(is.na(trashwheel$Polystyrene))
which(is.na(trashwheel$CigaretteButts))
```

It looks like just that one row of data has null values, so I'll need to take that into account when working with the numbers.

Also, all trash types are not collected on all trashwheels. 


## Transform/wrangle

```{r}
timeframes <- trashwheel %>%
  group_by(ID) %>%
  summarize(
    MinDate = min(Year, na.rm = TRUE),
    MaxDate = max(Year, na.rm = TRUE)
  )

head(timeframes)
```

The trashwheels have been collecting for different amounts of time, with Mister being in operation for the longest time.

## Visualization

What type of trash is collected the most?

```{r}
options(scipen=999999999)
par(mar=c(5, 5, 3, 1))
barplot(colSums(trashwheel[,9:15], na.rm = TRUE),
        cex.names=0.7,
        main = "Types of trash collected",
        ylab = "",
        xlab = "Type of object",
        yaxt = "n",
        ylim = c(0, max(colSums(trashwheel[,9:15], na.rm = TRUE)) * 1.2))
y_range <- range(0, max(colSums(trashwheel[,9:15], na.rm = TRUE)) * 1.2)
axis(2, las = 2, at=seq(0, y_range[2], by=2500000), 
     labels=scales::comma(seq(0, y_range[2], by=2500000)), 
     cex.axis=0.7)
mtext("Count", side=2, line=4)
```

Questions not answered: Do the different Trash Wheels collect different sets of trash? 
Are there times of the year when more or less trash is collected?
