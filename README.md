---
title: "NYC LEP Population"
author: "Olivia"
date: "2025-10-22"
output:
  pdf_document: default
  html_document: default
---
# Performing an analysis of NYC LEP populations

Before beginning this project, I installed and loaded the packages I would need to carry out the analysis and visualization.
```{r Creating my environment, message=FALSE}
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("forcats", repos = "http://cran.us.r-project.org")
install.packages("scales", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
```

## Part 1 - Identifying the community districts with the top 5 highest LEP populations

### Importing the dataset
The first step of this project was to import the csv file downloaded from NYC Open Data. The dataset is titled "Population and Languages of the Limited English Proficient LEP Speaker by Community District" and was made available by the NYC Civic Engagement Commission (CEC). 
```{r Importing the csv file, message=FALSE}
lep_speakers <- read.csv("Population_and_Languages_of_the_Limited_English_Proficient__LEP__Speakers_by_Community_District_2025.csv")
View(lep_speakers)
```

### Creating a new dataframe to identify the 5 community districts with the highest LEP population
After importing the dataset, I created a new dataframe that would show the community districts with the top 5 highest LEP populations, ordered by total LEP population from highest to lowest.
```{r Creating top 5 districts dataframe, message=FALSE}
top_5_districts <- lep_speakers %>%
  group_by(community_district_name) %>%
  summarise(total_lep = sum(lep_population, na.rm = TRUE)) %>%
  arrange(desc(total_lep)) %>%
  slice_head(n=5)
View(top_5_districts)
```

### Creating a horizontal bar graph
Next, I wanted to plot these results in a horizontal bar graph.
```{r Creating first visualization, message=FALSE}
ggplot(data = top_5_districts) + 
  geom_col(aes(y = reorder(community_district_name, total_lep), x = total_lep, fill = total_lep)) +
  labs(title="Top 5 Community Districts by LEP Population", 
       x="LEP Population", 
       y="Community District Name",
       fill = NULL,
       caption = "Data from NYC Open Data courtesy of the Civic Engagement Commission (CEC)") +
  geom_text(aes(y = reorder(community_district_name, total_lep), x = total_lep,
                label = comma(total_lep)), 
            hjust = 1.1, color = "white", size = 4) +
  scale_fill_continuous(trans = "reverse") +
  theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'), 
        plot.margin = margin(t = 10, r = 30, b = 10, l = 10)) +
  theme(plot.title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank())
```

This code generates a horizontal bar chart titles "Top 5 Community Districts by LEP Population," with Flushing, Bay Terrace as the community district with the highest population of LEP individuals.

## Part 2 - Visualizing how LEP population distribution by language differs between Queens and Brooklyn
Queens and Brooklyn are the boroughs with the top 2 highest LEP populations. I wanted to get more information about which languages are responsible for making up the bulk of the LEP population for each borough. I decided to find the top 5 langugaes in each borough by LEP population.

### Creating a new data frame with the top 5 LEP languages in Queens and Brooklyn
```{r Creating top 5 Queens and Brooklyn LEP languages dataframe, message=FALSE}
queens_brooklyn_lep_pop <- lep_speakers %>%
  
  #Filter only for languages in Queens or Brooklyn
  filter(borough %in% c("Queens", "Brooklyn")) %>%
  
  #Group results by borough and language
  group_by(borough, language) %>%
  
  #Sum the LEP population for each language by borough
  summarise(borough_lep_pop=sum(lep_population, na.rm = TRUE)) %>%
  
  #Get the top 5 languages with the highest LEP population by borough
  slice_max(borough_lep_pop, n=5) %>%
  
  #Arrange the results by borough and by language LEP population, from highest to lowest.
  arrange(borough, desc(borough_lep_pop))

View(queens_brooklyn_lep_pop)
```

The new dataframe queens_brooklyn_lep_pop is created from the dataframe lep_speakers made in Part 1. It is filtered down to only return the top 5 languages from Queens and the top 5 languages from Brooklyn with the highest LEP populations.

### Creating a bar chart of LEP population by language, and facet_wrap by borough
Finally, I wanted to plot this dataframe in a bar chart as well, but separate each borough into its own plot. I achieved this by using the facet_wrap function in ggplot2.

```{r Creating second visualization, message=FALSE}
ggplot(data = queens_brooklyn_lep_pop) +
  #Shorten the label for Chinese because it's running into the chart
  geom_col(aes(x=fct_recode(language, Chinese = "Chinese (incl. Mandarin, Cantonese)"), y=borough_lep_pop, fill = language)) +
  #Add labels to the columns to show the exact LEP Population
  geom_text(
    aes(
      x = fct_recode(language, Chinese = "Chinese (incl. Mandarin, Cantonese)"),
      y = borough_lep_pop,
      label = comma(borough_lep_pop)), vjust = -0.5) +
  #Select a color palette for the chart
  scale_fill_brewer(palette = "Dark2") +
  #Create separate plots for each borough, removing the x-axis text label under any empty columns
  facet_wrap(~borough, scales = "free_x") +
  #Add labels to the plot
  labs(title = "Top 5 Limited English Proficiency Languages in Brooklyn and Queens", 
       x = "Language", 
       y = "LEP Population",
       caption = "Data from NYC Open Data courtesy of the Civic Engagement Commission (CEC)",
       fill = "Language") +
  #Customize the look of theplot labels
  theme(plot.title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  #Rotate language labels 45 degrees for readability
  theme(axis.text.x = element_text(size = 10, angle = 45), axis.text.y = element_text(size = 10)) +
  #Remove the legend
  theme(legend.position = "none")
```