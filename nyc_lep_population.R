#Import and view the original csv file
lep_speakers <- Population_and_Languages_of_the_Limited_English_Proficient_LEP_Speakers_by_Community_District_2025
data(lep_speakers)
View(lep_speakers)

#Create a new data frame showing the total number of LEP speakers in each community district
total_lep_by_district <- lep_speakers %>%
  group_by(community_district_name) %>%
  summarise(total_lep_population = sum(lep_population, na.rm = TRUE)) %>%
  arrange(desc(total_lep_population))
View(total_lep_by_district)

#Filter the data frame created above to show only the community districts with the top 10 highest LEP populations
top_ten_lep <- top_n(total_lep_by_district, 10, total_lep_population)

#Create a horizontal bar graph plotting the data in top_ten_lep
ggplot(data = top_ten_lep) + 
  geom_col(aes(y = reorder(community_district_name, total_lep_population), x = total_lep_population, fill = -total_lep_population)) + 
  labs(x="Limited English Proficiency (LEP) Population", y="Community District") +
  theme(legend.position = "none")

#1 - Create a bar chart showing the top 5 community districts with the highest LEP population
#1A - Create a new data frame with the top 5 community districts by LEP population
top_5_districts <- lep_speakers %>%
  group_by(community_district_name) %>%
  summarise(total_lep = sum(lep_population, na.rm = TRUE)) %>%
  arrange(desc(total_lep)) %>%
  slice_head(n=5)

#1B - Create a horizontal bar graph
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

#2 - Visualize how LEP population distribution by language differs between Queens and Brooklyn
#2A - Create a new data frame with the top 5 LEP languages in Queens and Brooklyn
queens_brooklyn_lep_pop <- lep_speakers %>%
  #Filter only for languages in Queens or Brooklyn
  filter(borough %in% c("Queens", "Brooklyn")) %>%
  #Group results by borough and language
  group_by(borough, language) %>%
  #Sum the LEP population for each language by borough
  summarise(borough_lep_pop=sum(lep_population, na.rm = TRUE)) %>%
  #Get the top 5 languages with the highest LEP population by borough
  slice_max(borough_lep_pop, n=5) %>%
  arrange(borough, desc(borough_lep_pop))
View(queens_brooklyn_lep_pop)

library(forcats)
#2B - Create a bar chart of LEP population by language, and facet_wrap by borough
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
  #Create separate plots for each borough
  facet_wrap(~borough, scales = "free_x") +
  #Add labels to the plot
  labs(title = "Top 5 Limited English Proficiency Languages in Brooklyn and Queens", 
       x = "Language", 
       y = "LEP Population",
       caption = "Data from NYC Open Data courtesy of the Civic Engagement Commission (CEC)",
       fill = "Language") +
  #Customize the plot labels
  theme(plot.title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  #Rotate language labels 45 degrees for readability
  theme(axis.text.x = element_text(size = 10, angle = 45), axis.text.y = element_text(size = 10)) +
  #Remove the legend
  theme(legend.position = "none")

#3 - Create bar charts of the top 5 LEP languages for all five boroughs

#3A - Find the top 5 languages with the highest LEP population per borough
top_5_languages_per_borough <- lep_speakers %>%
  #Group results by borough and language
  group_by(borough, language) %>%
  #Sum the LEP population for each language by borough
  summarise(total_pop_per_language=sum(lep_population, na.rm = TRUE)) %>%
  #Get the top 5 languages with the highest LEP population by borough
  slice_max(total_pop_per_language, n=5) %>%
  arrange(borough, desc(total_pop_per_language))
View(top_5_languages_per_borough)

#3B - Make the plots
ggplot(top_5_languages_per_borough) +
  #Shorten the label for Chinese because it's running into the chart
  geom_col(aes(x=fct_reorder(language, total_pop_per_language), 
               y=total_pop_per_language, fill = language), show.legend = FALSE) +
  #Add labels to the columns to show the exact LEP Population
  geom_text(aes(x =fct_reorder(language,total_pop_per_language),
      y = total_pop_per_language,
      label = comma(total_pop_per_language)), vjust = -0.3, size = 3) +
  #Create separate plots for each borough
  facet_wrap(~borough, scales = "free_x") +
  scale_y_continuous(labels = comma) +
  #Select a color palette for the chart
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Top 5 Limited English Proficiency Languages (LEP) by Borough", x = "Language", y = "LEP Population", fill = "Language") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = "bold"), 
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
  expand_limits(y=max(top_5_languages_per_borough$total_pop_per_language)*1.1)
