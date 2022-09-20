
schools <- read.csv("nys_schools.csv", stringsAsFactors = FALSE)
counties <- read.csv("nys_acs.csv", stringsAsFactors = FALSE)
#For each poverty_level, total enrollment
merged_data %>%
  group_by(poverty_level) %>%
  summarize(output = sum(total_enroll)) %>%
  ggplot() +
  geom_col(aes(x = poverty_level, y = output)) +
  labs(x = 'poverty_level', y = 'total_enrollment', title = 'total enrollment of counties in different poverty levels')

#percent of population in poverty for each country
merged_data %>%
  group_by(county_name, poverty_level) %>%
  summarize(output=sum(total_enroll)) %>%
  ggplot()+
  geom_col(aes(x = county_name, y = output, group = poverty_level, fill = poverty_level), position = 'dodge')