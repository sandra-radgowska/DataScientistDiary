# Source (https://www.kaggle.com/sharoncherian/corona-virus-eda)


# 1. Loading data and packages------------------------------------------------------------------------------------------------------------------
library(coronavirus)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(maps)
library(gridExtra)

# 2. First glance ------------------------------------------------------------------------------------------------------------------
summary(coronavirus_data)
str(coronavirus_data)

coronavirus_data <- coronavirus::coronavirus %>%
  mutate(Province.State = ifelse(Province.State == "", "Unknown", Province.State))

# Missing values check
coronavirus_data$Province.State <- as.factor(coronavirus_data$Province.State)
coronavirus_data$Country.Region <- as.factor(coronavirus_data$Country.Region)
coronavirus_data$type <- as.factor(coronavirus_data$type)
coronavirus_data$cases <- as.numeric(coronavirus_data$cases)

str(coronavirus_data)
summary(coronavirus_data)

sapply(coronavirus_data, function(x) sum(is.na(x))) # no NAs
sapply(coronavirus_data, function(x) length(unique(x))) # unique values

# 3. Data transformation -----------------------------------------------------------------------------------------------------------

# Ujemne wartosci w cases to blad, zmiana na 0 - czy dobrze?
coronavirus_data <- coronavirus_data %>%
  mutate(cases = ifelse(cases < 0, 0, cases))

coronavirus_spread <- coronavirus_data %>%
  spread(type, cases)

coronavirus_spread <- coronavirus_spread %>%
  separate(date, into = c("Year", "Month", "Day"), sep = "-")

coronavirus_spread <- coronavirus_spread %>%
  mutate(
    confirmed = ifelse(is.na(confirmed), 0, confirmed),
    death = ifelse(is.na(death), 0, death),
    recovered = ifelse(is.na(recovered), 0, recovered)
  )

totalcases2 <- coronavirus_data %>%
  group_by(Province.State, Country.Region) %>%
  summarise(Total_cases = sum(cases))

coronavirus_spread <- coronavirus_spread %>%
  left_join(totalcases2, by = c("Province.State", "Country.Region"))

coronavirus_spread <- coronavirus_spread %>%
  mutate(
    Perc_confirmed = confirmed / Total_cases * 100,
    Perc_death = death / Total_cases * 100,
    Perc_recovered = recovered / Total_cases * 100
  )

coronavirus_spread <- coronavirus_spread %>%
  mutate(
    Perc_confirmed = round(Perc_confirmed, 2),
    Perc_death = round(Perc_death, 2),
    Perc_recovered = round(Perc_recovered, 2)
  )

# Last update
lastupdate <- coronavirus_data %>%
  group_by(Province.State, Country.Region, type) %>%
  filter(date == max(date))


lastupdate_spread <- lastupdate %>%
  spread(type, cases)

lastupdate_spread <- lastupdate_spread %>%
  separate(date, into = c("Year", "Month", "Day"), sep = "-")

lastupdate_spread <- lastupdate_spread %>%
  mutate(
    confirmed = ifelse(is.na(confirmed), 0, confirmed),
    death = ifelse(is.na(death), 0, death),
    recovered = ifelse(is.na(recovered), 0, recovered)
  )

totalcases <- lastupdate %>%
  group_by(Province.State, Country.Region) %>%
  summarise(Total_cases = sum(cases))

lastupdate_spread <- lastupdate_spread %>%
  left_join(totalcases, by = c("Province.State", "Country.Region"))

lastupdate_spread <- lastupdate_spread %>%
  mutate(
    Perc_confirmed = confirmed / Total_cases * 100,
    Perc_death = death / Total_cases * 100,
    Perc_recovered = recovered / Total_cases * 100
  )

lastupdate_spread <- lastupdate_spread %>%
  mutate(
    Perc_confirmed = round(Perc_confirmed, 2),
    Perc_death = round(Perc_death, 2),
    Perc_recovered = round(Perc_recovered, 2)
  )


# 4. Exploratory Visualizations ----------------------------------------------------------------------------------------------------------------

# Cases per country
coronavirus_data %>%
  filter(Country.Region == "Mainland China") %>%
  ggplot(., aes(x = date, y = cases)) +
  geom_histogram(stat = "identity") +
  facet_grid(type ~ ., scales = "free") +
  labs(
    title = "Corona Virus cases per country among time",
    x = "Date", y = "Number of cases"
  )

# Percentage of confirmed cases among time per country
coronavirus_spread %>%
  filter(Country.Region == "Mainland China") %>%
  ggplot(., aes(x = Day, y = Perc_confirmed)) +
  geom_histogram(stat = "identity") +
  facet_grid(Month ~ ., scales = "free") +
  labs(
    title = "Corona Virus confirmed cases per country among time",
    x = "Day of month", y = "Percentage of cases in total"
  )


# Percentage of death cases among time per country
coronavirus_spread %>%
  filter(Country.Region == "Mainland China") %>%
  ggplot(., aes(x = Day, y = Perc_death)) +
  geom_histogram(stat = "identity") +
  facet_grid(Month ~ ., scales = "free") +
  labs(
    title = "Corona Virus death cases per country among time",
    x = "Day of month", y = "Percentage of cases in total"
  )



# Percentage of recovered cases among time per country
coronavirus_spread %>%
  filter(Country.Region == "Mainland China") %>%
  ggplot(., aes(x = Day, y = Perc_recovered)) +
  geom_histogram(stat = "identity") +
  facet_grid(Month ~ ., scales = "free") +
  labs(
    title = "Corona Virus recovered cases per country among time",
    x = "Day of month", y = "Percentage of cases in total"
  )
# Last update
# China vs rest of world - comparison among time
coronavirus_data %>%
  ggplot(., aes(x = date, y = cases, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(China_vs_RestofWorld ~ ., scales = "free") +
  labs(
    title = "Corona Virus cases - China vs Rest of World",
    x = "Date", y = "Number of cases"
  )


lastupdate_spread %>%
  filter(China_vs_RestofWorld == "Rest of world") %>%
  ggplot(., aes(x = reorder(Country.Region, +Total_cases), y = Total_cases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Corona Virus cases outside China",
    x = "Country/Region", y = "Number of cases"
  )


# GGMAP
world_map <- map_data("world")

# Creat a base plot with gpplot2
p <- ggplot() +
  coord_fixed() +
  xlab("") +
  ylab("")

# Add map to base plot
base_world_messy <- p + geom_polygon(
  data = world_map, aes(x = long, y = lat, group = group),
  colour = "black", fill = "white"
)

base_world_messy

# Strip the map down so it looks super clean (and beautiful!)
cleanup <-
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "lightblue", colour = "white"),
    axis.line = element_line(colour = "lightblue"), legend.position = "none",
    axis.ticks = element_blank(), axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

base_world <- base_world_messy + cleanup

base_world

map_data <-
  base_world +
  geom_point(
    data = coronavirus_spread,
    aes(x = Long, y = Lat), colour = "Purple",
    fill = "Pink", pch = 21, size = 5, alpha = I(0.7)
  )

map_data

# Add data points to map with value affecting size

# CONFIRMED CASES MAP
confirmed_map <-
  base_world +
  geom_point(
    data = coronavirus_spread,
    aes(x = Long, y = Lat, size = Perc_confirmed), colour = "Purple",
    fill = "Pink", pch = 21, alpha = I(0.7)
  ) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(coronavirus_spread$Province.State)), " province states)"))

confirmed_map

# DEATH CASES MAP
death_map <-
  base_world +
  geom_point(
    data = coronavirus_spread,
    aes(x = Long, y = Lat, size = Perc_death), colour = "Purple",
    fill = "Pink", pch = 21, alpha = I(0.7)
  ) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(coronavirus_spread$Province.State)), " province states)"))

death_map

# RECOVERED CASES MAP
recovered_map <-
  base_world +
  geom_point(
    data = coronavirus_spread,
    aes(x = Long, y = Lat, size = Perc_recovered), colour = "Purple",
    fill = "Pink", pch = 21, alpha = I(0.7)
  ) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(coronavirus_spread$Province.State)), " province states)"))

recovered_map


# 5. Material for the article - storytelling with data
# Example 1

# Before
# Adding continent column
lastupdate_spread <- lastupdate_spread %>%
  mutate(Continent = ifelse(Country.Region %in% c("Mainland China", "Japan", "Malaysia", "Taiwan", "Singapore", "Russia", "Macau", "India", "Thailand", "Philippines", "South Korea", "Hong Kong", "Vietnam", "Sri Lanka", "United Arab Emirates", "Cambodia", "Nepal"), "Asia", ifelse(Country.Region %in% c("UK", "France", "Spain", "Germany", "Finland", "Sweden", "Italy", "Belgium"), "Europe", ifelse(Country.Region == "Egipt", "Africa", ifelse(Country.Region == "Australia", "Australia", ifelse(Country.Region %in% c("US", "Canada"), "North America", ifelse(Country.Region == "Egypt", "Africa", "Other")))))))


lastupdate_spread %>%
  filter(Country.Region != "Others" & Country.Region != "Mainland China") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases, fill = Continent)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 25)) +
  labs(
    title = "Corona Virus cases outside China",
    x = "Country/Region", y = "Number of cases"
  )




# After:
library(forcats)

lastupdate_spread$Country.Region <- as.factor(lastupdate_spread$Country.Region)
lastupdate_spread <- as.data.frame(lastupdate_spread)
str(lastupdate_spread)

# First change (no bar colours)
lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 25)) +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases")

# Second change (theme_bw) - could be better
lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_bw()

# Better
lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_minimal()

# Best one
lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic()

# Storytelling - Asia
lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  mutate(Asia_flag = ifelse(Continent == "Asia", T, F)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity", aes(fill = Asia_flag)) +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic() +
  scale_fill_manual(values = c("#595959", "blue")) +
  theme(legend.position = "none")

# Storytelling - Europe
europe <- lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  mutate(Europe_flag = ifelse(Continent == "Europe", T, F)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity", aes(fill = Europe_flag)) +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic() +
  scale_fill_manual(values = c("#595959", "blue")) +
  theme(legend.position = "none")


# Storytelling - North America
america <- lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  mutate(America_flag = ifelse(Continent == "North America", T, F)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity", aes(fill = America_flag)) +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic() +
  scale_fill_manual(values = c("#595959", "blue")) +
  theme(legend.position = "none")

# Storytelling - Australia
australia <- lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  mutate(Australia_flag = ifelse(Continent == "Australia", T, F)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity", aes(fill = Australia_flag)) +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic() +
  scale_fill_manual(values = c("#595959", "blue")) +
  theme(legend.position = "none")

# Storytelling - Africa
africa <- lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  mutate(Africa_flag = ifelse(Continent == "Africa", T, F)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity", aes(fill = Africa_flag)) +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic() +
  scale_fill_manual(values = c("#595959", "blue")) +
  theme(legend.position = "none")

grid.arrange(europe, america, australia, africa, nrow = 2, ncol = 2)


# Final version - Adding annotations
lastupdate_spread %>%
  filter(Country.Region != "Mainland China" & Continent != "Other") %>%
  mutate(Flag = ifelse(Country.Region %in% c("Japan", "US", "Australia", "UK", "Egypt"), T, F)) %>%
  mutate(Country.Region = reorder(Country.Region, Total_cases, sum)) %>%
  ggplot(., aes(x = Country.Region, y = Total_cases)) +
  geom_bar(stat = "identity", aes(fill = Flag)) +
  coord_flip() +
  labs(title = "Corona Virus cases outside China", subtitle = "Data as reported by February 16", x = "Country/Region", y = "Number of cases") +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic() +
  scale_fill_manual(values = c("#595959", "blue")) +
  annotate("text",
    x = c(20, 10),
    y = c(22, 22),
    label = c("Countries with most cases: \n Japan for Asia, \n US for N.America, \n UK for EU, \n Egypt for Africa and \n Australia for others.", "Most of affected countries \n are located in Asia."),
    family = "", fontface = 3, size = 4
  ) +
  theme(legend.position = "none")
