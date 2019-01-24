### Enrolment Rate of French Students Comparing with Other OECD Countries ###

## Step 1. Open the table of enrolment rate in OECD countries

library(readr)
library(readxl)
library(tidyverse)
Table_of_enrolment_rate_OECD <- read_csv("./Data/Table of enrolment rate_OECD.csv")
View(Table_of_enrolment_rate_OECD)

## Step 2. We choose age group 18 among age group 17-19 to make the comparison as it is more common and easy to compare
 
   # extract the age group 18 from the dataset and make a new one named "OECD_Comparision"
   Table_of_enrolment_rate_OECD %>% 
   filter(SUBJECT == "AGE_18") -> OECD_Comparison  
   View(OECD_Comparison) 

## Step 3. Make a bar chart of the enrollment rate in OECD countries
   
install.packages('hrbrthemes')
library(hrbrthemes)

OECD_Comparison %>% 
  group_by(LOCATION) %>% 
  summarise(mean_value = mean(Value)) %>% 
  filter(mean_value > 0) %>%
  #highlighting France in the bar charts
  mutate (highlight_flag = ifelse(LOCATION == 'FRA', T, F)) %>% 
  ggplot(aes(x = reorder(LOCATION,mean_value), y = mean_value)) +
  geom_bar(stat = "identity", width = 0.8, aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#00aaff', 'red')) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs (title = "Average Enrollment Rate in OECD Countries",
        subtitle = "The Percentage of French Students Compared to other OECD Countries", 
        caption = "Source:OECD Stat") +
  xlab("OECD countries") +
  ylab("Average enrollment rate") +
  theme(legend.position = 'none', 
        plot.title = element_text(size = 18, face = 'bold'))