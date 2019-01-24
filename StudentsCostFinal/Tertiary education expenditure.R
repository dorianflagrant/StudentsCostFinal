### Tertiary education expenditure in overall expenditure ###

install.packages("tidyverse")
library("tidyverse")

## Public expenditure on education as percentage of total government expenditure

expenditures <- read.csv("Data/EAG_FIN_RATIO_CATEGORY_23012019145211397.csv",
                         sep = ",",header = TRUE,
                         dec = ".")

## Narrow it down to tertiary education expenditures

tertiaryexp <- filter(expenditures, 
                      Education.level.and.programe.orientation == "Total tertiary education (ISCED2011 levels 5 to 8)")

## Narrow it down to tertiary education expenditures in 2015

tertiaryexp <- filter(expenditures, 
Education.level.and.programe.orientation == "Total tertiary education (ISCED2011 levels 5 to 8)",
Year == 2015, Institution.type == "All sectors")

names(tertiaryexp)

## Keep only the country and the total amount dedicated to tertiary education expenditures

tertiaryexpsimple <- tertiaryexp[c(2,15)]

## Graph

tertiaryexpsimple %>%
  filter(Value>0) %>%
  #highlighting France in the bar charts
  mutate (highlight_flag = ifelse(Country == 'France', T, F)) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(Country, Value), y = Value, fill = highlight_flag), 
           stat = "identity", 
           position = position_stack(reverse = TRUE)) +  
  scale_fill_manual(values = c('#00aaff', 'red')) +
  coord_flip() +
  ggtitle("Tertiary education expenditure in OECD countries") +
  xlab("OECD countries") +
  ylab("% tertiary education expenditure / Total public expenditure") + 
  labs(fill = "%",caption = "Source: OECD") +
  theme(legend.position = 'none', 
        plot.title = element_text(size = 15, face = 'bold'))