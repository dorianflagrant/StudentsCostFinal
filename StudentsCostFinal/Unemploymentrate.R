### Working on unemployment rate in OECD countries

library(tidyverse)

Unemployment <- read.csv("Data/Unemployment.csv", sep = ",")

Unemployment2 <- Unemployment %>%  filter(MEASURE == "VALUE")

Unemployment %>% 
  filter(MEASURE == "VALUE") %>% 
  View()

names(Unemployment)

Unemploymentsimple <- Unemployment[c(2,23)]

Unemploymentsimple2 <- Unemployment2[c(2,23)]

### Graphs

Unemployment2 %>% 
  filter(Value>0) %>%
  #highlighting France in the bar charts
  mutate (highlight_flag = ifelse(Country == 'France', T, F)) %>% 
    ggplot() +
    geom_bar(aes(x = reorder(Country, Value), y = Value, fill = highlight_flag), 
             stat = "identity", 
             position = position_stack(reverse = TRUE)) +  
    scale_fill_manual(values = c('#00aaff', 'red')) +
    coord_flip() +
    theme() +
    xlab("OECD countries") +
    ylab("Unemployment rate") +
    labs(title = "Unemployment rate among tertiary education graduates", 
         caption = "Source: OECD") +
    theme(legend.position = 'none', 
          plot.title = element_text(size = 13, face = 'bold'))