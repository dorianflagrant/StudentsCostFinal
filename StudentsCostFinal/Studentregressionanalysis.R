library(tidyverse)

Unemployment <- read.csv("Data/Unemployment.csv", sep = ",")

Unemployment2 <- Unemployment %>%  filter(MEASURE == "VALUE")

Unemployment %>% 
  filter(MEASURE == "VALUE") %>% 
  View()

names(Unemployment)

Unemploymentsimple <- Unemployment[c(2,23)]

Unemploymentsimple2 <- Unemployment2[c(2,23)]

Unemployment2 %>% 
  filter(Value>0) %>%
    ggplot() +
    geom_bar(aes(x = reorder(Country, Value), y = Value, fill = Value), stat = "identity", position = position_stack(reverse = TRUE)) +  
    coord_flip() +
    theme() +
    xlab("OECD countries") +
    ylab("Unemployment rate") +
    labs(title = "Unemployment rate among tertiary education graduates", caption = "Source: OECD")

###Comparision with Cost by student

##Defining Coststudent

Coststudent <- read.csv("Data/Cost-by-student.csv",
                        sep = ",",header = TRUE,
                        dec = ".")

##Narrow down to tertiary education, all expenditure types, year 2015 [there was no data for 2016]

Coststudenttertiary <- Coststudent %>% filter(Education.level.and.programe.orientation == 
                                                "Total tertiary education (ISCED2011 levels 5 to 8)",
                                              Type.of.expenditure == "All expenditure types",
                                              Year == 2015)

##Differentiating ALL institutions / PUBLIC institutions / PRIVATE institutions

CoststudenttertiaryPUB <- Coststudenttertiary %>% filter(Institution.type == "Public educational institutions")

CoststudenttertiaryALL <- Coststudenttertiary %>% filter(Institution.type == "All public and private educational institutions")

CoststudenttertiaryPRIV <- Coststudenttertiary %>% filter(Institution.type == "All private educational institutions")

##Regression line Cost per student/unemployment

Unemployment2 %>% 
  select(Country, Value) %>% 
  left_join(CoststudenttertiaryALL, by = "Country") %>% 
  select(Country, Value.x, Value.y) %>% 
  ggplot(aes(x=Value.x, y=Value.y)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab("Unemployment rate") +
  ylab("Spending per student (USD)") +
  labs(title = "Unemployment decreases with greater government spending per student", 
       subtitle = "Unemployment as a function of cost per student in higher education", 
       caption = "Source: OECD")

###Comparision with expenditures

expenditures <- read.csv("Data/EAG_FIN_RATIO_CATEGORY_23012019145211397.csv",
                         sep = ",",header = TRUE,
                         dec = ".")

View(expenditures)
names(expenditures)

##Narrow it down to tertiary education expenditures

tertiaryexp <- filter(expenditures, 
                      Education.level.and.programe.orientation == "Total tertiary education (ISCED2011 levels 5 to 8)")

##Narrow it down to tertiary education expenditures in 2015

tertiaryexp <- filter(expenditures, 
                      Education.level.and.programe.orientation == "Total tertiary education (ISCED2011 levels 5 to 8)",
                      Year == 2015, Institution.type == "All sectors")

names(tertiaryexp)

##Keep only the country and the total amount dedicated to tertiary education expenditures

tertiaryexpsimple <- tertiaryexp[c(2,15)]

##Regression line for Unemployment / Government Expenditures on higher ed

Unemployment2 %>% 
  select(Country, Value) %>% 
  left_join(tertiaryexpsimple, by = "Country") %>% 
  select(Country, Value.x, Value.y) %>% 
  ggplot(aes(x=Value.x, y=Value.y)) +
  geom_point() +
  geom_smooth(method='lm') + 
  xlab("Unemployment rate") +
  ylab("Proportion of total government expenditures for higher education (per cent)") +
  labs(title = "Unemployment decreases as government higher education spending increases", 
       caption = "Source: OECD")