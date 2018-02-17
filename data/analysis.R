library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
psy_test <- read_rds("./data/psy_test_analy.rds")

summ <- psy_test %>% 
    group_by(dept) %>% 
    summarise(count=n(),
              AT=mean(AT),
              CF=mean(CF),
              HD=mean(HD),
              RF=mean(RF),
              Fk=mean(Fk),
              Score=mean(total)) %>%
    filter(count>=5)

g <- ggplot(data = psy_test, aes(x = reorder(dept)))+
    geom_bar()
ggplotly(g)