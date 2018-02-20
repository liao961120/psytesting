library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(scales) # for ggplot color
library(plotly)

psy_test_f <- read_rds("./data/psy_test_filtered.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")


blank_theme <- theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    )

gender <- psy_test_f %>%
    count(gender)
    # mutate(gender= fct_recode(gender,
    #     "女"="female",
    #     "男"="male",
    #     "其他"="other"))

hue <- hue_pal()(3)

pie <- ggplot(gender, aes(x="", y=n, fill=gender))+
    geom_bar(width = 1, stat="identity")+
    scale_y_continuous(breaks = NULL)+
    coord_polar("y", start=0) +
    scale_fill_manual(values=c(hue[1], hue[3], "black"))+
    blank_theme+
    geom_text(aes(y = c(140, 43, 218), # label position adjustment
              label = percent(n/sum(n))), size=3, vjust="outward",hjust="outward")+
    labs(title="Gender Percentage", fill="Gender", subtitle =paste(" Samples: ",sum(gender$n), sep=""))


