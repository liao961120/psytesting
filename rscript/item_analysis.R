library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(plotly)
library(cowplot)
library(scales) # for ggplot color
psy_test <- read_rds("./data/psy_test_analy.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")

AT_item <- var_info$vars_en[var_info$construct_en=="AT"]
    AT_item <- AT_item[!is.na(AT_item)]
CF_item <- var_info$vars_en[var_info$construct_en=="CF"]
    CF_item <- CF_item[!is.na(CF_item)]
HD_item <- var_info$vars_en[var_info$construct_en=="HD"]
    HD_item <- HD_item[!is.na(HD_item)]
RF_item <- var_info$vars_en[var_info$construct_en=="RF"]
    RF_item <- RF_item[!is.na(RF_item)]
Fk_item <- var_info$vars_en[var_info$construct_en=="F"]
    Fk_item <- Fk_item[!is.na(Fk_item)]

# Filtering
## Grade: first -> Fk: gradient
psy_test_f <- psy_test %>%
    filter(grade != "first") %>%
    filter(Fk <= 18) 

# Cronbach's alpha--------------
All_data <- psy_test_f %>%
    select(AT_item,CF_item,HD_item,RF_item)
AT_data <- psy_test_f %>%
    select(AT_item)
CF_data <- psy_test_f %>%
    select(CF_item)
HD_data <- psy_test_f %>%
    select(HD_item)
RF_data <- psy_test_f %>%
    select(RF_item)
Fk_data <- psy_test_f %>%
    select(Fk_item)

psych::alpha(All_data)$total$std.alpha
psych::alpha(AT_data)$total$std.alpha
psych::alpha(CF_data)$total$std.alpha
psych::alpha(HD_data)$total$std.alpha
psych::alpha(RF_data)$total$std.alpha
psych::alpha(Fk_data)$total$std.alpha

# summ <- psy_test %>% 
#     group_by(dept) %>% 
#     summarise(count=n(),
#               AT=mean(AT),
#               CF=mean(CF),
#               HD=mean(HD),
#               RF=mean(RF),
#               Fk=mean(Fk),
#               score=mean(total)) %>%
#     filter(count>=5)


