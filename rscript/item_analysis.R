library(readr)
library(dplyr)
library(psych)
library(ggplot2)

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

all_alpha <- psych::alpha(All_data)$total$std.alpha
AT_alpha <- psych::alpha(AT_data)$total$std.alpha
CF_alpha <- psych::alpha(CF_data)$total$std.alpha
HD_alpha <- psych::alpha(HD_data)$total$std.alpha
RF_alpha <- psych::alpha(RF_data)$total$std.alpha
Fk_alpha <- psych::alpha(Fk_data)$total$std.alpha

## delete alpha--------------
item_stats <- var_info[1:62, -c(4,5,6)] %>% filter(construct_en != "F")
item_stats <- cbind(item_stats,c(rep(0, nrow(item_stats))),c(rep(0, nrow(item_stats))) )
colnames(item_stats) <- c("item","construct","code","alpha_ori","alpha")

for (i in item_stats$item){
    if (item_stats[item_stats$item==i, "construct"] == "AT"){
        index <- which(colnames(AT_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- AT_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(AT_data[,-index])$total$std.alpha
    } 
    else if (item_stats[item_stats$item==i, "construct"] == "CF"){
        index <- which(colnames(CF_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- CF_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(CF_data[,-index])$total$std.alpha
    } 
    else if (item_stats[item_stats$item==i, "construct"] == "HD"){
        index <- which(colnames(HD_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- HD_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(HD_data[,-index])$total$std.alpha
    } 
    else if (item_stats[item_stats$item==i, "construct"] == "RF"){
        index <- which(colnames(RF_data)==i)
        item_stats[item_stats$item==i, "alpha_ori"] <- RF_alpha
        item_stats[item_stats$item==i, "alpha"] <- psych::alpha(RF_data[,-index])$total$std.alpha
    }
}


