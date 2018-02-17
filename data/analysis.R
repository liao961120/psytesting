library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(psych)
psy_test <- read_rds("./data/psy_test_analy.rds")
var_info <- read_rds("./data/item_construct.rds")

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
## Fk: gradient-------------------------
Fk_cutoff <- tibble(i=15:30,
                    AT=rep(0,length(15:30)),
                    CF=rep(0,length(15:30)),
                    HD=rep(0,length(15:30)),
                    RF=rep(0,length(15:30)),
                    total=rep(0,length(15:30))
                    )
for (i in 15:30) {
    psy_test1 <- psy_test %>%
        filter(grade != "first") %>%
        filter(Fk <= i)
    Fk_cutoff$AT[i-14] <- cor(psy_test1$Fk,psy_test1$AT)
    Fk_cutoff$CF[i-14] <- cor(psy_test1$Fk,psy_test1$CF)
    Fk_cutoff$HD[i-14] <- cor(psy_test1$Fk,psy_test1$HD)
    Fk_cutoff$RF[i-14] <- cor(psy_test1$Fk,psy_test1$RF)
    Fk_cutoff$total[i-14] <- cor(psy_test1$Fk,psy_test1$total)
}

pl_cutoff <- ggplot(Fk_cutoff)+
    geom_line(mapping = aes(x=i,y=AT,color="AT"),size=1)+
    geom_line(mapping = aes(x=i,y=CF,color="CF"),size=1)+
    geom_line(mapping = aes(x=i,y=HD,color="HD"),size=1)+
    geom_line(mapping = aes(x=i,y=RF,color="RF"),size=1)+
    geom_line(mapping = aes(x=i,y=total,color="total"),size=1)+
    scale_y_continuous(breaks = seq(0.05,0.4,by=0.05))+
    scale_x_reverse(breaks=15:30)+
    labs(x="Cutoff", y="Corr. with Fk", color="Construct")
ggplotly(pl_cutoff)

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


