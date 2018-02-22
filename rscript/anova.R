library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(scales)
library(knitr)

psy_test_f <- read_rds("./data/psy_test_filtered.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")

psy_test_f <- psy_test_f %>%
    mutate(grade = fct_recode(grade,
            "Second" = "second",
            "Third" = "third",
            "Forth" = "forth",
            "Graduated" = "graduated",
            "Others" = "NoDegree",
            "Others" = "suspend")) %>%
    mutate(grade = grade %>% fct_infreq())

#### ploting theme-----------------
theme <- theme(
    plot.subtitle = element_text(size = 9, face = "bold"),
    plot.caption = element_text(size = 7, face = "plain", vjust = 0),
    axis.text.x = element_text(size = 7, face = "plain"),
    axis.text.y = element_text(size = 7, face = "plain"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0, size = 14, face = "bold"))


## Gender t-test-------------
gender_t_data <- psy_test_f %>% filter(gender != "other")

AT_t <- t.test(gender_t_data$AT ~ gender_t_data$gender)[c(1:3,5)]
AT_t <- c(unlist(AT_t[1:3]), unlist(AT_t[4]))

CF_t <- t.test(gender_t_data$CF ~ gender_t_data$gender)[c(1:3,5)]
CF_t <- c(unlist(CF_t[1:3]), unlist(CF_t[4]))

RF_t <- t.test(gender_t_data$RF ~ gender_t_data$gender)[c(1:3,5)]
RF_t <- c(unlist(RF_t[1:3]), unlist(RF_t[4]))

HD_t <- t.test(gender_t_data$HD ~ gender_t_data$gender)[c(1:3,5)]
HD_t <- c(unlist(HD_t[1:3]), unlist(HD_t[4]))

total_t <- t.test(gender_t_data$total ~ gender_t_data$gender)[c(1:3,5)]
total_t <- c(unlist(total_t[1:3]), unlist(total_t[4]))

gender_ttest_df <- as.data.frame(cbind(AT_t, CF_t, HD_t, RF_t, total_t)) %>% 
    mutate(var = c("t", "df", "p-value", "Female mean", "Male mean")) %>% 
    select(var, everything()) %>%
    setNames(c("Var", "AT", "CF", "HD", "RF", "Total"))

## ntu vs non-ntu t-test-------------------
ntu_t_data <- psy_test_f %>% 
    mutate(college = fct_lump(college, n=1))

AT_t <- t.test(ntu_t_data$AT ~ ntu_t_data$college)[c(1:3,5)]
AT_t <- c(unlist(AT_t[1:3]), unlist(AT_t[4]))

CF_t <- t.test(ntu_t_data$CF ~ ntu_t_data$college)[c(1:3,5)]
CF_t <- c(unlist(CF_t[1:3]), unlist(CF_t[4]))

RF_t <- t.test(ntu_t_data$RF ~ ntu_t_data$college)[c(1:3,5)]
RF_t <- c(unlist(RF_t[1:3]), unlist(RF_t[4]))

HD_t <- t.test(ntu_t_data$HD ~ ntu_t_data$college)[c(1:3,5)]
HD_t <- c(unlist(HD_t[1:3]), unlist(HD_t[4]))

total_t <- t.test(ntu_t_data$total ~ ntu_t_data$college)[c(1:3,5)]
total_t <- c(unlist(total_t[1:3]), unlist(total_t[4]))

ntu_ttest_df <- as.data.frame(cbind(AT_t, CF_t, HD_t, RF_t, total_t)) %>% 
    mutate(var = c("t", "df", "p-value", "NTU mean", "non-NTU mean")) %>% 
    select(var, everything()) %>%
    setNames(c("Var", "AT", "CF", "HD", "RF", "Total")) 



### AT_gender box plot (only AT sig.)----------------
AT_gender <- gender_t_data %>%
    mutate(gender = fct_recode(gender,
                "Female" = "female",
                "Male" = "male")) %>%
    mutate(gender = gender %>% fct_infreq()) %>%
    ggplot(aes(x = gender, y = AT/sum(var_info$construct_en=="AT", na.rm = T), fill=gender)) + # scale y as avg score
    geom_boxplot(show.legend = F) +
    theme +
    theme (
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.ticks.y = element_blank()) +
    scale_y_continuous(limits=c(1,5), breaks = 1:5) +
    labs(title="AT Comparison", subtitle="Male vs. Female", x="", y="AT Avg. Score", fill="Grade")+
    coord_flip()


### ntu box plot (AT CF sig.)-------------
#### box plot data-------------------
AT_data <- ntu_t_data %>%
    mutate(college = fct_recode(college,
            "其它學校" = "Other")) %>%
    mutate(college = college %>% fct_infreq()) %>%
    filter(college=="臺灣大學"|college=="其它學校") %>%
    mutate(construct="AT") %>%
    select(construct, college, AT) %>%
    rename(value=AT) %>%
    mutate(value = value/sum(var_info$construct_en=="AT", na.rm = T))

CF_data <- ntu_t_data %>%
    mutate(college = fct_recode(college,
            "其它學校" = "Other")) %>%
    mutate(college = college %>% fct_infreq()) %>%
    filter(college=="臺灣大學"|college=="其它學校") %>%
    mutate(construct="CF") %>%
    select(construct, college, CF) %>%
    rename(value=CF) %>%
    mutate(value = value/sum(var_info$construct_en=="CF", na.rm = T))

HD_data <- ntu_t_data %>%
    mutate(college = fct_recode(college,
                    "其它學校" = "Other")) %>%
    mutate(college = college %>% fct_infreq()) %>%
    filter(college=="臺灣大學"|college=="其它學校") %>%
    mutate(construct="HD") %>%
    select(construct, college, HD) %>%
    rename(value=HD) %>%
    mutate(value = value/sum(var_info$construct_en=="HD", na.rm = T))

RF_data <- ntu_t_data %>%
    mutate(college = fct_recode(college,
                    "其它學校" = "Other")) %>%
    mutate(college = college %>% fct_infreq()) %>%
    filter(college=="臺灣大學"|college=="其它學校") %>%
    mutate(construct="RF") %>%
    select(construct, college, RF) %>%
    rename(value=RF) %>%
    mutate(value = value/sum(var_info$construct_en=="RF", na.rm = T))

total_data <- ntu_t_data %>%
    mutate(college = fct_recode(college,
                    "其它學校" = "Other")) %>%
    mutate(college = college %>% fct_infreq()) %>%
    filter(college=="臺灣大學"|college=="其它學校") %>%
    mutate(construct="total") %>%
    select(construct, college, total) %>%
    rename(value=total) %>%
    mutate(value = value/56) # 56 items in total (F not included)

box_pl_data <- rbind(AT_data, CF_data, HD_data, RF_data, total_data)
    
### ntu box plot------------------------

box_pl_data <- box_pl_data %>% 
    mutate(construct = as_factor(construct),
           college = as_factor(college)) %>%
    mutate(college = fct_reorder(college, value))

ggplot(box_pl_data, aes(x=construct, y=value)) + 
    geom_boxplot(aes(fill=college)) +
    scale_fill_manual(values = grey.colors(2)) + # change boxplot color
    theme +
    theme (
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.ticks.y = element_blank()) +
    scale_y_continuous(limits=c(1,5), breaks = 1:5) +
    labs(title="Score Comparison",subtitle="NTU vs. non-NTU",y="Avg Score", x="Construct") +
    coord_flip()


## Grade=======================
### ANOVA--------------------
anova_data <- psy_test_f

AT_lm <- lm(anova_data$AT ~ anova_data$grade)
RF_lm <- lm(anova_data$RF ~ anova_data$grade)
AT_table <- anova(AT_lm) %>%
    `rownames<-`(c("Grade", "Residuals"))
RF_table <- anova(RF_lm) %>%
    `rownames<-`(c("Grade", "Residuals"))

### Tukey test-------------------

a1 <- aov(anova_data$AT ~ anova_data$grade)
AT_post <- TukeyHSD(x=a1, 'anova_data$grade', conf.level=0.95)
AT_post <- as.data.frame(AT_post[[1]])[,c(1,4)]
AT_post <- AT_post %>%
    rename(Diff=diff, "p-value"=`p adj`) %>%
    mutate(var=rownames(AT_post))%>%
    mutate(Diff=round(Diff, 2),
           `p-value`=round(`p-value`,4))%>%
    select(var, everything())

a1 <- aov(anova_data$RF ~ anova_data$grade)
RF_post <- TukeyHSD(x=a1, 'anova_data$grade', conf.level=0.95)
RF_post <- as.data.frame(RF_post[[1]])[,c(1,4)]
RF_post <- RF_post %>%
    rename(Diff=diff, "p-value"=`p adj`) %>%
    mutate(var=rownames(RF_post))%>%
    mutate(Diff=round(Diff, 2),
           `p-value`=round(`p-value`,4))%>%
    select(var, everything())

### AT box plot--------------
AT_box <- anova_data %>%
    mutate(grade = grade %>% fct_infreq()) %>%
    ggplot(aes(x = grade, y = AT, fill = grade)) +
    geom_boxplot(show.legend = F) +
    theme +
    labs(title="AT ANOVA", x="Grade", y="AT Score")


### RF box plot-------------
RF_box <- anova_data %>%
    mutate(grade = grade %>% fct_infreq()) %>%
    ggplot(aes(x = grade, y = RF, fill = grade)) +
    geom_boxplot(show.legend = F) +
    theme +
    labs(title="RF ANOVA", x="Grade", y="RF Score")