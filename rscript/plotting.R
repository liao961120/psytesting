library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(cowplot)
library(scales) # for ggplot color

psy_test <- read_rds("./data/psy_test_analy.rds")
var_info <- read_rds("./data/item_construct_conNum.rds")

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
# ggplotly(pl_cutoff)


## Distributions-------------------
psy_test_f <- psy_test %>%
    filter(grade != "first") %>%
    filter(Fk <= 18)

plot_distr <- function(item, color="#F8766D"){
    pl <- ggplot(data = psy_test_f)+
        geom_bar(aes(x=psy_test_f[,item]), fill=color)+
        scale_y_continuous(limits = c(0,125))+
        scale_x_discrete(limits=c("1","2","3","4","5"))+
        labs(x="", y="")
    pl
}

cl <- hue_pal()(4) ## ggplot default color generator 

### AT----------------------
AT_item <- unname(unlist(var_info[var_info$construct_en=="AT" & !is.na(var_info$construct_en),"vars_en"]))
p_AT <- plot_grid(plot_distr(AT_item[1]), plot_distr(AT_item[2]),
                  plot_distr(AT_item[3]), plot_distr(AT_item[4]),
                  plot_distr(AT_item[5]), plot_distr(AT_item[6]),
                  plot_distr(AT_item[7]), plot_distr(AT_item[8]),
                  plot_distr(AT_item[9]),
                  labels = unlist(var_info[var_info$construct_en=="AT","con_num"]), vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

### CF----------------------
CF_item <- unname(unlist(var_info[var_info$construct_en=="CF" & !is.na(var_info$construct_en),"vars_en"]))
label <- unlist(var_info[var_info$construct_en=="CF" & !is.na(var_info$construct_en),"con_num"])
    
p_CF1 <- plot_grid(plot_distr(CF_item[1],cl[2]), plot_distr(CF_item[2],cl[2]),
                  plot_distr(CF_item[3],cl[2]), plot_distr(CF_item[4],cl[2]),
                  plot_distr(CF_item[5],cl[2]), plot_distr(CF_item[6],cl[2]),
                  plot_distr(CF_item[7],cl[2]), plot_distr(CF_item[8],cl[2]),
                  plot_distr(CF_item[9],cl[2]), plot_distr(CF_item[10],cl[2]),
                  plot_distr(CF_item[11],cl[2]), plot_distr(CF_item[12],cl[2]),
                  labels = label[1:9], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

p_CF2 <- plot_grid(plot_distr(CF_item[12],cl[2]), plot_distr(CF_item[13],cl[2]), 
                   plot_distr(CF_item[14],cl[2]), plot_distr(CF_item[15],cl[2]), 
                   plot_distr(CF_item[16],cl[2]), plot_distr(CF_item[17],cl[2]),
                   labels = label[10:17], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

### HD----------------------
item <- unname(unlist(var_info[var_info$construct_en=="HD" & !is.na(var_info$construct_en),"vars_en"]))
label <- unlist(var_info[var_info$construct_en=="HD" & !is.na(var_info$construct_en),"con_num"])

p_HD1 <- plot_grid(plot_distr(item[1],cl[3]), plot_distr(item[2],cl[3]),
                   plot_distr(item[3],cl[3]), plot_distr(item[4],cl[3]),
                   plot_distr(item[5],cl[3]), plot_distr(item[6],cl[3]),
                   plot_distr(item[7],cl[3]), plot_distr(item[8],cl[3]),
                   plot_distr(item[9],cl[3]), plot_distr(item[10],cl[3]),
                   plot_distr(item[11],cl[3]), plot_distr(item[12],cl[3]),
                   labels = label[1:12], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

p_HD2 <- plot_grid(plot_distr(item[13],cl[3]), plot_distr(item[14],cl[3]),
                   plot_distr(item[15],cl[3]), plot_distr(item[16],cl[3]),
                   plot_distr(item[17],cl[3]), plot_distr(item[18],cl[3]),
                   plot_distr(item[19],cl[3]), plot_distr(item[20],cl[3]),
                   labels = label[13:20], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)

### RF----------------------
item <- unname(unlist(var_info[var_info$construct_en=="RF" & !is.na(var_info$construct_en),"vars_en"]))
label <- unlist(var_info[var_info$construct_en=="RF" & !is.na(var_info$construct_en),"con_num"])

p_RF <- plot_grid(plot_distr(item[1],cl[4]), plot_distr(item[2],cl[4]),
                   plot_distr(item[3],cl[4]), plot_distr(item[4],cl[4]),
                   plot_distr(item[5],cl[4]), plot_distr(item[6],cl[4]),
                   plot_distr(item[7],cl[4]), plot_distr(item[8],cl[4]),
                   plot_distr(item[9],cl[4]), plot_distr(item[10],cl[4]),
                   labels = label[1:10], vjust = 1, hjust=-2, label_size=11, ncol=4, nrow=3)