    # toc_depth: 2
    # code_folding: hide
    # number_sections: true
    # toc_float: true
    #   collapsed: false
    #   smooth_scroll: false

# install.packages("caret", dependencies=TRUE)
# install.packages("randomForest")
# install.packages("monomvn")
# install.packages("corrplot")
# install.packages("Amelia")
# install.packages("L1pack")
# install.packages("combinat")
# install.packages('knitr', dependencies=TRUE)


# Inicializa tudo
# library(ggplot2)
# library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(monomvn)
library(corrplot)
library(Amelia)
library(L1pack)
library(combinat)
library(knitr)

setwd("/Users/ema/Documents/empregos/vivareal")
train_all=read.csv("train.csv")
train_all_numeric=train_all[, c("feature_0", "feature_7", "feature_8", "feature_9", "feature_10", "feature_11", "feature_12", "feature_14", "feature_15", "feature_16", "target")]

# png(filename="missmap.png")
png(filename="missmap3.png", width=1344, height=960, units="px", pointsize=32, bg="transparent")
missmap(train_set_numeric, col=c("gray", "black"))
dev.off()

?png



rmarkdown::render("main.Rmd")



