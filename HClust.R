options(stringsAsFactors = FALSE)

library(tidyverse)
library(cluster)
library(dendextend)
library(devtools)
library(skimr)
library(factoextra)

train_num = read_csv("train_num.csv")

# ----------------------------- #
####### HClust #######

train_num_s = scale(train_num)

dist_num = dist(train_num_s)

h3 = hcut(dist_num, k = 3)
fviz_dend(h3, rect = TRUE)

h2 = hcut(dist_num, k = 2)
fviz_dend(h2, rect = TRUE)

h6 = hcut(dist_num, k = 6)
fviz_dend(h6, rect = TRUE)

h5 = hcut(dist_num, k = 5)
fviz_dend(h5, rect = TRUE)

h4 = hcut(dist_num, k = 4)
fviz_dend(h4, rect = TRUE)

# Will choose 4 clusters