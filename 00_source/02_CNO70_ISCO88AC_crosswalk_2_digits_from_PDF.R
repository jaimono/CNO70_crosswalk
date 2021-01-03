# =============================================================== #
# Title:                Read crosswalk from DANE_CNO70 PDF
# Author:               Jaime Montana
# Date:                 2-1-2021
# Description:          Creates the crosswalk table for CNO70 and ISCO-88. This file creates the 2-D version
# =============================================================== #

library(dplyr)


data <- readRDS("02_data/extractPDF.rds")

data$CIUO88_AC_2D <- ifelse(data$CIUO88_AC == "<NA>",NA,substr(as.character(data$CIUO88_AC),start = 2,stop = 3))
data$CNO70_2D     <- ifelse(data$CNO70     == "<NA>",NA,substr(as.character(data$CNO70),start = 2,stop = 3))

# Since weare going to classify from CNO70 , we supress the missing values in that variable.
data <- data %>%filter(!is.na(CNO70_2D))
        
             

data <- data %>% 
        select(CIUO88_AC_2D,CNO70_2D)     %>%
        arrange(CNO70_2D)                 %>% 
        group_by(CNO70_2D,CIUO88_AC_2D)   %>% 
        mutate(repeticiones = n())        %>% 
        unique() %>%
        group_by(CNO70_2D) %>%
        filter(repeticiones == max(repeticiones)) %>%
        select(CNO70_2D,CIUO88_AC_2D) %>%
        relocate(CNO70_2D,CIUO88_AC_2D)


saveRDS(data, "02_data/CNO70_ISCO88AC_crosswalk.rds")
write.csv(data, "02_data/CNO70_ISCO88AC_crosswalk.csv", row.names=FALSE)

rm(list = ls())
