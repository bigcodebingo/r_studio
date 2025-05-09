library(factoextra)
library(cluster)
library(parameters)
library(scatterplot3d)
library(tm)
library(text2vec)

setwd("C:/Users/soch1van/Desktop/r_studio/lab.5")

library(readr)

data <- read.csv("Airplan_Crushes.csv")
last_col_index <- ncol(data)
data[[last_col_index]] <- gsub(";;;;;", "", data[[last_col_index]])
data[data == "" | data == "-"] <- NA
data <- data[!is.na(data$Summary), ]

#========================================================================
str(data)  
summary(data$Aboard)
summary(data$Fatalities)
summary(data$Ground)
colSums(is.na(data)) 
#========================================================================
operator_fatalities <- aggregate(Fatalities ~ Operator, data = data, sum)
operator_fatalities <- operator_fatalities[order(-operator_fatalities$Fatalities), ]  
top_operators <- head(operator_fatalities, 100)

par(mar = c(11, 5, 3, 1))
barplot(top_operators$Fatalities, 
        names.arg = top_operators$Operator,
        col = rainbow(length(top_operators$Operator)),
        las = 2,                  
        main = "Статистика смертей",
        ylab = "кол-во смертей",
        cex.names = 0.6)
#========================================================================
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
data$Year <- format(data$Date, "%Y")
yearly_fatalities <- aggregate(Fatalities ~ Year, data = data, sum)

par(mar = c(11, 5, 3, 1))
barplot(yearly_fatalities$Fatalities, 
        names.arg = yearly_fatalities$Year,
        col = rainbow(length(yearly_fatalities$Year)),
        las = 2,                  
        main = "Статистика смертей",
        ylab = "кол-во смертей",
        cex.names = 0.6)
#========================================================================














