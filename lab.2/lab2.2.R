library(ggplot2)
library(readxl)

setwd("C:/Users/soch1van/Desktop/r_studio")
data <- read.csv("data.csv", row.names = 1)
data_new <- data[-(1)]
#-------------------------------------------
par(mfrow = c(1, 4))
for (col in colnames(data_new)) {
  hist(data_new[[col]], 
       main = paste(col), 
       xlab = "Оценка", 
       ylab = "Частота", 
       col = "lightblue", 
       breaks = 10)
}
par(mar = c(12,4,4,2))
boxplot(data_new, main = "boxplot для всех ресторанов", xlab = "Рестораны", ylab = "Значения", col = "lightblue",las = 2)
par(mar = c(5,4,4,2))

data_stats <- summary(data_new)
print(data_stats)
#----------------------------------------------
data_sort <- data_new[order(data$Уни), ]
#----------------------------------------------
tori_selected <- subset(data_new, Tori.ramen > 7)
print(dim(tori_selected))

tori_selected_stats <- summary((tori_selected))
print(tori_selected_stats)

par(mar = c(12,4,4,2))
boxplot(tori_selected, main = "boxplot для Tori >7", xlab = "Рестораны", ylab = "Значения", col = "lightblue",las = 2)
par(mar = c(5,4,4,2))


ggplot(tori_selected, aes(x = as.factor(Tori.ramen))) +
  geom_bar(fill = "lightblue", color = "black", position = "dodge") +
  labs(title = "Гистограмма всех оценок Tori.ramen", x = "Оценка Tori.ramen", y = "Частота")
#-----------------------------------------------
data1 <- read_excel("data1.xlsx")
data2 <- read_excel("data2.xlsx")

data_main <- merge(data1, data2, by = "Фамилия")

data3 <- read_excel("data3.xlsx")

data_main <- rbind(data_main, data3)

places <- names(data_main) %in% c("KFC", "Уни") 
data_remove_cols <- data_main[!places]
remove(places)

data_remove_rows <- data_remove[c(-1,-7)]

data_subset <- subset(data_main, KFC > 0.7 | Уни < 0.4, select=c(Фамилия, KFC, Уни))