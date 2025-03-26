library(readxl)

setwd("C:/Users/soch1van/Desktop/r_studio")
data <- read_excel("romania_rowing.xlsx")

data_new <- data[!grepl("мужчины|женщины", data$"Олимпийские Игры"), ]
data_men <- data[grepl("мужчины", data$"Олимпийские Игры"), ]
data_women <- data[grepl("женщины", data$"Олимпийские Игры"), ]

process_dataframe <- function(df){
  data_matrix <- as.matrix(df)
  data_matrix <- data_matrix[, -c(1, 10)]
  data_matrix <- apply(data_matrix, 2, as.numeric)
  rownames(data_matrix) <- data_new$"Олимпийские Игры"
  return(data_matrix)
}

matrix <- process_dataframe(data_new)
matrix_men <- process_dataframe(data_men)
matrix_women <- process_dataframe(data_women)


par(mar = c(11, 4, 2, 8), xpd = TRUE)

task1 <- barplot(t(matrix), beside = TRUE, col = rainbow(8),
              main = "Количество мест 1-8 по каждой Олимпиаде",
              ylab = "Количество мест",
              las = 2, axisnames = TRUE)

last_x <- max(task1)
legend(x = last_x, y = par("usr")[4], legend = paste("Место", 1:8),
       fill = rainbow(8),bty = "n", cex = 0.9,
       x.intersp = 0.3, y.intersp = 0.6)
#=================================================================
first_places <- matrix[matrix[, 1] != 0, 1]

pie(first_places, 
    main = "Количество первых мест Румынии по академической гребле",
    col = rainbow(length(first_places)), 
    labels = first_places)

legend(2, 1.1, 
       legend = names(first_places),
       fill = rainbow(length(first_places)), 
       title = "Олимпиада", 
       bty = "n",
       cex = 0.7)
#=================================================================
total_men <- rowSums(matrix_men)
total_women <- rowSums(matrix_women)
years <- sapply(data_new[, 1], function(x) as.numeric(gsub("[^0-9]", "", x)))

plot(years, total_men, type = "o", col = "darkblue",pch = 19, 
     main = "Тенденции изменения количества призовых мест",
     xlab = "Год", ylab = "Количество призовых мест", 
     ylim = c(0, max(total_men, total_women) + 1), xaxt = "n")
lines(years, total_women, type = "o", col = "magenta",pch = 19)
legend("topleft", legend = c("Мужчины", "Женщины"), col = c("darkblue", "magenta"), 
       lty = 1,bty = "n",cex = 0.8)
axis(1, at = years, labels = years, las = 2)
#=================================================================
men_last_6 <- matrix_men[1:6,]
par(mar = c(11, 4, 2, 8), xpd = TRUE)

task2 <- barplot(t(men_last_6), beside = TRUE, col = rainbow(8),
                 main = "Количество мест среди мужчин",
                 ylab = "Количество мест",
                 las = 2, axisnames = TRUE)

last_x <- max(task2)
legend(x = last_x, y = par("usr")[4], legend = paste("Место", 1:8),
       fill = rainbow(8),bty = "n", cex = 0.9,
       x.intersp = 0.3, y.intersp = 0.6)

women_last_6 <- matrix_women[1:6,]
task3 <- barplot(t(women_last_6), beside = TRUE, col = rainbow(8),
                 main = "Количество мест среди женщин",
                 ylab = "Количество мест",
                 las = 2, axisnames = TRUE)

last_x <- max(task3)
legend(x = last_x, y = par("usr")[4], legend = paste("Место", 1:8),
       fill = rainbow(8),bty = "n", cex = 0.9,
       x.intersp = 0.3, y.intersp = 0.6)
#=================================================================
par(mfrow = c(1, 2), mar = c(2, 2, 3, 2))

total_men_6 <- rowSums(matrix_men[1:6,])
non_zero_men <- total_men_6 > 0
filtered_labels_men <- names(total_men_6)[non_zero_men]
pie(total_men_6[non_zero_men], 
    main = "Мужчины: призовые места",
    col = rainbow(length(total_men_6[non_zero_men])), 
    labels = total_men_6[non_zero_men],
    cex.main = 0.9) 

legend("topright", 
       legend = filtered_labels_men,
       fill = rainbow(length(total_men_6[non_zero_men])), 
       bty = "n",
       cex = 0.7)

total_women_6 <- rowSums(matrix_women[1:6,])
non_zero_women <- total_women_6 > 0
filtered_labels_women <- names(total_women_6)[non_zero_women]
pie(total_women_6[non_zero_women], 
    main = "Женщины: призовые места",
    col = rainbow(length(total_women_6[non_zero_women])), 
    labels = total_women_6[non_zero_women],
    cex.main = 0.9)

legend("topright",
       xpd = TRUE,
       inset = c(-0.5, 0),
       legend = filtered_labels_women,
       fill = rainbow(length(total_women_6[non_zero_women])), 
       bty = "n",
       cex = 0.7)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
#=================================================================
data_all <- read_excel("top_7.xlsx")
years <- data_all$Олимпиада
gold_medals <- data_all[, c("США 1 место", "Россия 1 место", "Китай 1 место", 
  "Австралия 1 место", "Япония 1 место", "Франция 1 место", "Великобритания 1 место")]

par(mar = c(5, 5, 5, 13)) 
plot(years, gold_medals[[1]], type = "o", col = "red",
     main = "Золотые медали по странам",
     xlab = "Год",
     ylab = "Количество золотых медалей",
     ylim = c(0, max(gold_medals, na.rm = TRUE) + 5),
     pch = 19, las = 1,
     legend.text = paste("Место", 1:8), args.legend = list(x = "topright"))

colors <- rainbow(length(gold_medals))
for (i in 1:7) {
  lines(years, gold_medals[[i]], type = "o", col = colors[i], pch = 19)
}

legend("topright", legend = c("США", "Россия", "Китай", "Австралия", "Япония", "Франция", 
      "Великобритания"), xpd = TRUE, bty = "n", inset = -c(0.7, 0), fill = colors)
#==================================================================
data_all <- read_excel("top_7_summ.xlsx")
years <- data_all$Олимпиада
medals <- data_all[, c("США", "Россия", "Китай", "Австралия", "Япония", "Франция", 
                       "Великобритания")]

par(mar = c(5, 5, 5, 13)) 
plot(years, medals[[1]], type = "o", col = "red",
     main = "Медали 1-3 место по странам",
     xlab = "Год",
     ylab = "Количество призовых медалей",
     ylim = c(0, max(medals, na.rm = TRUE) + 5),
     pch = 19, las = 1,
     legend.text = paste("Место", 1:8), args.legend = list(x = "topright"))

colors <- rainbow(length(medals))
for (i in 1:7) {
  lines(years, medals[[i]], type = "o", col = colors[i], pch = 19)
}

legend("topright", legend = c("США", "Россия", "Китай", "Австралия", "Япония", "Франция", 
                              "Великобритания"), xpd = TRUE, bty = "n", inset = -c(0.7, 0), fill = colors)