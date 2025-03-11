library(readxl)
library(ggplot2)
setwd("C:/Users/soch1van/Desktop/дз/R labs")
data <- read_excel("data.xlsx")

data_new <- data[-1]
max_values <- apply(data_new, 2, max, na.rm = TRUE)
min_values <- apply(data_new, 2, min, na.rm = TRUE)
mean_values <- apply(data_new, 2, mean, na.rm = TRUE)


task1 <- data.frame(
  Max = max_values,
  Min = min_values,
  Mean = mean_values
)

selected_data1 <- data[data$Уни > 5, ]
selected_data2 <- data[data$KFC == 10, ]

high_pref <- colSums(data_new > 7, na.rm = TRUE)
low_pref <- colSums(data_new < 3, na.rm = TRUE)
task2 <- data.frame(High = high_pref, Low = low_pref)

task3 <- sort(colMeans(data_new, na.rm = TRUE), decreasing = TRUE)
print(task3)

data_no_na <- na.omit(data_new)
print(data_no_na)

data_replace_na <- data_new
for (col in names(data_replace_na)) {
  data_replace_na[[col]][is.na(data_replace_na[[col]])] <- mean(data_replace_na[[col]], na.rm = TRUE)
}
#--------------------------------------------------------------

barplot(task3, 
        main = "Средние оценки всех заведений",
        xlab = "Заведения",
        ylab = "Средняя оценка",
        col = "lightblue",
        las = 2)
