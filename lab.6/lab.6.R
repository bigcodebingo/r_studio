library(car)
library(dplyr)

setwd("C:/Users/soch1van/Desktop/r_studio/lab.6")

data <- read.csv('athlete_events.csv')
rowing_data <- data[data$Sport == "Rowing" & !is.na(data$Weight), c("Name", "Sex", 
                                                      "Weight", "Sport", "Team")]
rowing_data <- rowing_data[!duplicated(rowing_data), ]

dim(rowing_data)     
nrow(rowing_data)     
ncol(rowing_data)  
colSums(is.na(rowing_data))
summary(rowing_data$Weight)

sd(rowing_data$Weight, na.rm = TRUE)
var(rowing_data$Weight, na.rm = TRUE)

rowing_data %>%
  count(Weight) %>%
  arrange(desc(n)) %>%
  filter(n == max(n))
#======================================================
par(mfrow = c(2, 2)) 
hist(rowing_data$Weight, 
     main = "Гистограмма веса", 
     xlab = "вес (кг)", 
     ylab = "кол-во", 
     col = "green")

boxplot(Weight ~ Sex, 
        data = rowing_data, 
        main="Вес по полу", 
        ylab="вес (кг)", 
        xlab="пол",
        col="green")

hist(rowing_data$Weight[rowing_data$Sex == "M"],
     main = "Гистограмма веса (мужчины)",
     xlab = "вес (кг)",
     ylab = "кол-во",
     col = "blue")

hist(rowing_data$Weight[rowing_data$Sex == "F"],
     main = "Гистограмма веса (женщины)",
     xlab = "вес (кг)",
     ylab = "кол-во",
     col = "red")
#======================================================
par(mfrow = c(2, 2)) 
genders <- c("M", "F")

for (gender in genders) {
  cat(gender,"\n")
  
  subdata <- rowing_data[rowing_data$Sex == "M",]
  
  shapiro_result <- shapiro.test(subdata$Weight)
  print(shapiro_result)
  
  qqnorm(subdata$Weight, main = paste(gender))
  qqline(subdata$Weight, col = "blue")
  qqPlot(subdata$Weight, main = paste(gender), col = "red")
}
#======================================================
df <- rowing_data
df$Sex<-as.factor(df$Sex)
leveneTest(Weight ~ Sex, data = rowing_data)
#======================================================
wilcox.test(rowing_data$Weight, mu=80, conf.int=TRUE)
t.test(rowing_data$Weight, mu=80, conf.int=TRUE)
#======================================================
skating_and_skiing <- data[data$Sport %in% c("Speed Skating", "Alpine Skiing") & data$Sex == "M" & !is.na(data$Weight), c("Name", "Sex", "Weight", "Sport")]
skating_and_skiing <- skating_and_skiing[!duplicated(skating_and_skiing), ]

shapiro.test(skating_and_skiing$Weight)

qqnorm(skating_and_skiing$Weight)
qqline(skating_and_skiing$Weight)

leveneTest(Weight ~ Sport, data = skating_and_skiing)

skiing <- skating_and_skiing[skating_and_skiing$Sport == "Alpine Skiing" ,]
skating <- skating_and_skiing[skating_and_skiing$Sport == "Speed Skating" ,]

wilcox.test(skiing$Weight, skating$Weight)





