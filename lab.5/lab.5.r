library(factoextra)
library(cluster)
library(parameters)
library(scatterplot3d)
library(klaR)
library(party)

setwd("C:/Users/soch1van/Desktop/r_studio/lab.5")

library(readr)

data <- read.csv("Mobile.csv")
#========================================================================
summary(data)

features_1 <- c("ram", "int_memory", "battery_power", "px_width")
features_2 <- c("three_g", "wifi", "four_g", "three_g", "blue", "dual_sim", "touch_screen")
test_all <- names(data)

par(mfrow = c(2, 3))

for (feature in features_1) {
  hist(data[[feature]],
       main = paste(feature),
       xlab = "Значение",
       ylab = "Количество",
       col = "green")
}

for (feature in features_1) {
  boxplot(data[[feature]] ~ data$price_range,
          main = paste(feature),
          xlab = "Ценовая категория",
          ylab = feature,
          col = "green")
}

for (feature in test_all) {
  avg <- tapply(data[[feature]], data$price_range, mean)
  
  barplot(avg,
          main = paste(feature),
          xlab = "Ценовая категория",
          ylab = "Среднее значение",
          col = "green")
}
#========================================================================
data_clear <- data[, !(names(data) %in% "price_range")]
maxs <- apply(data_clear, 2, max)
mins <- apply(data_clear, 2, min)
scaled_data <- scale(data_clear, center = mins, scale = maxs - mins)
scaled_data <- as.data.frame(scaled_data)
scaled_data <- scaled_data[, c("ram", "battery_power")]

scaled_data <- scaled_data[, !(names(scaled_data) %in% features_2)]
#========================================================================
fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(title = "Метод силуэта: выбор числа кластеров")

fviz_nbclust(scaled_data, kmeans, method = "wss") +
  labs(title = "Метод локтя: выбор числа кластеров")

set.seed(123)
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 5, K.max = 6, B = 5)
fviz_gap_stat(gap_stat) + labs(title = "Статистика разрыва")

set.seed(123)
n_clust <- n_clusters(scaled_data, package = c("easystats", "NbClust", "mclust"), standardize = FALSE)
n_clust
plot(n_clust)
#========================================================================
dist_matrix <- dist(scaled_data, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc,
     main = "Дендрограмма: иерархическая кластеризация телефонов",
     ylab = "Евклидово расстояние",
     sub = "",
     cex = 0.7)  
rect.hclust(hc, k = 4, border = "red")  
#========================================================================
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)


fviz_cluster(kmeans_result, data = scaled_data, 
             geom = "point", 
             palette = "Set2", ggtheme = theme_minimal(),
             main = "Визуализация кластеров (k-means)")

fviz_cluster(kmeans_result, data = scaled_data, 
             geom = "point", 
             ellipse.type = "norm",
             main = "Визуализация кластеров (k-means)")
#========================================================================
groups <- cutree(hc, k=4) 
g1<-colMeans(scaled_data[groups==1,])
g2<-colMeans(scaled_data[groups==2,])
g3<-colMeans(scaled_data[groups==3,])
g4<-colMeans(scaled_data[groups==4,])

df<-data.frame(g1,g2,g3,g4)
df1<-t(df)
df<-t(df1)

barplot(df, ylim=c(0,12), axes = FALSE, 
        col=c("red","green"), beside=TRUE)
axis(2, at = 0:5, labels = 0:5)
legend("top", legend = rownames(df), col=c("red","green"), lwd=10, bty = "n")
#========================================================================
pairs(scaled_data)
colors <- c("red","green","blue","yellow")
pairs(scaled_data,pch = 19, cex = 0.8,
      col = colors[groups],
      lower.panel=NULL)

colors <- c("red","green","blue","yellow")
colors_groups <- colors[groups]
scatterplot3d(x = scaled_data$ram,
              y = scaled_data$battery_power,
              z = rep(0, nrow(scaled_data)),
              pch = 19,
              color = colors[groups],
              main = "3D кластеризация",
              xlab = "Ram",
              ylab = "Battery Power",
              zlab = "")
#========================================================================
data_2 = scaled_data
data_2$group <- as.factor(kmeans_result$cluster)

naive_df <- NaiveBayes(data_2$group ~ ., data=data_2)
naive_df$tables

opar=par()
par(mfrow = c(1, 2))
plot(naive_df)
par=opar

pred <- predict(naive_df, data_2[, -ncol(data_2)])
table(Факт = data_2$group, Прогноз = pred$class)
Accuracy <- mean(pred$class == data_2$group)
Accuracy
cat(paste("Точность ", round(100 * Accuracy, 2), "%", sep=""))
#========================================================================
set.seed(1234)
ind <- sample(2, nrow(data_2), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data_2[ind==1,]
testData <- data_2[ind==2,]
nrow(trainData) 
nrow(testData) 
nrow(data_2)

myFormula <- group ~ .

mobile_tree <- ctree(myFormula, data=trainData)
table(predict(mobile_tree), trainData$group)

plot(mobile_tree)
    
test_predicted <- predict(mobile_tree, newdata=testData)
table(test_predicted, testData$group)

cat(paste("Точность ", round(100 * mean(test_predicted == testData$group), 2), "%", sep=""))
#========================================================================
cf <- cforest(group ~ ., data=trainData, control=cforest_unbiased(mtry=2,ntree=100))
table(predict(cf), trainData$group)
cf
cat(paste("Точность ", round(100 * mean(predict(cf) == trainData$group), 2), "%", sep=""))


