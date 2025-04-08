#1. операции с векторами
q <- c(0, 1, 2, 3)
p <- c(7, 6, 5, 4)

sum_pq <- p + q
diff_pq <- p - q
prod_pq <- p * q
div_pq <- p / q
exp_pq <- p ^ q

#2. примеры векторов
vec1 <- 1:20
vec1[seq(1, length(vector1), by = 2)] <- 0
vec1
vec_2 <- 2^(0:19)
vec_2
vec_3 <- 10^(0:4)

#3. подсчет сумм
seq1 <- sum(1 / (1:50 * 2:51))
seq2 <- sum(1 / (2^(0:20)))
num <- seq(1, 28, by=3)
den <- 3^(0:(length(num)-1))
seq3_sum <- sum(num / den)
se3_val <- num/den
count_above_0.5 <- sum(se3_val > 0.5)

#4. работа с вектором 
vec3 <- seq(3, 27, by = 3)
vec3.1 <- vec3[c(2, 5, 7)]
vec3.2 <- vec3[length(vec3) - 1]
vec3.3 <- vec3[-(length(vec3) - 1)]
vec3.4 <- vec3[-6]
vec3.5 <- vec3[100]
vec3.6 <- vec3[-c(1, length(vec3))]
vec3.7 <- vec3[vec3 > 4 & vec3 < 10]
vec3.8 <- vec3[vec3 < 4 |vec3 > 10]

#5. фрейм вариант 3
set.seed(42)
N <- 20

df <- data.frame(
  Nrow = 1:N,  
  Name = paste0("Employee_", 1:N), 
  BirthYear = sample(1960:1985, N, replace = TRUE),  
  EmployYear = NA,  
  Salary = NA  
)

df$EmployYear <- sapply(df$BirthYear, function(x) sample((x + 18):2006, 1))

df$Salary <- ifelse(
  df$BirthYear < 1975,
  (log(2007 - df$EmployYear) +1) * 8000,  
  (log2(2007 - df$EmployYear) +1) * 8000 
)


count_15 = sum(df$Salary > 15000)

calculate_tax_for_year <- function(year, employ_year, birth_year) {
  if (birth_year < 1975) {
    (log(year - employ_year + 1) + 1) * 8000 * 0.13
  } else {
    (log2(year - employ_year + 1) + 1) * 8000 * 0.13
  }
}

Tax <- sapply(1:N, function(i) {
  years <- df$EmployYear[i]:2006  
  taxes_for_years <- sapply(years, function(year) {
    calculate_tax_for_year(year, df$EmployYear[i], df$BirthYear[i])
  })
  sum(taxes_for_years)  
})

df$Tax <- Tax
print(df)

#6. фрейм вариант 18
cat("введите количество векторов (n): ")
n <- scan(nmax = 1, what = integer())

set.seed(42)
index <- 1:n

vectors <- list(
  sample(1:100, 5), 
  runif(5),          
  sample(c(TRUE, FALSE), 5, replace = TRUE),
  sample(letters, 10, replace = FALSE)
)

vectors <- rep(vectors, length.out = n)

df_1 <- data.frame(vectors)
colnames(df_1) <- paste("Column", 1:n, sep = "_")
num_rows <- nrow(df_1)
num_cols <- ncol(df_1)
print(df_1)


