test_data <- iris
iris[2,1]

plot(test_data$Sepal.Length, test_data$Petal.Width, ylim=c(0, 2.5), xlim=c(4, 8))

(-3 + 1 + 2 + 3 + 5 + 6 + 7)/7

(1 + 2 + 3 + 4 + 6 + 8 + 11)/7

library(ggplot2)
d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 

df <- data.frame('x' = c(-3, 1, 2, 3, 5, 6, 7), 'y' = c(3, 4, 6, 8, 2, 11, 1))
kmeans(df, 1)

library(ggplot2) 
library(ggrepel) # для симпатичной подписи точек на графике

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data[c("x", "y")])
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 2) # укажите желаемое число кластеров, сейчас стоит 2

# data(swiss)
data <- swiss
fit <- prcomp(swiss, center = T)
plot(fit, type = "l")
summary(fit)
biplot(fit)

install.packages("pca3d")
library(pca3d)

dt <- swiss
dt$is_catholic <- ifelse(swiss$Catholic > 50, 1, 0)
dt$is_catholic <- factor(dt$is_catholic)
fit <- prcomp(swiss, center = T)
pca3d(fit, group = dt$is_catholic,
      fancy = T, 
      new=T)

df2 <- mtcars
fit <- prcomp(mtcars[c('mpg', 'disp', 'hp', 'drat', 'wt', 'qsec')], center = T)
plot(fit, type = "l")
summary(fit)
biplot(fit)

fit <- factanal(swiss, factors = 2, rotation = "varimax")
print(fit)

# Exercise 1

# Напишите функцию smart_hclust, которая получает на вход dataframe  с произвольным числом количественных переменных и число кластеров, 
# которое необходимо выделить при помощи иерархической кластеризации.
# Функция должна в исходный набор данных добавлять новую переменную фактор - cluster  -- номер кластера, к которому отнесено каждое из наблюдений.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")

smart_hclust <- function(test_data, cluster_number){
  d = dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(d, method = "complete", members = NULL)
  test_data$cluster <- cutree(fit, cluster_number)
  test_data$cluster <- as.factor(test_data$cluster)
  return(test_data)
}

smart_hclust(test_data, 3)

# Exercise 2

# Интересной особенностью кластерного анализа является тот факт, что мы получаем только итоговый ответ, к какому кластеру принадлежит каждое наблюдение. 
# Однако мы не знаем, по каким переменным различаются выделенные кластеры. Поэтому, если нас интересует не только сам факт того, что мы смогли выделить кластеры в наших данных, 
# но мы также хотим понять, чем же они различаются, разумно сравнить кластеры между собой по имеющимся переменным.
# Напишите функцию get_difference, которая получает на вход два аргумента: 
# test_data — набор данных с произвольным числом количественных переменных.
# n_cluster — число кластеров, которое нужно выделить в данных при помощи иерархической кластеризации. 
# Функция должна вернуть названия переменных, по которым были обнаружен значимые различия между выделенными кластерами (p < 0.05). 
# Иными словами, после того, как мы выделили заданное число кластеров, мы добавляем в исходные данные новую группирующую переменную — номер кластера, 
# и сравниваем получившиеся группы между собой по количественным переменным при помощи дисперсионного анализа.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")

get_difference <- function(test_data, n_cluster){
  col_count <- ncol(test_data)
  x <- c()
  
  d = dist(test_data)
  fit <- hclust(d)
  test_data$cluster <- cutree(fit, n_cluster)
  test_data$cluster <- as.factor(test_data$cluster)
  # print(test_data$cluster)
  
  for(i in 1:col_count){
    aov <- aov(test_data[, i] ~ cluster , data = test_data)
    # print(summary(aov))
    
    if(summary(aov)[[1]][[1, "Pr(>F)"]] < 0.05){
      x <- c(x, toString(colnames(test_data)[i]))
    }
  }
  return(x)
  # print(x)
}

get_difference(test_data, 2)

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
get_difference(test_data, 2)

test_data <- as.data.frame(list(X1 = c(11, 9, 8, 14, 7, 9, 21, 20, 20, 20, 11, 20), X2 = c(11, 8, 18, 1, 5, 10, 24, 21, 17, 19, 21, 18), X3 = c(5, 7, 14, 9, 11, 11, 21, 19, 21, 20, 20, 22), X4 = c(9, 15, 11, 9, 11, 5, 21, 21, 15, 22, 18, 22)))
get_difference(test_data, 4)

# Exercise 3

# Напишите функцию get_pc?, которая получает на вход dataframe с произвольным числом количественных переменных. 
# Функция должна выполнять анализ главных компонент и добавлять в исходные данные две новые колонки со значениями первой и второй главной компоненты. 
# Новые переменные должны называться "PC1"  и "PC2" соответственно.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")

get_pc <- function(d){
  fit <- prcomp(d, center = T)
  
  d$PC1 <- fit$x[, 1]
  d$PC2 <- fit$x[, 2]
  
  return(d)
}

get_pc(test_data)

test_data

# Exercise 4

# Усложним предыдущую задачу! Напишите функцию get_pca2, которая принимает на вход dataframe с произвольным числом количественных переменных. 
# Функция должна рассчитать, какое минимальное число главных компонент объясняет больше 90% изменчивости в исходных данных и добавлять значения этих компонент 
# в исходный dataframe в виде новых переменных.
data <- swiss

fit <- prcomp(data)

cumsum(fit$sdev^2 / sum(fit$sdev^2))

# summary(fit)

get_pca2 <- function(data){
  fit <- prcomp(data)
  j <- 0
  
  for(i in 1:ncol(data)){
    if(cumsum(fit$sdev^2 / sum(fit$sdev^2))[i] > 0.9){
      j <- i
      break
    }
  }

  data<- cbind(data, fit$x[,1:j])
  return(data)
}

result  <- get_pca2(swiss)
str(result)

get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}

# Exersice 6

# Давайте построим финальный график!
# В данных swiss, используя все переменные, выделите два кластера при помощи иерархической кластеризации и сохраните значение кластеров как фактор в переменную cluster.
library(ggplot2)

d = dist(swiss)
fit <- hclust(d)
swiss$cluster <- cutree(fit, 2)
swiss$cluster <- as.factor(swiss$cluster)

swiss
str(swiss)

ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_point()+
  geom_smooth(method="lm")

dist_matrix <- dist(swiss)    
fit <- hclust(dist_matrix)     
swiss$cluster <- as.factor(cutree(fit, 2))    
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster)) +      
  geom_point() +      
  geom_smooth(method = 'lm')
