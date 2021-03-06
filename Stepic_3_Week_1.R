library(ggplot2)
df <- mtcars

ggplot(df, aes(x=hp, y=mpg)) +
  geom_point()

# ����� �� hp
ggplot(df, aes(x=hp^0.5, y=mpg)) +
  geom_point()

# � ������ -0.5 - ���� ���������� - �� �������, ���� ������� �� ������ ���� �������
ggplot(df, aes(x=hp^-0.5, y=mpg)) +
  geom_point()

ggplot(df, aes(x=-hp^-0.5, y=mpg)) +
  geom_point()

# ������������ ���� �������� ������ ��� -0.7
ggplot(df, aes(x=-hp^-0.7, y=mpg)) +
  geom_point()


cor(df$hp, df$mpg)

x=-(df$hp)^-0.5
cor(x, df$mpg)

# ������������ ���� �������� ������ ��� -0.7
x=-(df$hp)^-0.7
cor(x, df$mpg)

fit1 <- lm(mpg ~ hp, mtcars)
fit2 <- lm(mpg ~ I(-hp^-0.7), mtcars)

summary(fit1) # R^2 = 0.58
summary(fit2) # R^2 = 0.73

ggplot(df, aes(x=log(hp), y=log(mpg))) +
  geom_point()

fit3 <- lm(log(mpg) ~ log(hp), mtcars)
summary(fit3) # R^2 = 0.7
# log(hp) = -0.53009, ����� ��� ��������� hp �� 1%, mpg ���������� �� 0.5%

log(1.1)*120

# �� ���������� ������� �������
hist(fit1$residuals)
shapiro.test(fit1$residuals)

# ���� ������������� ��� ���������� ������� �������
hist(fit2$residuals)
shapiro.test(fit2$residuals)

hist(fit3$residuals)
shapiro.test(fit3$residuals)

# ��������������������
library(ggplot2)
df <- diamonds

df <- mtcars
ggplot(df, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_smooth(method = lm)

fit_1 <- lm(mpg ~ hp, df)
plot(fit_1)

ggplot(df, aes(x=price, y=carat)) +
  geom_point() +
  geom_smooth(method = lm)

fit_2 <- lm(carat ~ price, df)
summary(fit_2)

fit_3 <- lm((fit_2$residuals)^2 ~ price, df)
summary(fit_3)$r.squared

sd(df$price)
3989.44^2
var(df$price)
var(df$carat)

fit_4 <- lm(price ~ carat, df)
summary(fit_4)      

# ���� ����� ��� ���� ������-�����
library(vars)
library(het.test)

bptest(lm(price ~ carat, df))

# �������� �������� ������������������
library(dplyr)
df_2 <- sample_n(diamonds, 500)

ggplot(df_2, aes(x=price, y=carat)) +
  geom_point() +
  geom_smooth(method = lm)

fit_1 <- lm(carat ~ price, df_2)
summary(fit_1)

fit_2 <- lm(I(fit_1$residuals^2) ~ price, df_2) 
summary(fit_2)
plot(fit_2)

library(lmtest)
# p<0,05 - � ����� �������� ��������������������
bptest(lm(price ~ carat, df_2))

# �������� �� ���� ������
ggplot(df_2, aes(x=log(price), y=log(carat))) +
  geom_point() +
  geom_smooth(method = lm)
bptest(lm(log(price) ~ log(carat), df_2))

fit_3 <- lm(log(price) ~ log(carat), df_2)
summary(fit_3)
plot(fit_3)

shapiro.test(fit_2$residuals)
shapiro.test(fit_3$residuals)

# ������������������
library(dplyr)
set.seed(42)
d <- data_frame(y = rnorm(30),
                x_1 = rnorm(30),
                x_2 = x_1,
                x_3 = rnorm(30))
pairs(d)
fit <- lm(y ~ ., d)
summary(fit)

# cars <- mtcars[c('mpg', 'cyl')]
# head(cars)

head(swiss)
fit_1 <- lm(Fertility ~ ., swiss)
summary(fit_1)

# Examination � fit_1 �� ��������, ����� ��������� ���� ���
# ������� ����� ������������������
# ��� ����� ������ �� ��������� �� ��� � ���������� ������
cor.test(~ Fertility + Examination, swiss)

library(car)
library(dplyr)
vif(fit_1)
# ��������� vif � Examination, �������� ���� �������� � �����

fit_2 <- lm(Fertility ~ ., select(swiss, -Examination))
summary(fit_2)
# ����� �� ����������� ������

vif(fit_2)

# Exercise 1

# ������� ��������� ���������� ������� ����� ��� �������� ������� ��������������������.  �������� ������� hetero_test, ������� �������� �� ���� ����� ������. 
# ������ ������� � ������ - ��������� ����������, ��������� ������� - �����������. ������� ������ ������������� ������, ��������� ��� ����������, 
# � ����� ���������, ���� �� � ������  ��������������������.
# ��� ����� ������� ������ ��������������� ���������, � ������� ��������� ���������� - ��� �������� �������� �������� ������, 
# � ����������� ���������� - ��� ���������� �� �������� ������. ������� ������ ���������� �������� R ������� ���� ��������������� ������.
df <- mtcars
str(df)
hetero_test <-function(test_data){
  test_data_2 <- test_data[,2:ncol(test_data)]
  
  # print(str(test_data))
  # print(ncol(test_data))
  # print(test_data[,1])
  
  fit_1 <- lm(test_data[,1] ~ ., test_data_2)
  fit_2 <- lm((fit_1$residuals^2) ~ ., test_data_2)
  return(summary(fit_2)$r.squared)
  # return(summary(fit_2)  $adj.r.squared)
}
hetero_test(df)

# mtcars_2 <- mtcars[,2:11]
mtcars_2 <- mtcars[,2:ncol(mtcars)]
fit_1 <- lm(mtcars[,1] ~ ., mtcars_2)
fit_2 <- lm((fit_1$residuals^2) ~ ., mtcars_2)
print(summary(fit_2)$r.squared)

# Exercise 4

# ������� ��������� ����� �������� ���������� ������� ��� ������������� ����������� ���������� � ������� ���������, � ����� �����������.
# ���� ������� transform_x �������� �� ���� ����� ������ �� ���� �������, ������ ������� y � ������ x. 
# ������� ������ ����� ����� ���������� ������� ��� ������������� x, ��� ������� ����� x � y ����� ������������ ���������� �������� ����������.
set.seed(42)
test_data <- data.frame(y = rnorm(10, 10, 1), x = rnorm(10, 10, 1))
print(test_data)

transform_x <-  function(test_data){
  df <- cbind(test_data)
  df_2 <- cbind(test_data)

  arr <- c()
  max_rsquared <- 0
  
  # print(df)
  for(i in seq(-2, 2, 0.1)){
    # print(i)
    
    if(i < 0){
      df$x <- -(df_2$x^i)
    }
    else if(i == 0) {
      df$x <- log(df_2$x)
    }
    else if(i > 0){
      df$x <- (df_2$x^i)
    }

    # print(df$x)
    fit <- lm(y ~ x, df)
    # print(summary(fit)$r.squared)
    
    if(summary(fit)$r.squared > max_rsquared){
      max_rsquared <- summary(fit)$r.squared
      arr <- df$x
    }
  }
  # print(df$x)
  print(arr)
}
transform_x(test_data)

# log(11.304870)
test_data$x <- log(test_data$x)
test_data
test_data$x <- test_data$x ^ -1.9
test_data
