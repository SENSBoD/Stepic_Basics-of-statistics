diamonds = data.frame(diamonds)
ggplot(data = diamonds, aes(color, fill = cut))+
  geom_bar(position = "dodge")


test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
test_data
str(test_data)
summary(test_data)
sum(test_data)
mean(test_data[,c(1)])

centered <- function(test_data, var_names){
  
}

# Напишите функцию smart_test, которая получает на вход dataframe с двумя номинативными переменными с произвольным числом градаций. 
# Функция должна проверять гипотезу о независимости этих двух переменных при помощи критерия хи - квадрат или точного критерия Фишера.
# Если хотя бы в одной ячейке таблицы сопряженности двух переменных меньше 5 наблюдений, 
# функция должна рассчитывать точный критерий Фишера и возвращать вектор из одного элемента: получившегося p - уровня значимости.
# Если наблюдений достаточно для расчета хи-квадрат (во всех ячейках больше либо равно 5 наблюдений), 
# тогда функция должна применять критерий хи-квадрат и возвращать вектор из трех элементов: значение хи-квадрат, 
# число степеней свободы,  p-уровня значимости.
smart_test <-  function(x){
  if(any(table(x) < 5)){
    fish <- fisher.test(table(x))
    return(fish$p.value)
  }
  else{
    chiq <- chisq.test(table(x))
    return(c(chiq$statistic, chiq$parameter, chiq$p.value))
  }
}
table(mtcars[1:20,c("am", "vs")])
smart_test(mtcars[1:20,c("am", "vs")])
table(mtcars[,c("am", "vs")])
smart_test(mtcars[,c("am", "vs")])

df <- table(mtcars[1:20,c("am", "vs")])
any(df<5)
fish <- fisher.test(df)
fish$p.value
df2 <- table(mtcars[,c("am", "vs")])
any(df2<5)
chiq <- chisq.test(df2)
chiq$statistic

# Вся наследственная информация в живых организмах хранится внутри молекулы ДНК. 
# Эта молекула состоит из последовательности четырех "букв" — ?A, T, G и C. 
# Напишите функцию most_significant, которая получает на вход dataframe с произвольным количеством переменных, 
# где каждая переменная это нуклеотидная последовательность. 
# Для каждой переменной мы можем проверить нулевую гипотезу о том, что все нуклеотиды (A, T, G, C) 
# встречаются равновероятно внутри этой последовательности. Однако, возможно, 
# что в некоторых последовательностях распределение частоты встречаемости каждого нуклеотида отличается от равномерного.
# Функция должна возвращать вектор с ?названием переменной (или переменных), в которой был получен минимальный 
# p - уровень значимости при проверке гипотезы о равномерном распределении нуклеотидов при помощи критерия хи - квадрат. 
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
most_significant <-  function(x){
  #print(x)
  a <- apply(x, 2, function(x) chisq.test(table(x)))
  #return(a$V1$p.value)
  return(a)
}
most_significant(test_data)
chisq.test(table(test_data$V1))
table(test_data$V1)
chisq.test(table(test_data$V3))
table(test_data$V3)

# В лекциях я говорил, что иногда возникает необходимость перекодировать количественную переменную в номинативную. 
# Однако зачастую мы можем создавать новую номинативную переменную, комбинируя значения нескольких количественных переменных. 
# Рассмотрим такой пример.
# Воспользуемся встроенными в R данными Iris.
# Создайте новую переменную important_cases - фактор с двумя градациями ("No" и "Yes"). 
# Переменная должна принимать значение Yes, если для данного цветка значения хотя бы трех количественных переменных выше среднего. 
# В противном случае переменная important_cases  будет принимать значение No.
df <- iris
iris$important_cases <- NA
iris$important_cases <- as.factor(c("Yes", "No"))
bigger_value <- 0

mean_1 <- mean(df[1:nrow(df), 1])
mean_2 <- mean(df[1:nrow(df), 2])
mean_3 <- mean(df[1:nrow(df), 3])
mean_4 <- mean(df[1:nrow(df), 4])

for(i in 1:(dim(df)[1])){
  for(j in 1:((dim(df)[2]) - 2)){
    if((df[i, j]) > (mean(df[, j]))){
      bigger_value <- bigger_value + 1
      if(bigger_value >= 3){
        df[i, 6] <- "Yes"
      }
    }
    else{
      df[i, 6] <- "No"
    }
  }
  bigger_value = 0
}

# Обобщим предыдущую задачу! Напишем функцию get_important_cases, которая принимает на вход dataframe 
# с произвольным числом количественных переменных (гарантируется хотя бы две переменные). 
# Функция должна возвращать dataframe с новой переменной - фактором important_cases.
# Переменная  important_cases принимает значение Yes, если для данного наблюдения больше половины количественных переменных 
# имеют значения больше среднего. В противном случае переменная important_cases принимает значение No.
# Переменная  important_cases - фактор с двумя уровнями 0 - "No", 1  - "Yes".  
# То есть даже если в каком-то из тестов все наблюдения получили значения "No", фактор должен иметь две градации. 
table(df$important_cases)

warnings()

dim(df)[1]
dim(df)[2]

df[1, 2] > mean(df[1:nrow(df), 2])
sum(is.na(df))

?warning

importance_calc <- function(v1, v2, threshold=3){    
  ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
iris$important_cases <- factor(apply(iris[1:4], 1, importance_calc, v2 = colMeans(iris[, 1:4])))

test_data <- data.frame(V1 = c(16, 21, 18),
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases <- function(x){
  x$important_cases <- "No"
  bigger_value <- 0
  for(i in 1:(dim(x)[1])){
    for(j in 1:((dim(x)[2]) - 1)){
      if((x[i, j]) > (mean(x[, j]))){
        bigger_value <- bigger_value + 1
        print(c(i, j, bigger_value))
        if(bigger_value > ((dim(x)[2]-1)%/%2)){
          x[i, c('important_cases')] <- "Yes"
        }
      }
      #else{
      #  x[i, c('important_cases')] <- "No"
      #}
    }
    bigger_value = 0
  }
  x$important_cases <- as.factor(x$important_cases)
  levels(x$important_cases) <- c("No", "Yes")
  return(x)
}
get_important_cases(test_data)

test_data <- as.data.frame(list(V1 = c(9, 17, 20, 18, 15, 21, 27), 
                                V2 = c(19, 26, 16, 24, 23, 31, 18), 
                                V3 = c(28, 17, 14, 17, 21, 17, 16)))
get_important_cases(test_data)

mean(test_data$V1)
mean(test_data$V2)
mean(test_data$V3)

(dim(test_data)[2])%/%2

get_important_cases  <- function(d){    
  m <-  colMeans(d)    
  compare_to_means <- apply(d, 1, function(x) as.numeric(x > m))    
  is_important <- apply(compare_to_means, 2, sum) > ncol(d)/2    
  is_important <- factor(is_important, levels = c(FALSE, TRUE), labels = c('No', 'Yes'))    
  d$important_cases <- is_important    
  return(d)
}

# В R мы без труда можем рассчитать среднее и медиану вектора, а вот встроенной функции для расчета моды — наиболее часто встречаемого значения — в R нет! 
# А мода так бы пригодилась нам при анализе номинативных данных! При этом функция mode в R существует, но выполняет абсолютно другую задачу 
# (если хотите узнать, какую именно, ознакомьтесь со справкой: наберите в консоли ?mode).
# Напишите функцию stat_mode, которая получает на вход вектор из чисел произвольной длины и возвращает числовой вектор с наиболее часто встречаемым значением. 
# Если наиболее часто встречаемых значений несколько, функция должна возвращать несколько значений моды  в виде числового вектора. 
stat_mode <- function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
}

# Доктор Пилюлькин решил вооружиться статистикой, чтобы сравнить эффективность трех лекарств! Давайте поможем ему и напишем функцию max_resid, 
# которая получает на вход dataframe с двумя переменными: типом лекарства и результатом его применения. 
# Drugs - фактор с тремя градациями: drug_1, drug_2, drug_3.     
# Result - фактор с двумя градациями: positive, negative.
# Функция должна находить ячейку таблицы сопряженности с максимальным  значением стандартизированного остатка и возвращать вектор из двух элементов: название строчки и столбца этой ячейки.
# Для расчета стандартизированных остатков вы можете воспользоваться уже знакомой вам функцией chisq.test(). Изучите справку по этой функции, чтобы найти, 
# где хранятся стандартизированные остатки.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
str(test_data)
data_table <- table(test_data)
table(test_data)
rownames(table(test_data))
colnames(table(test_data))

max_resid <- function(x){
  chisq <- chisq.test(table(x))
  max_row <- rownames(which(chisq$stdres==max(chisq$stdres), arr.ind=T))
  max_col <- colnames(chisq$stdres)[which(chisq$stdres==max(chisq$stdres), arr.ind=T, useNames = F)[2]]
  return(c(max_row, max_col))
  
}
max_resid(test_data)

test_data <- as.data.frame(list(Drugs = c(2, 2, 2, 1, 3, 2, 1, 3, 2, 3, 2, 1, 2, 2, 3, 1, 2, 1, 2, 2, 1, 3, 1, 2, 1, 1, 2, 3, 2, 2, 2, 3, 1, 1, 3, 3, 3, 3, 2, 3, 2, 1, 3, 2, 2, 1, 3, 3, 1, 2, 3, 2, 2, 3, 2, 2, 1, 2, 1, 1, 2, 2, 2, 2, 1, 2, 3, 2, 3, 2, 1, 3, 3, 2, 2, 2, 3, 2, 2, 3, 2, 3, 2, 2, 3, 2, 2, 1, 1, 3, 1, 1, 2, 2, 1, 3, 2, 3, 2, 3, 1), Result = c(1, 2, 2, 1, 2, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 1, 
                                                                                                                                                                                                                                                                                                                                                                     1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1)))
max_resid(test_data)

max_resid <- function(test_data){    
  d <- table(test_data)    
  chi <- chisq.test(d)    
  ind <- which(chi$stdres==max(chi$stdres), arr.ind = T)    
  return(c(row.names(d)[ind[1]],colnames(d)[ind[2]]))    
}

# Начнем с простого и вспомним, как применять логистическую регрессию в R. Напишите функцию get_coefficients, которая получает на вход dataframe 
# с двумя переменными x ( фактор с произвольным числом градаций) и y ( фактор с двумя градациями ?). Функция строит логистическую модель, 
# где y — зависимая переменная, а x — независимая, и возвращает вектор со значением экспоненты коэффициентов модели. 
get_coefficients  <- function(dataset){    
  fit <- glm(y ~ x, dataset, family = 'binomial')    
  return(exp(fit$coefficients))    
}

# Если в нашей модели есть количественные предикторы, то в интерцепте мы будем иметь значение, соответствующее базовому уровню категориальных предикторов и нулевому уровню количественных. 
# Это не всегда осмысленно. Например, нам не интересен прогноз для людей нулевого возраста или роста. 
# В таких ситуациях количественную переменную имеет смысл предварительно центрировать так, чтобы ноль являлся средним значением переменной. 
# Самый простой способ центрировать переменную — отнять от каждого наблюдения среднее значение всех наблюдений.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
test_data
var_names = c("X4", "X2", "X1")
mean(test_data[, c("X4")])

centered <- function(test_data, var_names){
  #for(i in test_data[, c(var_names)]){
  for(i in c(var_names)){
    test_data[, c(i)] <- test_data[, c(i)] - mean(test_data[, c(i)])
  }
  return(test_data)
}
centered(test_data, var_names)

centered <- function(test_data, var_names){    
  test_data[var_names] <- sapply(test_data[var_names], function(x) x - mean(x))    
  return(test_data)    
}

# Представьте, что мы работаем в аэропорту в службе безопасности и сканируем багаж пассажиров. 
# В нашем распоряжении есть информация о результатах проверки багажа за предыдущие месяцы. Про каждую вещь мы знаем:
# являлся ли багаж запрещенным - is_prohibited (No - разрешенный, Yes - запрещенный) 
# его массу (кг) - weight
# длину (см) - length 
# ширину (см) - width 
# тип багажа (сумка или чемодан) - type.
# Напишите функцию get_features , которая получает на вход набор данных о багаже. Строит логистическую регрессию, где зависимая переменная - являлся ли багаж запрещенным, 
# а предикторы - остальные переменные,? и возвращает вектор с названиями статистически значимых переменных (p < 0.05) (в модели без взаимодействия). 
# Если в данных нет значимых предикторов, функция возвращает строку с сообщением  "Prediction makes no sense".
get_features <- function(dataset){
  fit <- glm(is_prohibited  ~ ., data = dataset, family = "binomial")
  result <- anova(fit, test = "Chisq") #тут и будет вся нужная информация!
  
  p_value_count <-0
  new_vector <- vector()
  
  result <- as.data.frame(result)
  for(i in 2:length(result$`Pr(>Chi)`)){
    if((result$`Pr(>Chi)`[i]) < 0.05){
      new_vector <- c(new_vector, rownames(result)[i])
      p_value_count <- p_value_count + 1
    }
  }
  if(p_value_count == 0){
    return("Prediction makes no sense")
  }
  else{
    return(new_vector)
  }
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
get_features(test_data)

fit <- glm(is_prohibited  ~ ., data = test_data, family = "binomial")
result <- anova(fit, test = "Chisq") #тут и будет вся нужная информация!
result$`Pr(>Chi)`[2]
length(result$`Pr(>Chi)`)
df <- as.data.frame(result)
rownames(df)[2]

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
get_features(test_data)

get_features <- function(test_data){    
  fit <- glm(is_prohibited ~., test_data, family = 'binomial')    
  result <- anova(fit, test = 'Chisq')    
  if (all(result$`Pr(>Chi)`[-1] > 0.05)){      
    return('Prediction makes no sense')}    
  return(rownames(result)[-1] [result$`Pr(>Chi)`[-1] < 0.05])  
}

# Напишите функцию, которая принимает на вход два набора данных. Первый dataframe, как и в предыдущей задаче, содержит информацию об уже осмотренном багаже 
# (запрещенный или нет, вес, длина, ширина, тип сумки). 
# Второй набор данных — это информация о новом багаже, который сканируется прямо сейчас. В данных также есть информация:  вес, длина, ширина, тип сумки и имя пассажира 
# (смотри описание переменных в примере). 
# Используя первый набор данных, обучите регрессионную модель различать запрещенный и разрешенный багаж. 
# При помощи полученной модели для каждого наблюдения в новых данных предскажите вероятность того, что багаж является запрещенным. Пассажиров, 
# чей багаж получил максимальное значение вероятности, мы попросим пройти дополнительную проверку. 
# Итого, ваша функция принимает два набора данных и возвращает имя пассажира с наиболее подозрительным багажом. Если несколько пассажиров получили максимальное значение вероятности, 
# то верните вектор с несколькими именами. 
# В этой задаче для предсказания будем использовать все предикторы, даже если некоторые из них оказались незначимыми. Для предсказания стройте модель без взаимодействия предикторов.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(test_data, data_for_predict){
  fit  <- glm(is_prohibited ~ ., test_data, family = "binomial")
  data_for_predict$prob_predict <- predict(fit, newdata = data_for_predict, type = "response")
  return(data_for_predict$passangers[which.max(data_for_predict$prob_predict)])
}

most_suspicious(test_data, data_for_predict)

# Напишите функцию normality_test, которая получает на вход dataframe с произвольным количеством переменных разных типов (количественные, строки, факторы) 
# и проверяет нормальность распределения количественных переменных. Функция должна возвращать вектор значений p-уровней значимости теста shapiro.test для каждой количественной переменной.
test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")

normality_test <- function(dataset){
  df_num <- dataset[sapply(dataset, is.numeric)]
  lshap <- lapply(df_num, shapiro.test)
  lres <- sapply(lshap, `[`, c("p.value"))
  return(unlist(lres))
}

normality_test(test)

normality_test <- function(dataset){    
  numeric_var <- sapply(dataset, is.numeric)  
  sapply(dataset[numeric_var], function(x) shapiro.test(x)$p.value)    
}

# Напишите функцию normality_by, которая принимает на вход dataframe c тремя переменными. Первая переменная количественная, 
# вторая и третья имеют две градации и разбивают наши наблюдения на группы. 
# Функция должна проверять распределение на нормальность в каждой получившейся группе и возвращать dataframe с результатами применения теста shapiro.test (формат вывода смотри ниже).
library(ggplot2)
ggplot(data = iris, aes(Sepal.Length, fill = Species))+
  xlab("Sepal.Length")+
  ylab("density")+
  geom_density(alpha = 0.2) 

obj <- ggplot(iris, aes(x=Sepal.Length,fill=Species))+    
  geom_density(alpha = 0.2)

normality_by <- function(test){
  res <- setNames(aggregate(formula = test[, 1] ~ test[, 2] + test[, 3],
                              data = test,
                              FUN = function(x) {y <- shapiro.test(x); c(y$p.value)}), 
                    c(colnames(test[2]), colnames(test[3]), "p_value"))
  return(res)
}

normality_by(mtcars[, c("mpg", "am", "vs")])

test_data <- as.data.frame(list(x = c(10, 10, 8, 7, 9, 7, 12, 9, 11, 8, 11, 14, 12, 10, 11, 11, 14, 12, 8, 9, 11, 14, 12, 9, 10, 8, 8, 11, 9, 10, 11, 12, 14, 10, 8, 8, 8, 6, 8, 8), y = c(0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0), z = c(2, 3, 2, 3, 3, 2, 2, 2, 2, 3, 3, 2, 2, 3, 3, 3, 3, 2, 2, 3, 3, 2, 2, 3, 2, 3, 2, 3, 2, 2, 2, 3, 2, 3, 2, 3, 2, 3, 3, 3)))
normality_by(test_data)

library(dplyr)    
get_p_value <- function(x){      
  shapiro.test(x)$p.value    
}    
normality_by <- function(test){    
  grouped <- test %>%    
    group_by_(.dots = colnames(.)[2:3]) %>%         
    summarise_each(funs(get_p_value))         
  names(grouped)[3] <- 'p_value'         
  return(grouped)         
}