install.packages('lme4')
install.packages('mlmRev')
install.packages('lmerTest')

library(lme4)
library(mlmRev)
library(lmerTest)

# linear mixed effect regresion
lmer(DV ~ IV + (1 + IV | RV), data = my_data)

data("Exam")

str(Exam)
help(Exam)

library(ggplot2)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point()

ggplot(data = Exam, aes(x = standLRT, y = normexam, col = school)) + 
  geom_point()


# один головний ефеке (фіксований)
Model1 <- lm(normexam ~ standLRT, data=Exam)
summary(Model1)

Exam$Model1_pred <- predict(Model1)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point() + 
  geom_line(data = Exam, aes(x = standLRT, y = Model1_pred), col = 'blue', size = 1)


# головний ефект + випадковий вільний член
Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)

Exam$Model2_pred <- predict(Model2)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model2_pred, col = school))


# головний ефект + випадковий вільний член + випадковий кутовий коефіцієнт
Model3 <- lmer(normexam ~ standLRT + (1 + standLRT|school), data=Exam)
summary(Model3)

Exam$Model3_pred <- predict(Model3)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model3_pred, col = school))


# головний ефект + випадковий кутовий коефіцієнт
Model4 <- lmer(normexam ~ standLRT + (0 + standLRT|school), data=Exam)
summary(Model4)

Exam$Model4_pred <- predict(Model4)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model4_pred, col = school))


# якщо припустити, що немає кореляції між випадковими ефектами, тобто між
# випадкоим членом і кутовим коефіцієнтом
# нескорельовані випадкові ефекти
Model5 <- lmer(normexam ~ standLRT + (1|school) + (0 + standLRT|school), data=Exam)
summary(Model5)

Exam$Model5_pred <- predict(Model5)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model5_pred, col = school))


###################################################################################

# порівняння моделей
Model2 <- lmer(normexam ~ standLRT + (1|school), REML = FALSE, data=Exam)
summary(Model2)

Model0 <- lmer(normexam ~ 1 + (1|school), REML = FALSE, data = Exam)
summary(Model0)

# f-test
anova(Model0, Model2)


# p-значення
library(lmerTest)

Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)


# узагальнені змішані можелі
Exam$school_type <- ifelse(Exam$type == 'Mxd', 1, 0)

Model5 <- glmer(school_type ~ normexam + (1|school), family = "binomial", data = Exam)
summary(Model5)


# передбачення нових даних
predict(Model2, Exam)

new_Exam <- Exam[sample(1:nrow(Exam), 100), ]
new_Exam$school <- sample(101:200)

predict(Model2, new_Exam, allow.new.levels = T)


# дослідження випадкових ефектів
fixef(Model3) # коефіцієнти фіксованих ефектів
ranef(Model3) # коефіцієнти випадкових ефектів - середнє буде 0


# Exercise 1

# Давайте воспользуемся данными исследования*, изучавшего влияние различных факторов на высоту голоса человека.
# В исследовании приняло участие 6 испытуемых, каждому испытуемому поочередно предъявлялось на экране 7 предложений. Каждое предложение (scenario) — часть небольшой социальной ситуации: 
# например, нужно было извиниться за опоздание или попросить помощи в выполнении домашнего задания. При этом каждое предложение предъявлялось в двух условиях (attitude): 
# в одном условии испытуемый должен был произнести предложение, представив, что обращается к своему другу (однокурснику), а во втором условии — к старшему (преподавателю).
# Исследователей интересовал вопрос, повлияет ли тип социальной ситуации (attitude) на высоту голоса (frequency) испытуемых, с которой они произносят просьбу. 
# Задача очень простая, но как вы уже догадались, дизайн экспериментального исследования приводит к тому, что в данных есть два источника повторных измерений: 
# испытуемый и предложение. Идеальная ситуация для смешанных регрессионных моделей!
exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
str(exp_data)

exp_data$scenario <- as.factor(exp_data$scenario)

plot_1 <- ggplot(exp_data, aes(x=scenario, y=frequency, fill=attitude)) +
  geom_boxplot()
plot_1

# Exercise 2

# Итак, действительно можно предположить, что различия в стимулах могут сказаться на наших результатах. 
# Теперь давайте посмотрим, есть ли основания полагать, что фактор испытуемого и его пола тоже может повлиять на результат.
# Визуализируйте распределение высоты голоса у испытуемых в зависимости от пола и номера испытуемого
plot_2 <- ggplot(exp_data, aes(frequency, fill=subject)) +
  geom_density(alpha = 0.3) +
  facet_grid(gender ~ .)
plot_2

# Exercise 3

# Давайте начнем работать с моделями. Для построения моделей со случайными эффектами мы будем использовать пакет lme4.
# Итак, ваша задача — построить следующую регрессионную модель: 
# зависимая переменная — frequency, 
# фиксированный эффект — attitude,
# а также добавлен случайный intercept для переменных subject и scenario.
# Сохраните модель в переменную fit_1. Данные записаны в переменной exp_data.
fit_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=exp_data)
summary(fit_1)

# Exercise 4

# Теперь добавьте пол испытуемых в модель в качестве фиксированного эффекта*. Сохраните обновленную версию модели в переменную fit_2.
# Ваша задача — построить следующую регрессионную модель:
# зависимая переменная — frequency, 
# фиксированный эффект — attitude,
# фиксированный эффект — gender,
# а также добавлен случайный intercept для переменных subject и scenario.
# * вы уже, наверное, догадались, что не существует четкой границы между фиксированными и случайными эффектами. 
# В каком-то исследовании пол может выступать как случайный эффект, однако в этом исследовании авторы включили его в список основных эффектов. 
fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=exp_data)
summary(fit_2)

# Exercise 5

# Усложним нашу модель! Давайте теперь добавим случайный коэффициент наклона к нашим двум случайным эффектам. Сохраните финальную модель в переменную fit_3. 
# Ваша задача — построить следующую регрессионную модель:
# зависимая переменная — frequency, 
# фиксированный эффект — attitude,
# фиксированный эффект — gender,
# а также добавлен случайный intercept и slope для переменных subject и scenario. Теперь, когда в модели два фиксированных эффекта, мы можем по разному записать случайные эффекты. В этом исследовании нас интересует случайный эффект для социальной ситуации (attitude).
fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude|subject) + (1 + attitude|scenario), data=exp_data)
summary(fit_3)
