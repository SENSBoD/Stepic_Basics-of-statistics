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


# ���� �������� ����� (����������)
Model1 <- lm(normexam ~ standLRT, data=Exam)
summary(Model1)

Exam$Model1_pred <- predict(Model1)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point() + 
  geom_line(data = Exam, aes(x = standLRT, y = Model1_pred), col = 'blue', size = 1)


# �������� ����� + ���������� ������ ����
Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)

Exam$Model2_pred <- predict(Model2)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model2_pred, col = school))


# �������� ����� + ���������� ������ ���� + ���������� ������� ����������
Model3 <- lmer(normexam ~ standLRT + (1 + standLRT|school), data=Exam)
summary(Model3)

Exam$Model3_pred <- predict(Model3)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model3_pred, col = school))


# �������� ����� + ���������� ������� ����������
Model4 <- lmer(normexam ~ standLRT + (0 + standLRT|school), data=Exam)
summary(Model4)

Exam$Model4_pred <- predict(Model4)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model4_pred, col = school))


# ���� ����������, �� ���� ��������� �� ����������� ��������, ����� ��
# ��������� ������ � ������� ������������
# �������������� �������� ������
Model5 <- lmer(normexam ~ standLRT + (1|school) + (0 + standLRT|school), data=Exam)
summary(Model5)

Exam$Model5_pred <- predict(Model5)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model5_pred, col = school))


###################################################################################

# ��������� �������
Model2 <- lmer(normexam ~ standLRT + (1|school), REML = FALSE, data=Exam)
summary(Model2)

Model0 <- lmer(normexam ~ 1 + (1|school), REML = FALSE, data = Exam)
summary(Model0)

# f-test
anova(Model0, Model2)


# p-��������
library(lmerTest)

Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)


# ����������� ������ �����
Exam$school_type <- ifelse(Exam$type == 'Mxd', 1, 0)

Model5 <- glmer(school_type ~ normexam + (1|school), family = "binomial", data = Exam)
summary(Model5)


# ������������ ����� �����
predict(Model2, Exam)

new_Exam <- Exam[sample(1:nrow(Exam), 100), ]
new_Exam$school <- sample(101:200)

predict(Model2, new_Exam, allow.new.levels = T)


# ���������� ���������� ������
fixef(Model3) # ����������� ���������� ������
ranef(Model3) # ����������� ���������� ������ - ������� ���� 0


# Exercise 1

# ������� ������������� ������� ������������*, ���������� ������� ��������� �������� �� ������ ������ ��������.
# � ������������ ������� ������� 6 ����������, ������� ����������� ���������� ������������� �� ������ 7 �����������. ������ ����������� (scenario) � ����� ��������� ���������� ��������: 
# ��������, ����� ���� ���������� �� ��������� ��� ��������� ������ � ���������� ��������� �������. ��� ���� ������ ����������� ������������� � ���� �������� (attitude): 
# � ����� ������� ���������� ������ ��� ���������� �����������, ����������, ��� ���������� � ������ ����� (������������), � �� ������ ������� � � �������� (�������������).
# �������������� ����������� ������, �������� �� ��� ���������� �������� (attitude) �� ������ ������ (frequency) ����������, � ������� ��� ���������� �������. 
# ������ ����� �������, �� ��� �� ��� ����������, ������ ������������������ ������������ �������� � ����, ��� � ������ ���� ��� ��������� ��������� ���������: 
# ���������� � �����������. ��������� �������� ��� ��������� ������������� �������!
exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
str(exp_data)

exp_data$scenario <- as.factor(exp_data$scenario)

plot_1 <- ggplot(exp_data, aes(x=scenario, y=frequency, fill=attitude)) +
  geom_boxplot()
plot_1

# Exercise 2

# ����, ������������� ����� ������������, ��� �������� � �������� ����� ��������� �� ����� �����������. 
# ������ ������� ���������, ���� �� ��������� ��������, ��� ������ ����������� � ��� ���� ���� ����� �������� �� ���������.
# �������������� ������������� ������ ������ � ���������� � ����������� �� ���� � ������ �����������
plot_2 <- ggplot(exp_data, aes(frequency, fill=subject)) +
  geom_density(alpha = 0.3) +
  facet_grid(gender ~ .)
plot_2

# Exercise 3

# ������� ������ �������� � ��������. ��� ���������� ������� �� ���������� ��������� �� ����� ������������ ����� lme4.
# ����, ���� ������ � ��������� ��������� ������������� ������: 
# ��������� ���������� � frequency, 
# ������������� ������ � attitude,
# � ����� �������� ��������� intercept ��� ���������� subject � scenario.
# ��������� ������ � ���������� fit_1. ������ �������� � ���������� exp_data.
fit_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=exp_data)
summary(fit_1)

# Exercise 4

# ������ �������� ��� ���������� � ������ � �������� �������������� �������*. ��������� ����������� ������ ������ � ���������� fit_2.
# ���� ������ � ��������� ��������� ������������� ������:
# ��������� ���������� � frequency, 
# ������������� ������ � attitude,
# ������������� ������ � gender,
# � ����� �������� ��������� intercept ��� ���������� subject � scenario.
# * �� ���, ��������, ����������, ��� �� ���������� ������ ������� ����� �������������� � ���������� ���������. 
# � �����-�� ������������ ��� ����� ��������� ��� ��������� ������, ������ � ���� ������������ ������ �������� ��� � ������ �������� ��������. 
fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=exp_data)
summary(fit_2)

# Exercise 5

# �������� ���� ������! ������� ������ ������� ��������� ����������� ������� � ����� ���� ��������� ��������. ��������� ��������� ������ � ���������� fit_3. 
# ���� ������ � ��������� ��������� ������������� ������:
# ��������� ���������� � frequency, 
# ������������� ������ � attitude,
# ������������� ������ � gender,
# � ����� �������� ��������� intercept � slope ��� ���������� subject � scenario. ������, ����� � ������ ��� ������������� �������, �� ����� �� ������� �������� ��������� �������. � ���� ������������ ��� ���������� ��������� ������ ��� ���������� �������� (attitude).
fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude|subject) + (1 + attitude|scenario), data=exp_data)
summary(fit_3)
