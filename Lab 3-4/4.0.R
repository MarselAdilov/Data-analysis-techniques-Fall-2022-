#Пользуясь примером из лекции файл (6.0.R) проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height",
                "whole_weight", "shucked_weight",
                "viscera_weight", "shell_weight", "rings")

colnames(data)
data$sex <- as.factor(data$sex)
par(mfrow=c(1,3))
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")
#Видим ассиметрию https://en.wikipedia.org/wiki/Skewness
#и выбросы (от них нужно избавиться)
boxplot(data$diameter)
abline(h=0.155,col="red")
boxplot(data$whole_weight)
abline(h=2.15,col="red")
boxplot(data$height)
abline(h=0.04,col="red")
abline(h=0.24,col="red")

#Найти строки с выбросами и убрать их
data.noout <- data
data.noout <- data.noout[data.noout$diameter > 0.155,]
boxplot(data.noout$diameter)
data.noout <- data.noout[data.noout$whole_weight < 2.15,]
boxplot(data.noout$whole_weight)
data.noout <- data.noout[data.noout$height < 0.24 & data.noout$height > 0.04,]
boxplot(data.noout$height)

#Визулизируем возможные зависимости
par(mfrow=c(1,2))
plot(data.noout$diameter, data.noout$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(data.noout$height, data.noout$whole_weight,'p',main = "Зависимость веса от высоты")
plot(data.noout$length, data.noout$diameter,'p',main = "Зависимость длины от диаметра")

#Хорошо видна зависимость, нужно её исследовать
#построить линейные модели при помощи функции lm, посмотреть их характеристики
linear.model.1 <- lm(data$length ~ data$diameter)
summary(linear.model.1)
plot(linear.model.1)
#избавиться от выборосов, построить ещё модели и проверить их
# уже создан новый датафрейм без выбросов data.noout
linear.model.1 <- lm(data.noout$length ~ data.noout$diameter)
summary(linear.model.1)
plot(linear.model.1)

#разделить массив данных на 2 случайные части
set.seed(123)
library(caTools)
split = sample.split(data.noout$diameter, SplitRatio = 0.8)
training_set = subset(data.noout, split==TRUE)
test_set = subset(data.noout, split==FALSE)

#подогнать модель по первой части
regressor = lm(formula = diameter ~ length, data = training_set)
summary(regressor)

#спрогнозировать (функция predict) значения во второй части
par(mfrow=c(1,1))
y_pred_training = predict(regressor, newdata=training_set)
plot(x=training_set$length,y=training_set$diameter, col="red")
lines(x=training_set$length,y=y_pred_training,col="blue")

#проверить качество прогноза
y_pred_test = predict(regressor, newdata=test_set)
plot(x=test_set$length,y=test_set$diameter, col="red")
lines(x=test_set$length,y=y_pred_test,col="blue")




