#Дисперсионный анализ. Пример

#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
data = read.csv("data/diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#data/Diet_data_description.docx
#data/diet.csv
colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)


#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

#попарные различия между средними значениями для всех групп
TukeyHSD(fit)

#Tukey honest significant differences test)
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")




#Задание
#Добавить проверку на выбросы и избавиться от них

boxplot(data$initial.weight,data=data,col="light gray") # выплеск в 103 кг
boxplot(data$final.weight,data=data,col="light gray") # выплеск в 103 кг
## удаляем выплеск в 103 кг
data.noout <- data[data$initial.weight < 100,]
boxplot(data.noout$initial.weight,data=data,col="light gray")
boxplot(data.noout$final.weight,data=data,col="light gray")


boxplot(weight.loss~diet.type,data=data.noout,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type") # два выплеска в А
## удаляем выплески
data.noout <- data.noout[(data.noout$diet.type == "A" & data.noout$weight.loss < 7) | data.noout$diet.type == "B" |  data.noout$diet.type == "C",]
boxplot(weight.loss~diet.type,data=data.noout,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type") 




#повторно проверсти все тесты и сравнить результаты с выбросами и без

##Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data.noout,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")
### >>> различий нет (кроме выплесков)

##проверим сбалансированные ли данные
table(data.noout$diet.type)
### >>> данные стали менее сбалансированными

##График групповых средних
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=data.noout)
aggregate(data.noout$weight.loss, by = list(data.noout$diet.type), FUN=sd)
### >>> график групповых средних изменился,
### >>> так как среднее в диете А упало и стало ниже среднего В
### >>> Стандартное отклонение для А выросло на ~0.7


##Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
##тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data.noout)
summary(fit)
### >>> Значимость теста на межгрупповые различия стала лучше 
### >>> (новое 0.00057 против старого 0.00323)

##попарные различия между средними значениями для всех групп
TukeyHSD(fit)
### >>> значительно увеличилось различие между А и С (на 0.4)

##Tukey honest significant differences test)
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
### >>> различий нет (кроме выплесков)



#Открыть документ https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#и попытаться выполнить задания из него

## 1.	Paired t-test
## Ignore diet and test to see if weight has been lost

### Создадим датафрэйм
my_data <- data.frame( 
  group = rep(c("before", "after"), each = length(data.noout$initial.weight)),
  weight = c(data.noout$initial.weight,  data.noout$final.weight))
print(my_data)

### Групповая статистика
library("dplyr")
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

### Визуализация
boxplot(weight~group,data=my_data,col="light gray",
        ylab = "Weight", xlab = "Before_after")
plot(weight~group,data=my_data,col="light gray",
        ylab = "Weight", xlab = "Before_after")

### t-test
t.test (my_data$weight[my_data$group == "before"],
        my_data$weight[my_data$group == "after"]) 
### >>> p-value < 0.05, следовательно делаем вывод, что средний вес
### >>> испытуемых перед диетой значительно отличается от среднего веса
### >>> после диеты


## 2.	Compute variable	
## Remove weight lost and get students to calculate it
## using before/after weights
data.noout$weight.loss = data.noout$initial.weight - data.noout$final.weight
data.noout$weight.loss

        
## 3.	Summary statistics
## Summary statistics by diet
summary(data.noout)


## 4.	One way ANOVA
## Which diet was best for losing weight?
## Are there gender differences for weight lost?
fit <- aov(weight.loss ~ diet.type, data=data.noout)
summary(fit)
TukeyHSD(fit)
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
### >>> лучше всех оказалась диета С
fit <- aov(weight.loss ~ gender, data=data.noout)
summary(fit)
plot(weight.loss ~ gender, data=data.noout)
### >>> зависимости между гендором и потерей веса нет


## 5.	Two-way ANOVA
## Effect of diet and gender on weight lost
fit <- aov(weight.loss ~ diet.type * gender, data=data.noout)
summary(fit)
plot(weight.loss ~ diet.type * gender, data=data.noout)


## 6.	Interactions
## Means plot of weight lost by diet and gender
plotmeans(weight.loss ~ diet.type, data=data.noout)
plotmeans(weight.loss ~ gender, data=data.noout)

## 7.	ANCOVA
## Add height to either ANOVA
fit <- aov(weight.loss ~ diet.type * gender * height, data=data.noout)
summary(fit)
