library(MASS)
library(dplyr)
data(mtcars)
summary(mtcars)

help(mtcars)

colnames(mtcars)

colnames(mtcars) <- c("miles.per.galon", "cylinders",
                       "displacement", "gross.horsepower",
                       "rear.axle.ratio", "weight",
                       "quater.mile.sec", "engine",
                       "transmission", "gears", "carburetors")

mtcars$cylinders <- factor(mtcars$cylinders)
mtcars$engine <- factor(c("V-shaped", "straight")[mtcars$engine + 1])
mtcars$transmission <- factor(c("automatic", "manual")[mtcars$transmission + 1])
mtcars$gears <- factor(mtcars$gears)
mtcars$carburetors <- factor(mtcars$carburetors)

summary(mtcars)

plot (mtcars$cylinders)
title (main = "Количество цилиндров в двигателе")

plot (mtcars$miles.per.galon)
title (main = "Количество миль на галлон топлива")

plot (sort(mtcars$miles.per.galon))
title (main = "(Отсортировано) Количество миль на галлон топлива")
  
plot (mtcars$cylinders, mtcars$miles.per.galon)
title (main = "Кол-во миль на галлон в разрезе количества цилиндров")

plot (mtcars$transmission, mtcars$miles.per.galon)
title (main = "Кол-во миль на галлон в разрезе типа коробки передач")

plot (mtcars$miles.per.galon, mtcars$gross.horsepower,
      main="Кол-во миль на галлон в разрезе мощности двигателя",
      ylab = "Мощность (л.с.)", xlab="Миль")

t.test (mtcars$miles.per.galon[mtcars$transmission == "automatic"],
        mtcars$miles.per.galon[mtcars$transmission == "manual"]) # p-value should be < 0.05

linear.model.1 <- lm (miles.per.galon ~ transmission, data=mtcars) # зависимость кол-ва миль на галлон от типа трансмиссии
linear.model.1
summary(linear.model.1)

linear.model.2 <- lm (miles.per.galon ~ cylinders, data=mtcars) # зависимость кол-ва миль на галлон от количества цилиндров
linear.model.2
summary(linear.model.1)

plot(linear.model.2)

linear.model.3 <- lm (miles.per.galon ~ gross.horsepower, data=mtcars) # зависимость кол-ва миль на галлон от мощности
linear.model.3
summary(linear.model.3)
plot(linear.model.3)

linear.model.3a <- lm (miles.per.galon ~ + gross.horsepower + displacement, data=mtcars)
summary(linear.model.3a)
coef(linear.model.3a)

plot(linear.model.3a)

linear.model.4 <- lm (miles.per.galon ~ ., data=mtcars)
linear.model.4

linear.model.4a <- lm (miles.per.galon ~ . - weight, data=mtcars)
summary(linear.model.4a)
plot(linear.model.4a)

glm.0 <- glm (weight ~ . - miles.per.galon, data=mtcars)
plot(glm.0)

odds <- seq(1, nrow(mtcars), by=2)
mtcars.in <- mtcars[odds,]
mtcars.out <- mtcars[-odds,]

linear.model.half <- lm (miles.per.galon ~ . - weight, data=mtcars.in)
summary (linear.model.half)

mtcars.predict <- predict (linear.model.half)
cor (mtcars.in$miles.per.galon, mtcars.predict)
plot (mtcars.in$miles.per.galon, mtcars.predict)

