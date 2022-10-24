#Устанвоить пакеты
install.packages("tidyverse")
#install.packages("dplyr")
library(dplyr)

#Загрузим тестовый набор данных
data(starwars)

#Прочитайте описание массива данных
?starwars

#узнайте количество строк и колонок в наборе с помощью функций nrow и summary

starwars %>% nrow # кол-во строк 
starwars %>% ncol # кол-во  колонок
starwars %>% summary

###########
#СПРАВКА https://dplyr.tidyverse.org/reference/
##########

#С помощью функции distinct выберите уникальные цвета волос
starwars %>% distinct(hair_color)

#Сгруппируйте по цвету волос и посчитайте сколько всего строк каждого цвета
starwars %>%
  group_by(hair_color) %>%
  summarise(n())

#Отсортируйте по убыванию то что получили выше
starwars %>%
  group_by(hair_color) %>%
  summarise(sum = n()) %>%
  arrange(desc(sum))

#Посчитайте среднюю массу всех представителей
m <- starwars %>%
  filter(!is.na(mass))
mean(m$mass)

#Теперь найдите самого высокого, самого низкого
starwars %>%
  arrange(desc(height)) %>%
  slice(1) # самый высокий
starwars %>%
  arrange(height) %>%
  slice(1) # самый низкий

#Отфильтруйте их и снова посчитайте среднюю масса
m1 <- starwars %>%
  filter(name != 'Yoda', name != 'Yarael Poof', !is.na(mass)) %>%
  arrange(desc(height))
mean(m1$mass)

#Найдите средний рост жителя каждой из планет
starwars %>%
  filter(!is.na(height)) %>%
  group_by(homeworld) %>%
  summarise(mean(height))
