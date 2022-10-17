# > Анализ текста
msg <- readLines('putin-2018.txt') # files -> [choose folder] -> Set as working derictory
msg

# подсчитает кол-во символов
length(msg) # кол-во строк
l <- lapply(msg, nchar)
Reduce(sum,l) # кол-во символов

txt <- paste(msg, collapse = " ")
nchar(txt)

# ищем строки с "мы"
r <- grep(' мы ', msg, ignore.case = TRUE) # ignore.case - игнорировать регистр
r
msg[r]

r <- grep(' мы/.', msg) # find
r
msg[r]

# частота встречания слов
words <- unlist(strsplit(txt, ' '))
wc <- table(words) # wc - word count
wc <- sort(wc, decreasing = TRUE) # сортируем по частоте встречания
head(wc,20)
