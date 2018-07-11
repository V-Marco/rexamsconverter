setwd("~/Documents/RStudio")

# Файл.
to_read_name <- '2015_final.txt'
directory_name <- substr(to_read_name, 1, (nchar(to_read_name) - 4))

library(readr)
file <- read_file(to_read_name)

# Будем хранить результаты в папке.
dir.create(directory_name)

# Так как в файлах, кроме вопросов, могут быть команды LaTex для оформления 
# (типа ruler), и все блоки начинаются с \element, выбираем только те блоки 
# \element, в которых есть \begin{choices}, то есть список ответов -- он есть
# только в вопросах. Отдельно сохраняем текстовые вставки между заданиями --
# те элементы. где есть retcommontext.

splits <- strsplit(file, 'element')[[1]]

splits_true <- list()
splits_true_index <- 1

inter_splits <- list()
inter_splits_index <- 1

for (element in splits) {
  if (grepl("choices", element) == TRUE) {
    splits_true[[splits_true_index]] <- element
    splits_true_index <- splits_true_index + 1
  }
  
  if (grepl("commontext", element) == TRUE) {
    inter_splits[[inter_splits_index]] <- element
    inter_splits_index <- inter_splits_index + 1
  }
}

# Обработаем текстовые вставки, чтобы понять, в какие вопросы их добавить.
# Идея: делим текст по textbf и берём вторую часть -- это текст задания. Затем
# находим индекс первой "}" -- это конец блока {В вопросах 00-11}. Выделяем всё,
# что идёт после в текст задания, а из блока извлекаем все номера заданий в формате
# с(00, 01, ... 11).

# Номера заданий и соответсвующие им тексты будем хранить в двух списках.

inter_tasks_texts <- list()
inter_tasks_numbers <- list()
inter_tasks_index <- 1

for (element in inter_splits) {
  element = element[[1]]
  meaning_part <- strsplit(element, "textbf")[[1]][2]
  indexes_inter <- which(strsplit(meaning_part, "")[[1]] == "}")
  task_inter <- substr(meaning_part, indexes_inter[1] + 2, indexes_inter[length(indexes_inter) - 1])
  task_inter <- paste(toupper(substr(task_inter, 1, 1)), substr(task_inter, 2, nchar(task_inter)), sep = "")
  
  task_numbers <- substr(meaning_part, 1, indexes_inter[1])
  task_numbers <- gsub("[{}]","",strsplit(task_numbers,"\\}\\{")[[1]])
  task_numbers <- strsplit(task_numbers, " ")[[1]]
  task_numbers <- rev(task_numbers)[1]
  task_numbers <- strsplit(task_numbers, "-")[[1]]
  task_numbers <- strtoi(task_numbers)
  task_numbers <- c(task_numbers[[1]]:task_numbers[[2]])
  
  for (number in task_numbers) {
    inter_tasks_numbers[[inter_tasks_index]] <- number
    inter_tasks_texts[[inter_tasks_index]] <- task_inter
    inter_tasks_index <- inter_tasks_index + 1
  }
}

# Далее идём по списку вопросов, получая текст задания и список ответов.

counter <- 1
splits <- splits_true

while (counter <= length(splits)) {
  
  one <- splits[[counter]]

# Ловим текст задания. Идея: разбить вопрос по символам "}".
# Можно посчитать, что в большинстве вопросов текст задания находится между 3 с 
# начала и (12 с конца - 17) символами "{". Там, где это не так, в текст попадает
# часть begin{multicols}. Разделяем текст по ней и берём первую часть -- чистый
# текст задания.
  
# Проблема с этим подходом: иногда AMCMultiNoChoice стоит за пределами блока
# \begin{questionmult}, поэтому она будет потеряна.

  indexes <- which(strsplit(one, "")[[1]] == "}")
  indexes_rev <- rev(indexes)
  
  task <- substr(one, indexes[3] + 1, indexes_rev[12] - 17)
  
  if (grepl("begin\\{multicols\\}", task) == TRUE) {
    task <- strsplit(task, "begin\\{multicols\\}")[[1]][1]
  }

# Иногда какие-то символы в конце текста задания воспринимаются редактором слитно
# с другими и не удаляются. Видимо, придётся разбивать их вручную.
  
  if (substr(task, nchar(task) - 2, nchar(task)) == "\\be") {
    task <- substr(task, 1, nchar(task) - 2)
  }
  
  if (substr(task, nchar(task), nchar(task)) == "\\") {
    task <- substr(task, 1, nchar(task) - 1)
  }
  
  if (substr(task, nchar(task), nchar(task)) == "%") {
    task <- substr(task, 1, nchar(task) - 1)
  }

# Убираем комментарии Латеха в первой строчке (всё, что между % и \n).
  
  if (grepl('%', task) == TRUE) {
    index <- which(strsplit(task, '')[[1]] == '\n')[1]
    task <- substr(task, index + 1, nchar(task))
  }
  
# Соединяем задание с промежуточной информацией, если таковая имеется.
  
  if (counter %in% inter_tasks_numbers == TRUE) {
    index <- which(inter_tasks_numbers == counter)
    prom <- inter_tasks_texts[[index]]
    
    task <- paste(prom, '\n', '\n', task)
  }
  
# Если первый символ -- '\n', удаляем его.
  
  if (substr(task, 1, 1) == '\n') {
    task <- substr(task, 2, nchar(task))
  }
  
# Ловим варианты ответа. Идея: разбиваем вопрос по "choices}", ибо после этой строки
# всегда идёт перечисление вариантов ответа. Затем разбиваем по \n и находим индексы
# элементов, содержащих "choices". Затем собираем варианты ответа как строки,
# стоящие между этими индексами.

  choices <- strsplit(one, "choices\\}")[[1]][2]
  choices <- strsplit(choices, '\n')[[1]]
  
  ch_indexes <- which(grepl('choice', choices))
  ch_indexes[length(ch_indexes) + 1] <- length(choices)
  prom_choices <- list()
  prom_choices_index <- 1
  
  while (prom_choices_index <= length(ch_indexes) - 1) {
    
    str_prom <- ""
    prom_list <- choices[(ch_indexes[prom_choices_index]):(ch_indexes[prom_choices_index + 1] - 1)]
    
    for (element in prom_list) {
      str_prom <- paste(str_prom, element, sep = '\n')
    }
    
    prom_choices[[prom_choices_index]] <- str_prom
    
    prom_choices_index <- prom_choices_index + 1
  }
  
choices <- prom_choices

# Сохраняем сами тексты ответов.
  text_choices <- list()
  text_index = 1
# Сохраняем ответы в формате "10000", чтобы вставить в мета-данные.
  answer_string <- ""

# Идём по каждому варианту, добавляем текст вопроса в список, забирая его из
# фигурных скобок. Также соответственно изменяем answer_string, реагируя на
# correctchoice и wrongchoice.

  for (element in choices) {
    if (grepl('correctchoice', element) == TRUE) {
      a <- strsplit(element, 'correctchoice')[[1]][2]
      a <- gsub("[{}]","",strsplit(a,"\\}\\{")[[1]])
      text_choices[[text_index]] <- a
      text_index <- text_index + 1
      answer_string <- paste(answer_string, "1", sep = '')
    } else {
      a <- strsplit(element, 'wrongchoice')[[1]][2]
      a <- gsub("[{}]","",strsplit(a,"\\}\\{")[[1]])
      text_choices[[text_index]] <- a
      text_index <- text_index + 1
      answer_string <- paste(answer_string, "0", sep = '')
    }
  }

# Создаём новый файл Rmd с нужной разметкой.

  name <- paste(directory_name, "/", toString(counter), '.Rmd', sep = "")
  
  file.create(name)
  write('Question', name)
  write('========', name, append = TRUE)
  write(paste(task, "\n"), name, append = TRUE)
  write('Answerlist', name, append = TRUE)
  write('----------', name, append = TRUE)
  
  for (element in text_choices) {
    element <- element[1]
    write(paste('* ', element), name, append = TRUE)
  }
  
  write('\nSolution', name, append = TRUE)
  write('========', name, append = TRUE)
  write('\nAnswerlist', name, append = TRUE)
  write('----------', name, append = TRUE)
  
  for (element in strsplit(answer_string, "")[[1]]) {
    if (element == '0') {
    write(paste('* ', 'Неверно'), name, append = TRUE)
    } else {
    write(paste('* ', 'Отлично'), name, append = TRUE)
    }
  }
  
  write('\nMeta-information', name, append = TRUE)
  write('================', name, append = TRUE)
  write(paste('exname: ', toString(counter)), name, append = TRUE)
  write('extype: schoice', name, append = TRUE)
  write(paste('exsolution', answer_string), name, append = TRUE)
  write('exshuffle: 5', name, append = TRUE)
  
  counter = counter + 1
}
