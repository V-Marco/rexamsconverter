# Рабочей должна стать папка, в которой лежат файлы Rmd.
setwd("~/Documents/RStudio/2015_final")
library(readr)
counter <- 1
n_files <- length(list.files("."))
name <- 'exam.txt'
file.create(name)

while (counter <= n_files - 1) {

file <- read_file(paste(counter, ".Rmd", sep = ""))

# Разделим по смысловым частям: ========
# 2 -- текст задания + ответы.
# 3 -- текстовые пояснения к ответам.
# 5 -- мета-информация.

file <- strsplit(file, '========')[[1]]
task <- file[2]
answers_codes <- file[5]

# Разделим задание и ответы.

answers <- strsplit(task, '----------')[[1]][2]
task <- strsplit(task, '----------')[[1]][1]

# Очистим от ненужного.

task <- strsplit(task, 'Answerlist')[[1]][1]

# Сохраним ответы в список.
answers <- strsplit(answers, '\\*')[[1]][2:6]

if (is.na(answers[[5]]) == TRUE) {
  answers <- answers[-c(5)]
}

# Очистим ответы.
answers_clear <- list()
index <- 1

for (element in answers) {
  answers_clear[[index]] <- substr(element, 3, (nchar(element) - 1))
  index <- index + 1
}

index <- length(answers_clear)

answers_clear[[index]] <- strsplit(answers_clear[[index]], 'Solution')[[1]][1]
answers_clear[[index]] <- substr(answers_clear[[index]], 1, 
                          (nchar(answers_clear[[index]]) - 2))

# Получим, какие ответы правильные, какие -- нет.

answers_codes <- strsplit(answers_codes, '\n')[[1]][4]
answers_codes <- strsplit(answers_codes, " ")[[1]][2]
answers_codes <- strsplit(answers_codes, "")[[1]]

# Составим правильный перечень вопросов в формате Латеха.

answer_string <- "\\begin{multicols}{3} \n \\begin{choices} \n"
code_index <- 1

for (element in answers_clear) {
  if (answers_codes[[code_index]] == "0") {
    answer_string <- paste(answer_string, "\\", "wrongchoice{", element, '} \n', sep = "")
  } else {
    answer_string <- paste(answer_string, "\\", "correctchoice{", element, '} \n', sep = "")
  }
  code_index <- code_index + 1
}

answer_string <- paste(answer_string, '\\end{multicols} \n')

# Соберём вопрос.

question <- paste('\\element{prob_one_sample}{ \n \\begin{questionmult}{', 
            counter, '} \n', task, answer_string, '\\end{questionmult} \n } \n\n', sep = '')

# Обновим текстовый файл.
write(question, name, append = TRUE)

counter <- counter + 1
}
