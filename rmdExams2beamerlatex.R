RmdExams2beamerlatex <- function(path2RmdFilesFolder) {
  
  library(readr)
  counter <- 1
  path <- paste(path2RmdFilesFolder, "/", "exam.txt", sep = "")
  file.create(path)
  
  # Create lists
  questions_list <- list()
  questions_right <- list()
  questions_wrong <- list()
  
  # Prepare the document
  write('\\documentclass[t]{beamer}\n', path, append = TRUE)
  write('\\usetheme{Boadilla} \n \\usecolortheme{seahorse} \n', path, append = TRUE)
  write('\\setbeamertemplate{footline}[frame number]{} \n \\setbeamertemplate{navigation symbols}{} \n \\setbeamertemplate{footline}{}', path, append = TRUE)
  write('\\usepackage{cmap} \n', path, append = TRUE)
  write('\\usepackage{mathtext} \n \\usepackage{booktabs} \n', path, append = TRUE)
  write('\\usepackage{amsmath,amsfonts,amssymb,amsthm,mathtools}', path, append = TRUE)
  write('\\usepackage[T2A]{fontenc} \n', path, append = TRUE)
  write('\\usepackage[utf8]{inputenc} \n', path, append = TRUE)
  write('\\usepackage[english,russian]{babel} \n', path, append = TRUE)
  write('\\usetheme{Boadilla} \n \\usecolortheme{seahorse}')
  write('\\DeclareMathOperator{\\Lin}{\\mathrm{Lin}} \n \\DeclareMathOperator{\\Linp}{\\Lin^{\\perp}} \n \\DeclareMathOperator*\\plim{plim}', path, append = TRUE)
  write('\n \\DeclareMathOperator{\\grad}{grad} \n \\DeclareMathOperator{\\card}{card} \n \\DeclareMathOperator{\\sgn}{sign} \n \\DeclareMathOperator{\\sign}{sign} \n \\DeclareMathOperator*{\\argmin}{arg\\,min} \n \\DeclareMathOperator*{\\argmax}{arg\\,max} \n \\DeclareMathOperator*{\\amn}{arg\\,min} \n \\DeclareMathOperator*{\\amx}{arg\\,max} \n \\DeclareMathOperator{\\cov}{Cov} \n', path, append = TRUE)
  write('\\DeclareMathOperator{\\Var}{Var} \n \\DeclareMathOperator{\\Cov}{Cov} \n \\DeclareMathOperator{\\Corr}{Corr} \n \\DeclareMathOperator{\\E}{\\mathbb{E}} \n \\let\\P\\relax \n', path, append = TRUE)
  write('\\DeclareMathOperator{\\P}{\\mathbb{P}} \n \\newcommand{\\cN}{\\mathcal{N}} \n \\def \\R{\\mathbb{R}} \n \\def \\N{\\mathbb{N}} \n \\def \\Z{\\mathbb{Z}} \n', path, append = TRUE)
  write('\\title{Midterm 2018} \n \\subtitle{Теория вероятностей и математическая статистика} \n \\author{Обратная связь: \\url{https://github.com/bdemeshev/probability_hse_exams}} \n \\date{Последнее обновление: \\today}', path, append = TRUE)
  write('\\begin{document} \n', path, append = TRUE)
  write('\\frame[plain]{\\titlepage}', path, append = TRUE)
  
  n_files <- length(list.files(path2RmdFilesFolder))
  
  while (counter <= n_files - 1) {
    
    file <- read_file(paste(path2RmdFilesFolder, "/", counter, ".Rmd", sep = ""))
    
    # Split each question by '========' and take the following parts:
    # 2 -- task text and answer options
    # 5 -- meta-data.
    file <- strsplit(file, "========")[[1]]
    task <- file[2]
    answers_codes <- file[5]
    
    # Split the task by '----------' to get task text and answer options.
    answers <- strsplit(task, "----------")[[1]][2]
    task <- strsplit(task, "----------")[[1]][1]
    
    # Clear out what is not needed.
    task <- strsplit(task, "Answerlist")[[1]][1]
    
    # Put the answer options into the list.
    answers <- strsplit(answers, "\\*")[[1]][2:6]
    
    if (is.na(answers[[5]]) == TRUE) {
      answers <- answers[-c(5)]
    }
    
    # Clear the answer options.
    answers_clear <- list()
    index <- 1
    
    for (element in answers) {
      answers_clear[[index]] <- substr(element, 3, (nchar(element) - 1))
      index <- index + 1
    }
    
    index <- length(answers_clear)
    
    answers_clear[[index]] <- strsplit(answers_clear[[index]], "Solution")[[1]][1]
    answers_clear[[index]] <- substr(answers_clear[[index]], 1, 
                                     (nchar(answers_clear[[index]]) - 2))
    
    # Get which answer options is correct and which are not.
    answers_codes <- strsplit(answers_codes, "\n")[[1]][4]
    answers_codes <- strsplit(answers_codes, " ")[[1]][2]
    answers_codes <- strsplit(answers_codes, "")[[1]]
    
    # Format the answer options to obtain the LaTex formatting.
    keeper <- list()
    answer_string <- ""
    code_index <- 1
    
    for (element in answers_clear) {
      if (answers_codes[[code_index]] == "0") {
        keeper[[code_index]] <- paste(answer_string, '\\item[] \\hyperlink{', counter, '-No}{\\beamergotobutton{} ', element, '}', "\n", sep = "")
      } else {
        keeper[[code_index]] <- paste(answer_string, '\\item[] \\hyperlink{', counter, '-Yes}{\\beamergotobutton{} ', element, '}', "\n", sep = "")
      }
      code_index <- code_index + 1
    }
    
    keeper <- sample(keeper)
    
    for (element in keeper) {
      answer_string <- paste(answer_string, element, sep = "")
    }
    
    
    # Prepare the question by collecting all the parts.
    question <- paste("\n \\begin{frame} \\label{", 
                      counter, "} \n", '\\begin{block}{', counter, '} \n',  task, '\n \\end{block} \n', '\\begin{enumerate} \n', answer_string, '\\end{enumerate} \n', "\\end{frame} \n", sep = "")
    
    question_wrong <- paste("\n \\begin{frame} \\label{", 
                      counter, "-No} \n", '\\begin{block}{', counter, '} \n',  task, '\n \\end{block} \n', '\\begin{enumerate} \n', answer_string, '\\end{enumerate} \n', '\n \\alert{Нет!} \n', "\\end{frame} \n", sep = "")
    
    question_right <- paste("\n \\begin{frame} \\label{", 
                            counter, "-Yes} \n", '\\begin{block}{', counter, '} \n',  task, '\n \\end{block} \n', '\\begin{enumerate} \n', answer_string, '\\end{enumerate} \n', '\n \\textbf{Да!} \n \\hyperlink{', (counter + 1), '}{\\beamerbutton{Следующий вопрос}}', "\\end{frame} \n", sep = "")
    
    questions_list[[counter]] <- question
    questions_right[[counter]] <- question_right
    questions_wrong[[counter]] <- question_wrong
    
    counter <- counter + 1
  }
  
  # Update the text file.
  for (element in questions_list) {
    write(element, path, append = TRUE)
  }
  
  for (element in questions_right) {
    write(element, path, append = TRUE)
  }
  
  for (element in questions_wrong) {
    write(element, path, append = TRUE)
  }
  
  # End the document
  write('\\end{document}', path, append = TRUE)
}

