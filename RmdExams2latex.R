RmdExams2latex <- function(path2RmdFilesFolder) {
  
  library(readr)
  counter <- 1
  path <- paste(path2RmdFilesFolder, "/", "exam.txt", sep = "")
  file.create(path)
  
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
    answer_string <- "\\begin{multicols}{3} \n \\begin{choices} \n"
    code_index <- 1
    
    for (element in answers_clear) {
      if (answers_codes[[code_index]] == "0") {
        answer_string <- paste(answer_string, "\\", "wrongchoice{", element, "} \n", sep = "")
      } else {
        answer_string <- paste(answer_string, "\\", "correctchoice{", element, "} \n", sep = "")
      }
      code_index <- code_index + 1
    }
    
    answer_string <- paste(answer_string, "\\end{multicols} \n")
    
    # Prepare the question by collecting all the parts.
    question <- paste("\\element{prob_one_sample}{ \n \\begin{questionmult}{", 
                      counter, "} \n", task, answer_string, "\\end{questionmult} \n } \n\n", sep = "")
    
    # Update the text file.
    write(question, path, append = TRUE)
    
    counter <- counter + 1
  }
}

