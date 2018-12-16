#' Extracts unique argument from a latex command.
#' 
#' Extracts unique argument from a latex command.
#'
#' For example, from `\command{aabbcc}` we will extract `aabbcc`.
#' Assumptions:
#' 1. Unique command with given name.
#' 2. Last closing bracket in element is the bracket we need.
#' @param element character string with the command
#' @param command command name 
#' @return string with the argument of a command
#' @export
#' @examples
#' element = "\n      \\correctchoice{ $\\frac{S_n - 4n}{10\\sqrt{n}}$ }"
#' extract_from_command(element, command = "correctchoice)
extract_from_command = function(element, command = "correctchoice") {
  
  search_pattern = paste0("\\\\", command, "\\{")
  
  start_pos = stringr::str_locate(element, search_pattern)[, 2]
  end_pos = utils::tail(stringr::str_locate_all(element, "\\}")[[1]], 1)[, 2]
  
  answer = stringr::str_trim(stringr::str_sub(element, start = start_pos + 1, end = end_pos - 1))
  return(answer)
}


#' Convert a formatted LaTex document into multiple Rmd files.
#'
#' Convert a formatted LaTex document into multiple Rmd files formatted as rexams.
#' The required format is the following: the LaTex document must contain all the questions you wish to convert to Rmd. 
#' The questions must be formatted \href{https://github.com/V-Marco/rexamsconverter/blob/master/format_example.txt}{as following}.
#' The document may also contain other elements (e.g. used for decoration) which will be ignored.
#' There can also be text pieces related to several questions, e.g. 
#' 'In questions 20-22...', which can be added to each of the questions mentioned.
#' To do so, wrap these text pieces \href{https://github.com/V-Marco/rexamsconverter/blob/master/format_example.txt}{as following}.
#' @param path2latex A path to the LaTex document in .txt format. The last element of the path must be the file itself.
#' @param directory_name folder name, optional
#' @return Creates a folder with Rmd files in the same directory where the LaTex document is stored.
#' @examples
#' # latex2RmdExams("2015_midterm.txt")
latex2RmdExams <- function(path2latex, directory_name = substr(path2latex, 1, nchar(path2latex) - 4)) {

  file <- readr::read_file(path2latex)

  # We'll keep the results in the folder.
  dir.create(directory_name)

  # As the files may contain LaTex decoration commands (such as 'ruler'), which
  # are not questions, and all the blocks begin with '\element', we need to choose
  # only those '\element blocks', which contain '\begin{choices}', i.e. the answer
  # list, as it is present in questions only. Moreover, we need save text pieces
  # between the questions (such as '...in the questions 27-30...') separately
  # to process them as well -- these are the elements which have 'retcommontext'.

  splits <- strsplit(file, "element")[[1]]

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

  # We process text pieces between the questions to find the questions to add them to.
  # The idea is to split the text by 'textbf' and take the second part -- this is the
  # task text. Then find the index of the firt '}' -- this is the end of the structure
  # '{In questions 00-11}'. Then take everything that comes after this index as a
  # task text, and take the numbers of questions from the structure itself, formating
  # as 'c(00, 01, ..., 11)'.

  # Question numbers and corresponding task texts are kept in two lists.
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

  # Further on, we iterate over the question list, getting the task text and the
  # list of answers.

  counter <- 1
  splits <- splits_true

  while (counter <= length(splits)) {

    one <- splits[[counter]]

    # Getting the task text. The idea is to split the question by '}'.
    # It can be observed that in the majority of questions the task text is located
    # between the 3d from the beginning and (12 from the end - 17)th symbols '{'.
    # If if is not so, the resulting text will contain a part of 'begin{multicols}'.
    # To process that, we split the resulting text by 'begin{multicols}' and take
    # the first part -- the cleared task text.

    # Related problem: if 'AMCMultiNoChoice' is placed outside the
    # '\begin{questionmult}' block, it will be lost.

    indexes <- which(strsplit(one, "")[[1]] == "}")
    indexes_rev <- rev(indexes)

    task <- substr(one, indexes[3] + 1, indexes_rev[12] - 17)

    if (grepl("begin\\{multicols\\}", task) == TRUE) {
      task <- strsplit(task, "begin\\{multicols\\}")[[1]][1]
    }

    # Sometimes some symbols in the end of the task text are identified by the
    # processor as one symbol, and therefore, are not deleted. The manual processing
    # for these cases is required.

    if (substr(task, nchar(task) - 2, nchar(task)) == "\\be") {
      task <- substr(task, 1, nchar(task) - 2)
    }

    if (substr(task, nchar(task), nchar(task)) == "\\") {
      task <- substr(task, 1, nchar(task) - 1)
    }

    if (substr(task, nchar(task), nchar(task)) == "%") {
      task <- substr(task, 1, nchar(task) - 1)
    }

    # Clearing the LaTex comments in the first row (everything between '%' and '\n').
    if (grepl("%", task) == TRUE) {
      index <- which(strsplit(task, "")[[1]] == "\n")[1]
      task <- substr(task, index + 1, nchar(task))
    }

    # Merging the task text with a corresponding text piece if appropriate.
    if (counter %in% inter_tasks_numbers == TRUE) {
      index <- which(inter_tasks_numbers == counter)
      prom <- inter_tasks_texts[[index]]

      task <- paste(prom, "\n", "\n", task)
    }

    # If the first symbol is '\n', we delete it.
    if (substr(task, 1, 1) == "\n") {
      task <- substr(task, 2, nchar(task))
    }

    # Getting the answer options. The idea is to split the question by 'choices', as this
    # string is always followed by the list of the answer options. Then split the result
    # by '\n' and find indices of elements, containing 'choices'. Then merge the answer
    # options as strings located between these indeces.

    choices <- strsplit(one, "choices\\}")[[1]][2]
    choices <- strsplit(choices, '\n')[[1]]

    ch_indexes <- which(grepl("choice", choices))
    ch_indexes[length(ch_indexes) + 1] <- length(choices)
    prom_choices <- list()
    prom_choices_index <- 1

    while (prom_choices_index <= length(ch_indexes) - 1) {

      str_prom <- ""
      prom_list <- choices[(ch_indexes[prom_choices_index]):(ch_indexes[prom_choices_index + 1] - 1)]

      for (element in prom_list) {
        str_prom <- paste(str_prom, element, sep = "\n")
      }

      prom_choices[[prom_choices_index]] <- str_prom

      prom_choices_index <- prom_choices_index + 1
    }

    choices <- prom_choices

    # We keep the answer texts in a list.
    text_choices <- list()
    text_index = 1
    # We also keep the answers as string formatted as "10000" to insert to meta-data.
    answer_string <- ""

    # Iterating over each answer option, we add the answer option to the list, taking it
    # from the curly brackets. Correspondingly we change 'answer_string' in accordance
    # with 'correctchoice' and 'wrongchoice'.

    for (element in choices) {
      if (grepl("correctchoice", element)) {
        text_choices[[text_index]] <- extract_from_command(element, command = "correctchoice")
        answer_string <- paste(answer_string, "1", sep = '')
      } else {
        text_choices[[text_index]] <- extract_from_command(element, command = "wrongchoice")
        answer_string <- paste(answer_string, "0", sep = '')
      }
      text_index <- text_index + 1
    }

    # Create a new Rmd file with an appropriate formatting.

    name <- paste(directory_name, "/", toString(counter), ".Rmd", sep = "")

    file.create(name)
    write("Question", name)
    write("========", name, append = TRUE)
    write(paste(task, "\n"), name, append = TRUE)
    write("Answerlist", name, append = TRUE)
    write("----------", name, append = TRUE)

    for (element in text_choices) {
      element <- element[1]
      write(paste("* ", element), name, append = TRUE)
    }

    write("\nSolution", name, append = TRUE)
    write("========", name, append = TRUE)
    write("\nAnswerlist", name, append = TRUE)
    write("----------", name, append = TRUE)

    for (element in strsplit(answer_string, "")[[1]]) {
      if (element == "0") {
        write(paste("* ", "Bad answer :("), name, append = TRUE)
      } else {
        write(paste("* ", "Good answer :)"), name, append = TRUE)
      }
    }

    write("\nMeta-information", name, append = TRUE)
    write("================", name, append = TRUE)
    write(paste0("exname: ", counter), name, append = TRUE)
    write("extype: schoice", name, append = TRUE)
    write(paste0("exsolution: ", answer_string), name, append = TRUE)
    write("exshuffle: 5", name, append = TRUE)

    counter = counter + 1
  }
}

