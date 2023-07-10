#' Extracts answers from ugly list to human readable table
#'
#' Extracts answers from ugly list to human readable table
#' @param exam_data ugly list with answers from R exams package
#' @param preserved_fields preserved fields in metainfo
#' @return tibble with answers
#' @export
#' @examples
#' # data_2_answers(exam_data)
data_2_answers = function(exam_data, 
                          preserved_fields = c('name', 'Language', 'Level', 'Type')) {
  exam_vector = unlist(exam_data)
  exam_tibble = tibble::tibble(value = exam_vector, branch = names(value))
  exam_tibble = dplyr::mutate(exam_tibble, n_dots = stringr::str_count(branch, pattern = "\\."))
  max_dots = max(exam_tibble$n_dots)
  exam_tibble = dplyr::mutate(exam_tibble, branch = purrr::pmap_chr(list(branch, n_dots),
                                                      ~paste0(.x, rep(".", max_dots - .y), collapse = "")))

  exam_tibble = tidyr::separate(exam_tibble, branch, into = c("var_raw", "exercise", "level1", "level2"), sep = "\\.")
  exam_tibble = dplyr::select(exam_tibble, -n_dots)

  ex_answers = dplyr::filter(exam_tibble,
                      (level1 == "metainfo") & ((level2 %in% preserved_fields) | (stringr::str_detect(level2, "solution")))) %>%
    dplyr::select(-level1)
  ex_options = dplyr::filter(exam_tibble, stringr::str_starts(level1, "questionlist")) %>%
    dplyr::select(-level1)
  ex_options = dplyr::group_by(ex_options, exercise) %>% 
    dplyr::mutate(id = dplyr::row_number(),
                  colname = ifelse(level2 == '', paste0('option_', id), level2))
    
  
  ex_options_wide = tidyr::pivot_wider(ex_options, 
      id_cols = 'exercise', values_from = 'value', names_from = 'colname') 

  ex_answers_wide = tidyr::spread(ex_answers, level2, value)
  ex_answers_long = tidyr::pivot_longer(ex_answers_wide, 
                                        cols = dplyr::starts_with("solution"),
                                        names_to = 'option')
  ex_answers_true = dplyr::filter(ex_answers_long, value == TRUE)
  ex_answers_letter = dplyr::mutate(ex_answers_true, 
                                  ans_no = as.numeric(stringr::str_sub(option, start = -1)),
                                  ans_letter = base::letters[ans_no]) %>%
                    dplyr::select(exercise, ans_no, ans_letter)
  ex_answers_wide = dplyr::mutate(ex_answers_wide,
                           q_no = as.numeric(stringr::str_extract(exercise, "[0-9]+$"))) %>%
          dplyr::select(-var_raw)
  
  ex_questions = dplyr::filter(exam_tibble, stringr::str_starts(level1, 'question[0-9]*$'))  %>%
    dplyr::group_by(exercise) %>% 
    dplyr::summarise(text = paste(value, sep='\n', collapse = '\n'))
  
  
  all_info = dplyr::left_join(ex_answers_wide, 
                              ex_answers_letter,
                              by = 'exercise')
  all_info = dplyr::left_join(all_info, 
                              ex_options_wide,
                              by = 'exercise') %>%
              dplyr::left_join(ex_questions, by = 'exercise') %>%
    dplyr::select(-exercise) %>%
    dplyr::relocate(q_no, ans_no, ans_letter, text)
  

  return(all_info)
}


#' User-friendly version of exams2pdf from exams package
#'
#' User-friendly version of exams2pdf from exams package. 
#' Option texengine = "xelatex" may be useful.
#' @param n_vars number of variants
#' @param output_dir folder for output (pdf/tex/rmd copy)
#' @param language language
#' @param name name
#' @param date date
#' @param institution instituition
#' @param logo name of the logo file
#' @param filename character vector of Rmd file names
#' @param shuffle logical, whether to shuffle questions in each variant
#' @param add_seed actual seed is variant number plus add_seed
#' @param encoding encoding
#' @param samepage ?
#' @param blank number of blank pages
#' @param template name of the template tex file, not used if nops == TRUE
#' @param header header tex file name, not used if nops == FALSE
#' @param reglength ?
#' @param title title
#' @param answers_as_tbl logical, return answers as tibble and not as ugly list
#' @param nops logical, whether to use exam2nops or exam2pdf
#' @param ... further parameters passed to exams2nops or exams2pdf
#' @return Creates tex files and runs latex compilation
#' @export
#' @examples
#' # library(exams)
#' Sys.which("latexmk") # check whether latex is present and specify path to latexmk
#' latex_executable <- Sys.which("pdflatex")
#' options(texi2dvi = latex_executable)
#' # files_all <- list.files('rmd/', pattern = "*.Rmd", full.names = TRUE)
#' # exams2pdf_source(files_sample, date = "2019-09-27",
#' #            n_vars = 2, title = "Теория вероятностей!", institution = "Поехали :)",
#' # nops = TRUE, shuffle = TRUE)
exams2pdf_source = function(filename, n_vars = 1, 
                            shuffle = TRUE, add_seed = 777,
                            output_dir = "output",
                            language = "ru",
                            name = "the_exam",
                            date = "2018-12-28", institution = "Probability theory",
                            logo = "",
                            encoding = "UTF-8",
                            samepage = TRUE,
                            reglength = 3, # is not working?
                            blank = 0,
                            template = "plain_no_sweave.tex",
                            header = "\\input{../header.tex}",
                            title = "Be Happy :)",
                            nops = TRUE, 
                            answers_as_tbl = TRUE, ...) {
  all_answers = list()

  dir.create(output_dir)

  rmd_dir = paste0(output_dir, "/rmd/")
  dir.create(rmd_dir)

  n_question = length(filename)
  pad_width = round(log10(n_question)) + 1
  files_sample_unshuffled = tibble::tibble(filename = filename,
                        local_filename = paste0(stringr::str_pad(1:n_question, pad_width, pad = "0"), ".Rmd"))

  for (i in 1:n_question) {
    file.copy(files_sample_unshuffled$filename[i], paste0(rmd_dir, files_sample_unshuffled$local_filename[i]))
  }

  for (var_no in 1:n_vars) {
    var_no_string = stringr::str_pad(var_no, 2, pad = "0")
    pdf_dir_no = paste0(output_dir, "/pdf_", var_no_string)
    dir.create(pdf_dir_no)
    tex_dir_no = paste0(output_dir, "/tex_", var_no_string)
    dir.create(pdf_dir_no)

    name_no = paste0(name, "_v", var_no_string, '_')

    if (shuffle) {
      set.seed(var_no + add_seed)
      files_sample = dplyr::sample_n(files_sample_unshuffled, nrow(files_sample_unshuffled))
    } else {
      files_sample = files_sample_unshuffled
    }

    # set.seed(var_no + add_seed)
    if (nops) {
      exams <- exams::exams2nops(files_sample$filename, n = 1, 
                          startid = var_no + add_seed,
                          dir = pdf_dir_no,
                          verbose = TRUE,
                          language = language,
                          texdir = tex_dir_no,
                          name = name_no,
                          date = date, institution = institution,
                          logo = "",
                          encoding = encoding,
                          samepage = samepage,
                          reglength = reglength, # is not working?
                          blank = blank,
                          header = header,
                          title = title,
                          seed = FALSE, ...)
    } else {
      exams <- exams::exams2pdf(files_sample$filename, n = 1,
                         dir = pdf_dir_no,
                         verbose = TRUE,
                         name = name_no,
                         language = language,
                         texdir = tex_dir_no,
                         encoding = encoding,
                         template = template, 
                         seed = FALSE, ...)
    }
    if (answers_as_tbl) {
      exam_df = data_2_answers(exams)
      exam_df = dplyr::mutate(exam_df, var_no = var_no) %>%
        dplyr::relocate(var_no)
      exam_df = dplyr::bind_cols(exam_df, files_sample)
      all_answers[[var_no]] = exam_df
    } else {
      all_answers[[var_no]] = exams
    }
  }
  if (answers_as_tbl) {
    all_answers = dplyr::bind_rows(all_answers)
    readr::write_csv(all_answers, paste0(output_dir, "/question_info.csv"))
  } else {
    readr::write_rds(all_answers, paste0(output_dir, "/question_info.Rds"))
  }

  return(all_answers)
}

