#' Extracts answers from ugly list to human readable table
#'
#' Extracts answers from ugly list to human readable table
#' @param exam_data ugly list with answers from R exams package
#' @return tibble with answers
#' @export
#' @examples
#' # data_2_answers(exam_data)
data_2_answers = function(exam_data) {
  exam_vector = unlist(exam_data)
  exam_tibble = tibble::tibble(value = exam_vector, branch = names(value))
  exam_tibble = dplyr::mutate(exam_tibble, n_dots = stringr::str_count(branch, pattern = "\\."))
  max_dots = max(exam_tibble$n_dots)
  exam_tibble = dplyr::mutate(exam_tibble, branch = purrr::pmap_chr(list(branch, n_dots),
                                                      ~paste0(.x, rep(".", max_dots - .y), collapse = "")))

  exam_tibble = tidyr::separate(exam_tibble, branch, into = c("var_raw", "exercise", "level1", "level2"), sep = "\\.")
  exam_tibble = dplyr::select(exam_tibble, -n_dots)

  ex_answers = dplyr::filter(exam_tibble,
                      (level1 == "metainfo") & ((level2 == "name") | (stringr::str_detect(level2, "solution")))) %>%
    dplyr::select(-level1)

  ex_answers_wide = tidyr::spread(ex_answers, level2, value)
  ex_answers_wide = tidyr::pivot_longer(ex_answers_wide, 
                                        cols = starts_with("solution"),
                                        names_to = 'option')
  ex_answers_wide = dplyr::filter(ex_answers_wide, value == TRUE)
  ex_answers_wide = dplyr::mutate(ex_answers_wide, 
                                  ans_no = as.numeric(stringr::str_sub(option, start = -1)),
                                  ans_letter = base::letters[ans_no])
  ex_answers_wide = dplyr::mutate(ex_answers_wide,
                           q_no = as.numeric(stringr::str_extract(exercise, "[0-9]+$")))
  ex_answers_wide = dplyr::select(ex_answers_wide, var_raw, q_no, name, ans_letter)

  return(ex_answers_wide)
}


#' User-friendly version of exams2pdf from exams package
#'
#' User-friendly version of exams2pdf from exams package
#' @param n_vars number of variants
#' @param output_dir folder for output (pdf/tex/rmd copy)
#' @param language language
#' @param name name
#' @param date date
#' @param institution instituition
#' @param logo name of the logo file
#' @param filename character vector of Rmd file names
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
#' @param shuffle logical, whether to shuffle questions in each variant
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
exams2pdf_source = function(filename, n_vars = 1, add_seed = 777,
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
                            nops = TRUE, shuffle = TRUE,
                            answers_as_tbl = TRUE) {
  all_answers = list()

  dir.create(output_dir)

  rmd_dir = paste0(output_dir, "/rmd/")
  dir.create(rmd_dir)

  n_question = length(filename)
  pad_width = round(log10(n_question)) + 1
  files_sample_unshuffled = tibble(filename = filename,
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

    name_no = paste0(name, "_", var_no_string)

    if (shuffle) {
      set.seed(var_no + add_seed)
      files_sample = sample_n(files_sample_unshuffled, nrow(files_sample_unshuffled))
    } else {
      files_sample = files_sample_unshuffled
    }

    set.seed(var_no + add_seed)
    if (nops) {
      exams <- exams::exams2nops(files_sample$filename, n = 1, startid = var_no + add_seed,
                          dir = pdf_dir_no,
                          verbose = TRUE,
                          language = language,
                          texdir = tex_dir_no,
                          name = name,
                          date = date, institution = institution,
                          logo = "",
                          encoding = encoding,
                          samepage = samepage,
                          reglength = reglength, # is not working?
                          blank = blank,
                          header = header,
                          title = title)
    } else {
      exams <- exams::exams2pdf(files_sample$filename, n = 1,
                         dir = pdf_dir_no,
                         verbose = TRUE,
                         language = language,
                         texdir = tex_dir_no,
                         encoding = encoding,
                         template = template)
    }
    if (answers_as_tbl) {
      exam_df = data_2_answers(exams)
      exam_df = dplyr::mutate(exam_df, var_no = var_no)
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

