data_2_answers = function(filename) {
  exam_data = readr::read_rds(filename)
  exam_vector = unlist(exam_data)
  exam_tibble = tibble::tibble(value = exam_vector, branch = names(value))
  exam_tibble = dplyr::mutate(exam_tibble, n_dots = str_count(branch, pattern = "\\."))
  max_dots = max(exam_tibble$n_dots)
  exam_tibble = dplyr::mutate(exam_tibble, branch = pmap_chr(list(branch, n_dots),
                                                      ~paste0(.x, rep(".", max_dots - .y), collapse = "")))
  
  exam_tibble = tidyr::separate(exam_tibble, branch, into = c("variant", "exercise", "level1", "level2"), sep = "\\.")
  exam_tibble = dplyr::select(exam_tibble, -n_dots)
  
  ex_answers = dplyr::filter(exam_tibble, 
                      (level1 == "metainfo") & ((level2 == "name") | (str_detect(level2, "solution")))) %>%
    select(-level1)
  
  ex_answers_wide = tidyr::spread(ex_answers, level2, value)
  ex_answers_wide = dplyr::mutate(ex_answers_wide, ans_letter = 
                             pmap_chr(list(solution1, solution2, solution3, solution4, solution5),
                                      ~ paste0(ifelse(..1, "a", ""), 
                                               ifelse(..2, "b", ""), 
                                               ifelse(..3, "c", ""), 
                                               ifelse(..4, "d", ""), 
                                               ifelse(..5, "e", ""), collape = "")))
  ex_answers_wide = dplyr::mutate(ex_answers_wide, 
                           q_no = as.numeric(str_extract(exercise, "[0-9]+$")))
  ex_answers_wide = dplyr::select(ex_answers_wide, variant, q_no, name, ans_letter)
  
  return(ex_answers_wide)
}




exams2pdf_source = function(files_sample, n_vars = 1, add_seed = 777, 
                            pdf_dir = "pdf",
                            language = "ru",
                            tex_dir = "tex",
                            name = "the_exam",
                            date = "2018-12-28", institution = "Теория вероятностей",
                            logo = "",
                            encoding = "UTF-8",
                            samepage = TRUE,
                            reglength = 3, # is not working?
                            blank = 0,
                            header = "\\input{../header.tex}",
                            title = "С Наступающим Новым Годом :)", 
                            nops = TRUE, shuffle = TRUE) {
  for (var_no in 1:n_vars) {
    var_no_string = stringr::str_pad(var_no, 2, pad = "0")
    pdf_dir_no = paste0(pdf_dir, "_", var_no_string)
    dir.create(pdf_dir_no)
    tex_dir_no = paste0(tex_dir, "_", var_no_string)
    dir.create(pdf_dir_no)
    temp_dir_no = paste0("temp_", var_no_string)
    # dir.create(temp_dir_no)
    supp_dir_no = paste0("supp_", var_no_string)
    # dir.create(supp_dir_no)
    
    
    name_no = paste0(name, "_", var_no_string)
    
    if (shuffle) {
      set.seed(var_no + add_seed)
      files_sample = sample(files_sample)
    }
    
    set.seed(var_no + add_seed)
    if (nops) {
      exams <- exams2nops(files_sample, n = 1, startid = var_no + add_seed,
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
                          header = "\\input{../header.tex}",
                          title = title)
    } else {
      exams <- exams2pdf(files_sample, n = 1, 
                         dir = pdf_dir_no,
                         verbose = TRUE,
                         language = language,
                         texdir = tex_dir_no,
                         encoding = encoding,
                         template = "plain_no_sweave.tex",
                         header = "\\input{../header.tex}")
    }
    
    
  }
  return(NULL)
}

