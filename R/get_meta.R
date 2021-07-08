#' Get meta information from one or many Rmd rexams files
#'
#' Get meta information from one or many Rmd rexams files
#' @param file character vector path to Rmd file or Rmd files
#' @return tibble
#' @export
#' @examples
#' # res = get_meta_information(file = c('1.Rmd', '2.Rmd'))
get_meta_information = function(file) {
  all_meta = NULL
  for (the_file in file) {
    print(the_file)
    one_file_meta = get_meta_information0(the_file)
    all_meta = dplyr::bind_rows(all_meta, one_file_meta)
  }
  return(all_meta)
}




#' Get meta information from one Rmd rexams file
#'
#' Get meta information from one Rmd rexams file
#' @param file character path to Rmd file
#' @return tibble
#' @export
#' @examples
#' # res = get_meta_information0(file = '1.Rmd')
get_meta_information0 = function(file) {
  question_text = readr::read_lines(file)

  meta_info_line = (question_text == 'Meta-information')
  if (sum(meta_info_line) > 1) {
    stop('Too many "Meta-information" lines in ', file, '.')
  }
  if (sum(meta_info_line) == 0) {
    stop('"Meta-information" line not found in ', file, '.')
  }
  useful_lines = question_text[which(meta_info_line):length(question_text)]
  useful_lines = stringr::str_subset(useful_lines, ': ')
  splitted_info = stringr::str_split_fixed(useful_lines, ': ', 2)
  splitted_info[, 1] = stringr::str_replace_all(splitted_info[, 1],
                                                pattern = '\\[', replacement = '_')
  splitted_info[, 1] = stringr::str_replace_all(splitted_info[, 1],
                                                pattern = '\\]', replacement = '_')
  splitted_info = t(splitted_info)
  colnames(splitted_info) = splitted_info[1, ]
  splitted_info = splitted_info[2, ]

  meta = tibble::as_tibble_row(splitted_info)
  meta = dplyr::mutate(meta, file = file)
  return(meta)
}

