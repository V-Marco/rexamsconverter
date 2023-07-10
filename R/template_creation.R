#' Get path to pretemplate examples
#'
#' Get path to pretemplate examples. 
#' There are some pretemplates in ints/extdata directory.
#' @param path name of pretemplate
#' @return vector of file names of path to specific file
#' @export
#' @examples
#' pretemplate_example()
#' pretemplate_example('zetex-pre-template.tex')
pretemplate_example = function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "rexamsconverter"))
  } else {
    system.file("extdata", path, package = "rexamsconverter", mustWork = TRUE)
  }
}

#' Transform pretemplate file to template
#'
#' Transform pretemplate file to template.
#' There are some pretemplates in ints/extdata directory.
#' @param path name of pretemplate
#' @param what vector of replaced words in pretemplate
#' @param to vector of replacement words 
#' @return template as character string
#' @export
#' @examples
#' cat(pretemplate2template())
pretemplate2template = function(path = pretemplate_example('zetex-pre-template.tex'), 
      what = c('lhead', 'chead', 'rhead', 'firstpage', 'nquestions',
               'lfoot', 'cfoot', 'rfoot'),
      to = c('Теория вероятностей', '', '1984-01-01', 'Удачи', '30',
             '', 'Dont panic', '\\thepage')) {
  template = readr::read_file(path)
  for (str_no in 1:length(what)) {
    what_with_hash = paste0('##', what[str_no], '##')
    template = stringr::str_replace(template, what_with_hash, to[str_no])
  }
  return(template)
}