#' Get path to pretemplate examples
#'
#' Get path to pretemplate examples. 
#' There are some pretemplates in inst/extdata directory.
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

#' Get a list of pretemplate parameters
#'
#' Get a list of fields that can be replaced by something meaningful. 
#' Use four slashes to write one slash. 
#' @param path name of pretemplate
#' @return character vector of pretemplate params
#' @export
#' @examples
#' pretemplate_params()
pretemplate_params = function(path = pretemplate_example('zetex-pre-template.tex')) {
  template = readr::read_file(path)              
  params = stringr::str_match_all(template, '##([a-zA-Z]+)##')[[1]][, 2]
  return(params)
}


#' Transform pretemplate file to template
#'
#' Transform pretemplate file to template.
#' There are some pretemplates in inst/extdata directory.
#' @param path path to pretemplate
#' @param ... fields replaced in pretemplate as character vectors.
#' The length of each character vector should be 1 or equal to the number of variants.
#' @return templates as character vector
#' @export
#' @examples
#' template_text = pretemplate2template(
#'   lhead = 'Теория вероятностей', chead = '', rhead = '1984-01-01', 
#'   firstpage = 'Удачи!', nquestions = '30', 
#'   lfoot = c('Вариант 1', 'Вариант 2'), 
#'   cfoot = 'Don\'t panic', rfoot = '\\\\thepage')
#' cat(template_text[1])

pretemplate2template = function(path = pretemplate_example('zetex-pre-template.tex'), ...) {
  template = readr::read_file(path)
  params = pretemplate_params(path)
  message('You can set: ', paste0(params, collapse = ', '), '\n', 'in the pretemplate\n', path, '\n')
  message('You may write your own pretemplate file. \nThat\'s an ordinary tex-file with some ##fieldname## entries.\n')
  
  arguments = c(as.list(environment()), list(...))
  fields = setdiff(names(arguments), 'path')
  
  n_vars = 1
  for (field in fields) {
    n_vars = max(n_vars, length(arguments[[field]]))
  }
  template = rep(template, n_vars)
  
  for (var_no in 1:n_vars) {
    for (field in fields) {
      what_with_hash = paste0('##', field, '##')
      if (n_vars > length(arguments[[field]])) {
        replacement = arguments[[field]][1]
      } else {
        replacement = arguments[[field]][n_vars] 
      }
      if (is.null(replacement)) { 
        replacement = ''
      }
      template[var_no] = stringr::str_replace(template[var_no], what_with_hash, replacement)
    }
  }
  return(template)
}


