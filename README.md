# rexamsconverter

Conversion of quiz files:

* from rexams Rmd to beamer
* from rexams Rmd to amc tex
* from from amc tex to rexams Rmd.

Some misc functions.

## Installation

```r
# install.packages("devtools")
devtools::install_github("bdemeshev/rexamsconverter")
```

Get all meta information in a tibble:
```r
files = list.files('path to Rmd files', pattern = "*.Rmd", full.names = TRUE, recursive = TRUE)
meta = get_meta_information(files)
```

The function `get_meta_information` supposes tags proposed by [sharestats](https://sharestats.github.io/Statistics_Taxonomy/Statistics_Taxonomy.html) project.


## Other converters

* [latex2gift](https://github.com/tomeucapo/latex2gift)
* [amc2moodle](https://github.com/nennigb/amc2moodle)
* [tex2quiz](https://github.com/hig3/tex2quiz)
* [latex2moodle](https://www.ctan.org/pkg/moodle)
* [GIFT format desctiption](https://docs.moodle.org/311/en/GIFT_format)
