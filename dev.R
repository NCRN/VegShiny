# create the dev environment from scratch

base::options(download.file.method = "wininet")
# wd <- getwd()
# unlink(base::file.path(wd, 'renv'), recursive = TRUE, force=T)
# unlink(base::file.path(wd, 'renv.lock'), recursive = TRUE, force=T)
library(renv)
renv::init()
default_dependencies <- base::c(
  # packages that consistently cause problems for me
  'usethis'
  ,'fs'
  ,'languageserver'
  ,'magrittr'
  ,'purrr'
  ,'jsonlite'
  ,'knitr'
  ,'rlang'
  ,'ggplot2'
  ,'glue'
  ,'pillar'
  ,'tibble'
  ,'stringr'
  ,'data.table'
  ,'readr'
  ,'tidyr'
  ,'ritis'
  ,'taxize'
  ,'covr'
  ,'EML'
  ,'httr'
  ,'worrms'
  ,'devtools'
  ,'remotes'
  ,'gargle'
  ,'ids'
  ,'readxl'
  ,'curl'
  ,'EML'
  ,'rlang'
  ,'tidyverse'
  ,'ISOcodes'
  ,'ellipsis'
  ,'gtable'
  ,'scales'
  ,'desc'
  ,'shiny'
  ,'leaflet'
  ,'lattice'
  ,'shinyjs'
  ,'DT'
  ,'sf'
)
renv::install(default_dependencies, prompt=F,rebuild=T)

library(remotes)
remotes::install_github('NCRN/NPSForVeg', force=T, upgrade='never')
library(NPSForVeg)
renv::snapshot(prompt = F)
renv::status()

