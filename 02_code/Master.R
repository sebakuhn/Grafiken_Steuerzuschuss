Sys.setenv(LANG = "en")
library(pacman)
# you need devtools to run this code!
# require(devtools)
# install_github("16mc1r/dpaux", ref = "master")
# require(dpaux)

# Check whether required packages are installed. If not they are installed.

# paletteer_d("ggsci::category10_d3")

# Rtools paths ------------------------------------------------------------
Sys.setenv(PATH = paste("H:/programs/rtools40",
                        # "H:/programs/clang32/bin",
                        # "H:/programs/clang64/bin",
                        "H:/programs/rtools40/mingw32/bin",
                        "H:/programs/rtools40/mingw64/bin",
                        "H:/programs/rtools40/usr/bin",
                        "H:/programs/rtools40/bin",
                        "H:/programs/rtools40/perl/bin",
                        "H:/programs/R/R-4.0.5/bin",
                        "H:/programs/tinytex/bin/win32/tlmgr",
                        "H:/programs/tinytex/bin/win32",
                        "H:/programs/optipng",
                        "H:/programs/git/bin/",
                        Sys.getenv("PATH"),
                        sep = ";"
))


Sys.setenv(BINPREF = "H:/programs/rtools40/mingw$(WIN)/bin/")
# Rstudio appearance --> Cobalt

# Packages <-<-<-<-<-<-<-<-<
std_pckg <- c(
  "tidyverse",
  "datapasta",
  "knitr",
  "kableExtra",
  "janitor",
  "stringr",
  "Hmisc",
  "cowplot",
  "naniar",
  "ggsci",
  "keyring",
  "cyphr",
  "glue",
  "here",
  "aokaux",
  "data.table",
  "targets",
  "tarchetypes",
  #  "summarytools",
  "skimr",
  #  "gt",
  "tidylog",
  "aokbwcd",
  "extrafont"
)

# additional packages -- Varying by project
# add_pckg <- c("randomForest", "glmnet",
#               "kernlab", "xgboost", "foreach",
#               "naniar", "vtreat", "grid", "gridExtra",
#               "lubridate", "xml2", "rvest")

add_pckg <- c("DBI", "openxlsx", "odbc", "patchwork")

if (exists("add_pckg")) {
  required_packages <- c(std_pckg, add_pckg)
} else {
  required_packages <- std_pckg
}

# load all packages

p_load(required_packages, character.only = TRUE,
       install = TRUE,
       update = FALSE)


# DB connections ----------------------------------------------------------
conns <- c("conDB", "bwr4", "bwr3", "ak7")

# Options -----------------------------------------------------------------

# set penalty for scientific notation
options(scipen = 100)
# options(digits = 4)

# set global graphing options

# theme_set(theme_cowplot(font_size = 12))
# theme_update(
#   legend.position = "bottom",
#   panel.grid.major = element_line(colour = "grey85"),
#   panel.grid.major.x = element_blank(),
#   plot.title.position = "plot",
#   legend.text = element_text(size = 10),
#   legend.box = "horizontal", # group multiple legends below each other
#   legend.box.just = "top", # must be top if legend box is horizontal
#   legend.text.align = 0, # justify legends left
#   legend.title = element_text(size = 10)
# )

# Tibble options
options(tibble.print_min = 10)


# Set scales to use the scale_fill_d3
# By redefining the function
scale_fill_discrete <- function(...) {
  scale_fill_d3()
}

scale_colour_discrete <- function(...) {
  scale_colour_d3()
}

# Keyring and encryption --------------------------------------------------
# cyphr_key <- key_sodium(charToRaw(key_get("cyphr")))
# 
# # register readr rds functions for en- and decryption
# cyphr::rewrite_register("readr", "write_rds", "file")
# cyphr::rewrite_register("readr", "read_rds", "file")
# openxlsx formats --------------------------------------------------------

# openxlsx Style objects
header_style <- createStyle(
  halign = "center", valign = "center", textDecoration = "Bold",
  border = "TopBottomLeftRight"
)

# suppress the hh:mm:ss part of datetime
options("openxlsx.datetimeFormat" = "yyyy-mm-dd")

# locale settings ---------------------------------------------------------

eur_loc <- readr::locale(
  encoding = "latin1", decimal_mark = ",",
  grouping_mark = "."
)

# Parallel Processing -----------------------------------------------------
# set up parallel processing

n_cores <- parallel::detectCores() - 1
data.table::setDTthreads(threads = n_cores)

# aux functions -----------------------------------------------------------

ViewR <- function(x, n = 100) {
  View(x[sample(1:nrow(x), size = n), ])
}

`%not_in%` <- purrr::negate(`%in%`)


dispull_narm <- compose(pull, na.omit, distinct)
dispull <- compose(pull, distinct)

# DEV Section -------------------------------------------------------------

# 
# 
# source(here("02_code/90_aux_functions_morbi_qs.R"))
# 

# System and Software Info
# sink(file = file.path(table_dir, "Software_doc.txt"))
# sessionInfo()
# separator("Required Packages")
# kable(
# as.tibble(installed.packages()) %>% select(Package, Version) %>%
#   filter(Package %in% required_packages)
# )
# sink(NULL)


# gt formatter options ----------------------------------------------------

# require(gt)
# gt_fmt_eur <- partial(gt::fmt_currency, currency = "EUR", decimals = 0, locale = "de_DE")


# DB connection -----------------------------------------------------------
require(aokaux)
conDB <- AokCondbConnector$new() 
#hana <- AokHanaConnector$new("HANA_AK7") 


