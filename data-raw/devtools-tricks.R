usethis::use_data_raw()
usethis::use_data(gb_load)
usethis::use_data(clust_train)
usethis::use_data(clust_test)
devtools::document()
usethis::use_package('magrittr')
usethis::use_package('lubridate')
usethis::use_package('dplyr')
usethis::use_package('stats')
usethis::use_build_ignore('cran-comments.md')
usethis::use_build_ignore('NEWS.md')
