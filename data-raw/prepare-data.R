

# LOAD DATA

# DIRECTORY
rep <- file.path('/Users', 'amandinepierrot', 'Documents', 'donnees', 'UK')

# PACKAGES
library(dplyr)

# IMPORT
lct <- Sys.getlocale('LC_TIME')
Sys.setlocale('LC_TIME', 'C')

load_data <- read.csv(file.path(rep, 'DemandData_2011-2016.csv'), sep = ',') %>%
  dplyr::select(SETTLEMENT_DATE, SETTLEMENT_PERIOD, ENGLAND_WALES_DEMAND) %>%
  dplyr::mutate(SETTLEMENT_DATE = lubridate::dmy(SETTLEMENT_DATE,
                                                 tz = 'Europe/London'),
                HOURS = (SETTLEMENT_PERIOD - 1) %/% 2,
                MINUTES = (SETTLEMENT_PERIOD - 1) %% 2 * 30,
                TIMESTAMP = SETTLEMENT_DATE + lubridate::dhours(HOURS) +
                  lubridate::dminutes(MINUTES)) %>%
  dplyr::select(SETTLEMENT_DATE, SETTLEMENT_PERIOD, TIMESTAMP, ENGLAND_WALES_DEMAND)

Sys.setlocale('LC_TIME', lct)

rm(lct, rep)

save(load_data, file = file.path('data', 'gb_load.RData'))

# modifier description gb_load


