
# PACKAGES
library(dplyr)

# DIRECTORY
data_dir <- file.path('/Users', 'amandinepierrot', 'Documents', 'donnees', 'UK')


# LOAD DATA --------------------------------------------------------------------

# IMPORT
lct <- Sys.getlocale('LC_TIME')
Sys.setlocale('LC_TIME', 'C')

gb_load <- read.csv(file.path(data_dir, 'DemandData_2011-2016.csv'), sep = ',') %>%
  dplyr::select(SETTLEMENT_DATE, SETTLEMENT_PERIOD, ENGLAND_WALES_DEMAND) %>%
  dplyr::mutate(SETTLEMENT_DATE = lubridate::dmy(SETTLEMENT_DATE,
                                                 tz = 'Europe/London'),
                HOURS = (SETTLEMENT_PERIOD - 1) %/% 2,
                MINUTES = (SETTLEMENT_PERIOD - 1) %% 2 * 30,
                TIMESTAMP = SETTLEMENT_DATE + lubridate::dhours(HOURS) +
                  lubridate::dminutes(MINUTES)) %>%
  dplyr::select(SETTLEMENT_DATE, SETTLEMENT_PERIOD, TIMESTAMP,
                ENGLAND_WALES_DEMAND)

Sys.setlocale('LC_TIME', lct)

rm(lct, rep)


# WEATHER DATA -----------------------------------------------------------------

# FUNCTIONS
source(file.path('data-raw', 'trick-functions.R'))

# IMPORT
gb_weather <- read.table(file.path(data_dir, 'meteo', '29657358679dat.txt'),
                         header = TRUE, fill = TRUE, na.strings = '****') %>%
  mutate(code = factor(USAF),
         timestamp = lubridate::with_tz(lubridate::ymd_hm(YR..MODAHRMN, tz = 'GMT'),
                                        tzone = 'Europe/London'),
         value = weathermetrics::fahrenheit.to.celsius(TEMP)) %>%
  select(code, timestamp, value)

# INTERPOLATION
time_index <- seq(lubridate::floor_date(gb_weather$timestamp[1], unit = 'hour'),
                  lubridate::floor_date(gb_weather$timestamp[nrow(gb_weather)],
                                        unit = 'hour'),
                  by = '30 min')
data_with_info <- detect_NA(gb_weather)
interpol_data <- data.frame(code = rep(unique(data_with_info[['code']]),
                                       each = length(time_index)),
                            timestamp = time_index,
                            value = NA,
                            mv = NA)
for (c in unique(interpol_data[['code']])) {
  interpol_idx <- interpol_data[['code']] == c
  original_idx <- data_with_info[['code']] == c
  interpol_data[interpol_idx, 'value'] <- approx(
    data_with_info[original_idx, 'timestamp'],
    data_with_info[original_idx, 'value'],
    xout = time_index)$y
  interpol_data[interpol_idx, 'mv'] <- approx(
    data_with_info[original_idx, 'timestamp'],
    data_with_info[original_idx, 'mv'],
    xout = time_index)$y > 0
}

# WEIGHTED TEMPERATURE
station_pop <- read.table(file.path(data_dir, 'meteo',
                                    'corr_meteo_town_code.txt'),
                          sep = '\t', header = TRUE)
# two stations in Liverpool
station_pop[station_pop[['town']] == 'LIVERPOOL',
            'pop'] <- station_pop[station_pop[['town']] == 'LIVERPOOL',
                                  'pop'] / 2
station_pop[station_pop[['town']] == 'LIVERPOOL_CROSBY',
            'pop'] <- station_pop[station_pop[['town']] == 'LIVERPOOL', 'pop']
station_pop <- station_pop %>%
  mutate(weight = pop / sum(pop))
raw_temperatures <- tidyr::spread(interpol_data[, c('code', 'timestamp', 'value')],
                                 code, value)
is_mv <- tidyr::spread(interpol_data[, c('code', 'timestamp', 'mv')],
                       code, mv)
gb_weather <- data.frame(TIMESTAMP = interpol_data[['timestamp']],
                         TEMPERATURE = as.matrix(raw_temperatures[, -1]) %*%
                           station_pop[['weight']],
                         MV = as.matrix(is_mv[, -1]) %*%
                           station_pop[['weight']])


# BOTH DATA --------------------------------------------------------------------
gb_load <- dplyr::left_join(gb_load, gb_weather, by = 'TIMESTAMP') %>%
  distinct()

save(gb_load, file = file.path('data', 'gb_load.RData'))

# modifier description gb_load une fois que toutes les data seront finies
# penser à invalider jours avec valeurs négatives à un moment



