
# PACKAGES
library(dplyr)

# DIRECTORY
data_dir <- file.path('/Users', 'amandinepierrot', 'Documents', 'donnees', 'UK')

# FUNCTIONS
source(file.path('data-raw', 'trick-functions.R'))


# LOAD DATA --------------------------------------------------------------------

# IMPORT

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


# WEATHER DATA -----------------------------------------------------------------

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


# DAY TYPES --------------------------------------------------------------------
bank_hol <- timeDate::holidayLONDON(
  unique(lubridate::year(gb_load[['TIMESTAMP']]))
  )
bank_hol <- lubridate::with_tz(as.POSIXct(bank_hol), tzone = 'Europe/London')
gb_load <- gb_load %>%
  mutate(wday7 = lubridate::wday(TIMESTAMP),
         bh = ifelse(SETTLEMENT_DATE %in% bank_hol, 1, 0),
         DAY_TYPE = ifelse(bh == 0, wday7, 8)) %>%
  select(-wday7, -bh)
rm(bank_hol)

save(gb_load, file = file.path('data', 'gb_load.RData'))



# CLUSTERS ---------------------------------------------------------------------
cluster_data <- gb_load %>%
  select(SETTLEMENT_DATE, DAY_TYPE) %>%
  mutate(MONTH = month(SETTLEMENT_DATE),
         season_clust = ifelse(MONTH %in% c(1:3, 11:12), 1, 2)) %>%
  select(-MONTH) %>%
  distinct()

Y_info <- cluster_data[2:nrow(cluster_data), ] %>%
  select(-SETTLEMENT_DATE) %>%
  rename(Y_dt = DAY_TYPE,
         Y_sc = season_clust)
X_info <- cluster_data[1:(nrow(cluster_data) - 1), ] %>%
  select(-SETTLEMENT_DATE) %>%
  rename(X_dt = DAY_TYPE,
         X_sc = season_clust)

clusters <- cbind(Y_info, X_info) %>%
  distinct() %>%
  filter(Y_dt != 8,
         X_dt != 8)
clusters <- data.frame(clust = 1:nrow(clusters),
                       clusters)

begin_pred <- which(as.character(
  cluster_data[2:nrow(cluster_data), 'SETTLEMENT_DATE']) == '2016-01-01')

# clusters on train data
clust_train <- find_id(Y_info = Y_info[1:(begin_pred - 1), ],
                       X_info = X_info[1:(begin_pred - 1), ],
                       clusters = clusters)
n_byclust <- sapply(clust_train, length)
clust_train <- clust_train[n_byclust > 5]

save(clust_train, file = file.path('data', 'clust_train.RData'))


# clusters on test data
clust_test <- find_id(Y_info = Y_info[begin_pred:nrow(Y_info), ],
                      X_info = X_info[begin_pred:nrow(X_info), ],
                      clusters = clusters)
clust_test <- clust_test[n_byclust > 5]
# keep only clusters with 5 obs for fitting

save(clust_test, file = file.path('data', 'clust_test.RData'))


# clusters on all data
clust <- find_id(Y_info = Y_info,
                 X_info = X_info,
                 clusters = clusters)
# keep only clusters with 5 obs on fitting period
clust <- clust[n_byclust > 5]

