library(dplyr)
library(lubridate)

detect_frequency <- function(data){
  data <- data %>%
    mutate(diff = difftime(timestamp, lag(timestamp), units = "mins"),
           freq_temp = round(quantile(diff, na.rm = TRUE, probs = 0.99)),
           freq = ifelse(freq_temp > 1000,
                         ifelse(freq_temp >= 28 * 1440,
                                "month",
                                "day" ),
                         as.character(freq_temp)),
           begin_date = round_date(min(timestamp), unit = "minute"),
           end_date   = round_date(max(timestamp), unit = "minute")
    ) %>%
    select(code, freq, begin_date, end_date) %>%
    distinct(.keep_all = TRUE)
}

create_calendar <- function(freq, begin_date, end_date){
  calendar <- NULL
  calendar <- switch(freq,
                     "month" = {
                       #calculate the number of month to create in the calendar
                       number_of_months = interval(
                         floor_date(ymd_hms(begin_date), "month"),
                         floor_date(ymd_hms(end_date), "month")) %/% months(1)

                       #calculate the number of month to create in the calendar
                       seq_date = ymd_hms(begin_date) %m+% months(0:number_of_months)

                       data.frame(date = seq_date)
                     },

                     "day" = {
                       seq_date = seq(from = begin_date,
                                      to   = end_date,
                                      by   = "day")
                       data.frame(date = seq_date)
                     },

                     {
                       data.frame(date = seq(from = begin_date,
                                             to = end_date,
                                             by = paste(freq , "min", sep = " ")))
                     }
  )
  return(calendar)
}



create_theo_cal <- function(data_freq) {
  theo_cal <- NULL
  for (i in 1:nrow(data_freq)) {
    theo_cal_temp <- create_calendar(
      freq         = data_freq[[i, 'freq']],
      begin_date   = data_freq[[i, 'begin_date']],
      end_date     = data_freq[[i, 'end_date']])
    theo_cal_temp$code <- rep(data_freq[[i, 'code']], nrow(theo_cal_temp))
    theo_cal <- bind_rows(theo_cal, theo_cal_temp)
  }
  return(theo_cal)
}


detect_NA <- function(data) {

  data_freq <- data %>%
    group_by(code) %>%
    detect_frequency() %>%
    ungroup()

  theo_cal <- create_theo_cal(data_freq)

  data <- full_join(data, theo_cal, by = c('timestamp' = 'date', 'code')) %>%
    arrange(code, timestamp) %>%
    mutate(mv = ifelse(is.na(value), 1, 0))

  return(data)
}