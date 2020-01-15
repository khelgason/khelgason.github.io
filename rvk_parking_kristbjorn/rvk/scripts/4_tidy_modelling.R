library(tsibble)
library(fable) # þarf version >= 8.3 af {forecast}
library(feasts)
library(lubridate)

theme_set(theme_minimal())

# Make monthly data into timeseries, visualize and decompose
monthly_ts <- as_tsibble(monthly_usage) %>% 
  select(date, usage_mon) %>%
  as_tsibble(index = date)

p_mon_dec <- monthly_ts %>% feasts::STL() %>% autoplot()


# create timeseries grouped by each postnumber
monthly_by_pn_ts <- monthly_usage_by_pn %>% 
  select(date, postnumer, usage_mon) %>% 
  as_tsibble(index = date, key = c(postnumer))

p_mon_dec_by_pn <- monthly_by_pn_ts %>% feasts::STL() %>% autoplot() + scale_color_viridis_d()

# Modelling ---------------------------------------------------------------

# Model total usage
fit_monthly_usage <- monthly_ts %>% 
  model(fable::ARIMA()) #%>% report() #returns <ARIMA(0,1,1)(2,1,0)[12]>

# Model usage by postnumer
fit_monthly_usage_by_pn <- monthly_by_pn_ts %>% 
  model(
    lm_non_season = TSLM(usage_mon ~ date),
    arima = fable::ARIMA(usage_mon ~ pdq(0,1,1) + PDQ(2,1,0))
  )

# Forcast usage for next 3 years, add 90% confidence level values
fc_monthly_usage_by_pn <- fit_monthly_usage_by_pn %>% 
  fabletools::forecast(h = "3 years") %>% 
  mutate(hilo = hilo(.distribution, 90))

# plot the forecast and historical data
p_models_by_pn <- fc_monthly_usage_by_pn %>% 
  autoplot(monthly_by_pn_ts, level = 90) + 
  facet_wrap(~postnumer, scales = 'free') +
  theme_bw()

# create usage forecast sums for each year for arima model
usage_forecasted <- fc_monthly_usage_by_pn %>%
  filter(.model == 'arima') %>% 
  mutate(year = lubridate::year(date)) %>% 
  index_by(year) %>% 
  summarise(usage = sum(usage_mon),
            lower_forec = sum(hilo$.lower),
            upper_forec = sum(hilo$.upper)) %>% 
  mutate(key = 'forecast') %>% 
  as_tibble()


usage_year <- monthly_usage %>% 
  group_by(year) %>% 
  summarise(usage = sum(usage_mon)) %>% 
  mutate(key = 'actual')

usage_act_and_forec <- usage_forecasted %>% 
  bind_rows(usage_year)

# model_acc <- accuracy(fit_monthly_usage_by_pn) %>% arrange(postnumer)

# Test Model Performance --------------------------------------------------

# make train data of data before 2017
train <- monthly_by_pn_ts %>%
  filter(lubridate::year(date) < 2017)

# fit arima model to train data
fit <- train %>%
  model(
    arima = ARIMA(usage_mon ~ pdq(0,1,1) + PDQ(2,1,0))
  )

# make forecasted data from 2018-2021
fc <- fit %>% 
  forecast(h = "4 years") %>% 
  mutate(hilo = hilo(.distribution, 90))

# plot for two postnumbers
p_model_performance <- fc %>%
  filter(postnumer %in% c(101, 103)) %>%
  autoplot(monthly_by_pn_ts, col = 'red', level = 90) + 
  facet_wrap(~postnumer, scales = 'free') +
  theme_bw()

# calculate usage sums and compare with actual
fc_usage <- fc %>%
  mutate(year = lubridate::year(date)) %>% 
  index_by(year) %>% 
  summarise(forcasted = sum(usage_mon),
            lower_forec = sum(hilo$.lower),
            upper_forec = sum(hilo$.upper)) %>%
  as_tibble()

fc_diff_actual <- monthly_usage %>% 
  group_by(year) %>% 
  summarise(actual = sum(usage_mon)) %>% 
  left_join(fc_usage) %>% 
  mutate(diff_from_actual = actual - forcasted,
         diff_ratio = diff_from_actual / actual)

p_forecast_vs_actual <- fc_diff_actual %>% 
  select(-starts_with('diff')) %>% 
  na.omit() %>% 
  tidyr::gather('key', 'value', -c(year, lower_forec, upper_forec)) %>% 
  mutate(lower_forec = ifelse(key == 'actual', NA, lower_forec)) %>% 
  ggplot(aes(x=factor(year), y=value, fill=key)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=lower_forec, ymax=upper_forec), 
                width=.2, position=position_dodge(.9)) +
  scale_fill_brewer(palette = 'Set1') +
  labs(x = 'year')
