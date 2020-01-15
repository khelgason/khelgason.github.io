library(ggplot2)
library(plotly)
library(tsibble)

# function that returns a vector of n sk_parking numbers
random_lots <- function(n, df = park_data) {
  unique(park_data$sk_parking) %>% 
    sample(n, replace = F)
}

# Question 3 - Join tables and show top 20 rows ---------------------------

park_data <- f_parking %>% 
  left_join(d_calendar, by = 'sk_time') %>% 
  left_join(d_parking, by = 'sk_parking')

park_data_head20 <- head(park_data, 20)


# Data munging and aggregates ---------------------------------------------

# Remove extra coordinate columns
park_data <- park_data %>% 
  select(-ends_with('_isn'))

# Sum monthly usage
monthly_usage <- park_data %>% 
  group_by(sk_time, year, month_number) %>% 
  summarise(usage_mon = sum(usage)) %>% 
  ungroup() %>% 
  mutate(date = tsibble::yearmonth(paste(year, month_number, sep = '-')))

# Sum monthly usage by postnumer
monthly_usage_by_pn <- park_data %>% 
  group_by(sk_time, 
           year, 
           month_number, 
           postnumer) %>% 
  summarise(usage_mon = sum(usage)) %>% 
  ungroup() %>% 
  mutate(date = tsibble::yearmonth(paste(year, month_number, sep = '-')))


# Plots -------------------------------------------------------------------

theme_set(theme_minimal()) 

# Plot monthly parking usage
p_monthly <- ggplot(monthly_usage, aes(x=date, y=usage_mon)) +
  geom_line()

# Plot monthly parking usage by postnumer
p_mon_by_pn <- ggplot(monthly_usage_by_pn) +
  geom_line(aes(x=date, y=usage_mon, col = factor(postnumer)))  +
  scale_color_viridis_d()

# Plot a few random parking lots usage
p_some_lots <- ggplot(park_data %>% 
                        filter(sk_parking %in% random_lots(12))) +
  geom_line(aes(x=sk_time, y=usage, col=factor(postnumer))) +
  facet_wrap(~paste('sk:', sk_parking, ',', postnumer, 'Rvk.'), scales = 'free', as.table = T) +
  scale_color_viridis_d() +
  guides(color=guide_legend(title="Póstnúmer")) +
  theme_bw()

p_usage_by_pn_gjald <- park_data %>%
  sample_frac(0.2) %>%
  ggplot() +
  geom_violin(aes(x=postnumer, y = usage, col = gjaldskyld)) +
  scale_color_viridis_d()

p_usage_by_pn_eig <- park_data %>%
  sample_frac(0.2) %>%
  ggplot() +
  geom_violin(aes(x=postnumer, y = usage, col = eigandi)) +
  scale_color_viridis_d()

# park_data %>% 
#   filter(sk_parking %in% random_lots(10)) %>% 
#   select(usage, postnumer, eigandi, eigandi, gjaldskyld, landnumer) %>% 
#   pairs(lower.panel = NULL)

# Plot all usage per postnumer
# ggplot(park_data, aes(x = factor(postnumer), y = usage)) +
#   geom_violin() +
#   theme_minimal()

# ggplot(park_data %>% 
#          group_by(year, postnumer) %>% 
#          summarise(n = n_distinct(sk_parking))) +
#   geom_line(aes(x = as.integer(year), y = n, col = factor(postnumer)))

# 
# 
# park_data %>% 
#   sample_frac(0.2) %>% 
#   filter(eigandi != "(Vantar)") %>% 
#   ggplot(data = .) +
#   geom_smooth(aes(x=sk_time, y=usage, col = eigandi), se = F) +
#   facet_wrap(~postnumer, scales = "free")
# 
