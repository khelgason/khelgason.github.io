# if(!require(leaflet)) install.packages('leaflet')
library(leaflet)
library(ggplot2)
library(ggmap)

# Leaflet Litapalletta byggð á eiganda
pal_eig <- colorFactor(palette = c('violet', 'darkgreen', 'orange2', 'yellow3', 'navy', 'grey', 'red'), 
                   domain = d_parking$eigandi)

# Groupa stæði saman á rúnnuð lat og long hnit til að geta plottað
parking_plot_data <- d_parking %>% 
  transmute(lat = round(lat * 2000) / 2000, #rúnna fjórða aukastaf að 0 eða 5
            long = round(long, 3),  #rúnna að 3 aukastaf
            eigandi,
            postnumer) %>% 
  group_by(lat, long, postnumer, eigandi) %>% 
  tally() %>% #gefur n: stæði per rúnnað hnit
  group_by(eigandi) %>% 
  mutate(n_eig = sum(n)) #gefur total stæði per eiganda
  
# Gera leaflet kort fyrir öll stæði með rúnnuð gögn
parking_map_all <- leaflet(height = 600, width = 800,
                           parking_plot_data) %>% 
  addTiles(urlTemplate = "http://tile.stamen.com/toner/{z}/{x}/{y}.png") %>%
  addCircles(lng = ~jitter(long), # jitter til að koma 
             lat = ~jitter(lat),  # í veg fyrir overlaps
             popup = ~paste(paste('Fjöldi:', n),
                            paste('Postnumer:',postnumer),
                            paste('Eigandi:', eigandi), 
                            paste('á samtals:', n_eig),
                            sep = '<br>'),
             color = ~pal_eig(eigandi), 
             fillOpacity = 0.6,
             radius = ~n/2 + 12) %>% 
  addLegend(position = "topright", 
            pal = pal_eig,
            values = ~eigandi,
            opacity = 0.8,
            title = "Eigandi")



# # Static plots
# rvk_borders <- c(bottom  = min(d_parking$lat),
#                  top     = max(d_parking$lat),
#                  left    = min(d_parking$long),
#                  right   = max(d_parking$long))
# 
# map <- get_stamenmap(rvk_borders, zoom = 11, maptype = "toner")
# 
# # Plot Eigandi
# ggmap(map) +
#   geom_point(data = d_parking, 
#              aes(x = long, 
#                  y = lat, 
#                  col = eigandi),
#              size = 0.8,
#              alpha = 0.6) +
#   scale_color_brewer(palette = 'Set1')
# 
# # Plot Postnumer
# ggmap(map) +
#   geom_point(data = parking_plot_data, 
#              aes(x = long %>% jitter(), 
#                  y = lat %>% jitter(),
#                  col = factor(postnumer)),
#              size = 0.8,
#              alpha = 1)

