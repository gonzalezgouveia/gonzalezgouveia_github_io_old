library(tidyverse)
library(trelliscopejs)

# reading data
path <- "C:/Users/fafa/Desktop/proyecto-wine/wine-reviews/winemag-data-130k-v2.csv"
winedata <- read_csv(file = path, col_names = TRUE)

# summary of data
head(winedata)
summary(winedata)

# visualizing raw data
set.seed(12321)
ggplot(sample_n(winedata, size=10000), 
       aes(x=price, 
           y=jitter(points, factor = 3), 
           color=country)) + 
  geom_point(size=2) + 
  xlab('Precio') + 
  ylab('Puntuación') +
  ggtitle('Exploración por país')
ggsave('./../Desktop/proyecto-wine/images/all_wine_country.png')

# cleaning data
clean_data <- winedata %>%
  select(country, points, price) %>%
  drop_na() %>%                        # quitando los nulos
  group_by(country) %>%
  filter(n()>2000) %>%                 # filtrando
  ungroup() %>%
  mutate(log_price = log(price))       # log price

# plot of processed data
set.seed(12321)
ggplot(sample_n(clean_data, size=1000),
       aes(x=jitter(log_price, factor = 3), 
           y=jitter(points, factor = 3), 
           color=country)) + 
  geom_point(size=2) +
  xlab('log(Precio)') + 
  ylab('Puntuación') +
  ggtitle('Revisiones de vinos por país')
ggsave('./../Desktop/proyecto-wine/images/clean_wine_country.png')

# querying models by country

all_models <- clean_data %>% 
  group_by(country) %>% 
  summarise(n_obs = n(),
            b = lm(points ~ log(price))$coefficients[1],
            m = lm(points ~ log(price))$coefficients[2]) 

# trelliscope dashboard
path_trelliscope <- "C:/Users/fafa/Desktop/proyecto-wine/trelliscope/"
ggplot(clean_data, 
       aes(x=log(price), y=points)) +
  geom_point() + 
  geom_smooth(method=lm, se = FALSE) +
  facet_trelliscope(~ country, nrow = 1, ncol = 3,
                    as_plotly = TRUE,
                    path = path_trelliscope,
                    self_contained=TRUE)
