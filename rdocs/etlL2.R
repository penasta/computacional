if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,broom,lubridate,corrplot)

climbing <- read_csv("rdocs/dados/climbing_statistics.csv")
weather <- read_csv("rdocs/dados/Rainier_Weather.csv")
convert <- function(x) (x-32) * 5/9
shift <- function(x) x - mean(x)
dados <- inner_join(climbing, weather) %>%
  select(-matches("Percent|Battery")) %>%
  filter(Attempted >= Succeeded,
         Route != "glacier only - no summit attempt",
         Route != "Unknown") %>%
  mutate(`Temperature AVG`= convert(`Temperature AVG`),
         Cleaver = Route=="Disappointment Cleaver",
         Date = mdy(Date)) %>%
  select(Date, Succeeded, everything()) %>%
  rename(Data = Date,
         Rota = Route,
         Sucessos = Succeeded,
         Tentativas = Attempted,
         Temperatura = `Temperature AVG`,
         Umidade_relativa = `Relative Humidity AVG`,
         Velocidade_vento = `Wind Speed Daily AVG`,
         Direc_vento = `Wind Direction AVG`,
         Radiacao_solar = `Solare Radiation AVG`) %>%
  group_by(Data, Rota) %>%
  mutate(Sucessos = sum(Sucessos),
         Tentativas = sum(Tentativas)) %>%
  distinct() %>%
  ungroup()
rm(climbing,weather,convert,shift)
saveRDS(dados,"rdocs/dados/dados.rds")
