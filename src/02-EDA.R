# Librerías ----
library(tidyverse)

# Carga de datos ----
homicidios_estado <- read_csv('data/interim/homicidios_estatales.csv')

# Gráfico

# Homicidios por estado dividido por sexo
homicidios_estado %>% 
  mutate(nom_loc = fct_reorder(nom_loc, tasa)) %>% 
  filter(tipo_pob != 'Total') %>% 
  ggplot(aes(tasa, nom_loc)) +
  geom_col(aes(fill = tipo_pob),
           width = 0.8,
           position = position_stack(reverse = FALSE)) +
  labs(title = 'Tasa de Homicidios por Estado 2020',
       x = 'Homicidios por cada 100,000 habitantes',
       fill = 'Tipo de población',
       caption = 'Datos obtenidos de https://www.inegi.org.mx/programas/mortalidad/#Datos_abiertos') +
  theme(axis.title.y = element_blank())

# Homicidios femeninos por estado
homicidios_estado %>% 
  filter(tipo_pob == 'Femenina') %>% 
  mutate(nom_loc = fct_reorder(nom_loc, tasa)) %>% 
  ggplot(aes(tasa, nom_loc)) +
  geom_col(width = 0.8,
           fill = '#c994c7') +
  labs(title = 'Tasa de Homicidios Femeninos por Estado 2020',
       x = 'Homicidios por cada 100,000 mujeres',
       caption = 'Datos obtenidos de https://www.inegi.org.mx/programas/mortalidad/#Datos_abiertos') +
  theme(axis.title.y = element_blank())

# Homicidios masculinos por estado
homicidios_estado %>% 
  filter(tipo_pob == 'Masculina') %>% 
  mutate(nom_loc = fct_reorder(nom_loc, tasa)) %>% 
  ggplot(aes(tasa, nom_loc)) +
  geom_col(width = 0.8,
           fill = '#9ecae1') +
  labs(title = 'Tasa de Homicidios Masculinos por Estado 2020',
       x = 'Homicidios por cada 100,000 hombres',
       caption = 'Datos obtenidos de https://www.inegi.org.mx/programas/mortalidad/#Datos_abiertos') +
  theme(axis.title.y = element_blank())
