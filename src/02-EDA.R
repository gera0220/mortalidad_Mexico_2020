# Librerías ----
library(tidyverse)

# Carga de datos ----
homicidios_estado <- read_csv('data/interim/homicidios_estatales.csv')

lista_mx <- read_csv('data/raw/diccionario_especifico/lista_mexicana.csv')

defunciones <- read_csv('data/raw/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2020.csv')

# Homicidios por estado dividido por sexo ----
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

# Homicidios femeninos por estado -----
plot_homicidios_estado_fem <- homicidios_estado %>% 
  filter(tipo_pob == 'Femenina') %>% 
  mutate(nom_loc = fct_reorder(nom_loc, tasa)) %>% 
  ggplot(aes(tasa, nom_loc)) +
  geom_col(width = 0.8,
           fill = '#c994c7') +
  labs(title = 'Tasa de Homicidios Femeninos por Estado 2020',
       x = 'Homicidios por cada 100,000 mujeres',
       caption = 'Datos obtenidos de https://www.inegi.org.mx/programas/mortalidad/#Datos_abiertos') +
  theme(axis.title.y = element_blank())

ggsave('figs/homicidios_nacional_fem.png', plot = plot_homicidios_estado_fem, height = 6, width = 10)


# Homicidios masculinos por estado ----
plot_homicidios_estado_mas <- homicidios_estado %>% 
  filter(tipo_pob == 'Masculina') %>% 
  mutate(nom_loc = fct_reorder(nom_loc, tasa)) %>% 
  ggplot(aes(tasa, nom_loc)) +
  geom_col(width = 0.8,
           fill = '#9ecae1') +
  labs(title = 'Tasa de Homicidios Masculinos por Estado 2020',
       x = 'Homicidios por cada 100,000 hombres',
       caption = 'Datos obtenidos de https://www.inegi.org.mx/programas/mortalidad/#Datos_abiertos') +
  theme(axis.title.y = element_blank())

ggsave('figs/homicidios_nacional_mas.png', plot = plot_homicidios_estado_mas, height = 6, width = 10)

# Defunciones a nivel nacional ----

# Causas de muerte a nivel nacional
causas_muerte <- defunciones %>% 
  group_by(lista_mex, sexo) %>% 
  summarise(total_sexo = n()) %>% 
  left_join(lista_mx,
            by = c('lista_mex' = 'CVE')) %>% 
  mutate(sexo = as_factor(sexo)) %>% 
  left_join(defunciones %>% 
              group_by(lista_mex) %>% 
              summarise(total_conjunto = n()),
            by = 'lista_mex') %>% 
  arrange(desc(total_conjunto)) %>% 
  relocate(DESCRIP, .before = sexo)
  
plot_causas_muerte <- causas_muerte %>% 
  filter(sexo != '9') %>% 
  head(20) %>% 
  ungroup() %>% 
  mutate(DESCRIP = fct_reorder(DESCRIP, total_conjunto)) %>% 
  ggplot(aes(total_sexo, DESCRIP)) +
  geom_col(aes(fill = sexo),
           width = 0.8) + 
  labs(title = 'Mayores causas de muerte en México 2020',
       x = 'Total de casos',
       fill = 'Sexo',
       caption = 'Datos obtenidos de https://www.inegi.org.mx/programas/mortalidad/#Datos_abiertos') +
  scale_fill_manual(values = c('#9ecae1', '#c994c7'),
                    labels = c('Masculino', 'Femenino')) +
  theme(axis.title.y = element_blank())

ggsave('figs/causas_muerte_nacional.png', plot = plot_causas_muerte, height = 4, width = 8)

