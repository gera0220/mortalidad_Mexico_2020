# Librerías ----

library(tidyverse)

# Crear operador notin
'%notin%' <- Negate('%in%')

# Carga de datos útiles ----

diccionario_general <- read_csv('data/raw/diccionario_general/diccionario_datos_defunciones_registradas_2020.csv',
         locale = locale(encoding = 'ISO-8859-1')) %>% 
  select('NOMBRE_CAMPO', 'NEMÓNICO', 'RANGO_CLAVES')

diccionario_entidades <- read_csv('data/raw/diccionario_especifico/entidad_municipio_localidad_2020.CSV',
                                  locale = locale(encoding = 'ISO-8859-1'), show_col_types = FALSE)

defunciones <- read_csv('data/raw/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2020.csv',
                        show_col_types = FALSE) %>% 
  select('ent_regis', 'mun_regis', 'ent_ocurr', 'mun_ocurr', 'tloc_ocurr', 'causa_def',
          'lista_mex', 'sexo', 'edad', 'dia_ocurr', 'mes_ocurr', 'anio_ocur',
          'ocupacion', 'escolarida', 'edo_civil', 'presunto', 'sitio_ocur',
          'vio_fami', 'cond_act', 'par_agre')

iter_nalcsv20 <- read_csv('data/raw/ITER_NALCSV20.csv')

# Creación de df útiles ----

# Guardar estados y municipios de México. Filtrar datos para Chihuahua
estados <- diccionario_entidades %>% 
  filter(cve_loc == '0000', cve_mun == '000') %>% 
  select(cve_ent, nom_loc)

municipios_mexico <- diccionario_entidades %>% 
  filter(cve_loc == '0000') %>% 
  select(cve_ent, cve_mun, nom_loc) %>% 
  left_join(iter_nalcsv20 %>%
              filter(NOM_LOC == 'Total del Municipio'),
            by = c('cve_ent' = 'ENTIDAD', 'nom_loc' = 'NOM_MUN')) %>% 
  select(cve_ent, nom_loc, POBTOT, POBFEM, POBMAS)

municipios_chihuahua <- municipios_mexico %>% 
  filter(cve_ent == '08')

# Pegar población total, femenina y masculina al df estados
estados_poblacion <- estados %>%
  left_join(iter_nalcsv20 %>% 
              select(ENTIDAD, NOM_LOC, POBTOT, POBFEM, POBMAS) %>% 
              filter(NOM_LOC == 'Total de la Entidad'),
            by = c('cve_ent' = 'ENTIDAD')) %>% 
  select(cve_ent, nom_loc, POBTOT, POBFEM, POBMAS) %>% 
  mutate(POBFEM = as.numeric(POBFEM),
         POBMAS = as.numeric(POBMAS)) %>% 
  rename(Total = 'POBTOT',
         Femenina = 'POBFEM',
         Masculina = 'POBMAS') %>% 
  pivot_longer(cols = c('Total', 'Femenina', 'Masculina'),
               names_to = 'tipo_pob',
               values_to = 'poblacion')

write_csv(estados_poblacion, 'data/interim/poblacion_estatal.csv')

# Homicidios por estado ----

# Se crea el tibble de homicidios totales
homicidios_total <- defunciones %>% 
  filter(ent_regis %notin% c('88', '99'),
         lista_mex == '55') %>% 
  group_by(ent_regis) %>% 
  summarise(total = n())

# Se divide ahora los homicidios por sexo
homicidios_sexo <- defunciones %>% 
  filter(ent_regis %notin% c('88', '99'),
         lista_mex == '55') %>% 
  group_by(ent_regis, sexo) %>% 
  summarise(total = n()) %>% 
  pivot_wider(names_from = sexo,
              values_from = total)

# Se pegan los homicidios totales y por sexo
homicidios_full <- homicidios_total %>% 
  left_join(homicidios_sexo,
            by = 'ent_regis') %>% 
  rename(Total = 'total',
         Masculina = '1',
         Femenina = '2',
         'No identificado' = '9') %>% 
  pivot_longer(cols = c(2:5),
               names_to = 'tipo_pob',
               values_to = 'homicidios')

# Se agregan los nombres de estado, población total y por sexo, así como la tasa
# por cada 100,000 habitantes
homicidios_estado <- estados_poblacion %>% 
  left_join(homicidios_full,
            by = c('cve_ent' = 'ent_regis',
                   'tipo_pob' = 'tipo_pob')) %>% 
  mutate(tasa = (homicidios / poblacion) * 100000) %>% 
  filter(cve_ent %notin% c('33', '34', '35', '88', '99'))

write_csv(homicidios_estado, 'data/interim/homicidios_estatales.csv')
