if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, readxl, ggplot2)

# Importazione dati ------------------------------------------------------------
tftCitt <- read_excel("Dati/TFT per cittadinanza.xlsx",
                      skip = 1, n_max = 496) |> 
           filter(Territorio == 'Umbria' | Territorio == 'Italia')
etaCitt <- read_excel("Dati/Età media al parto per cittadinanza.xlsx", 
                      skip = 1, n_max = 506) |> 
           filter(Territorio == 'Umbria' | Territorio == 'Italia')

## Merge
fecdf <- full_join(tftCitt, etaCitt) |> 
  select(!Note) |> 
  pivot_longer(cols = `Tasso di fecondità totale, madri italiane`:`Età media al parto, tutte le madri`,
               names_to = 'var') |> 
  rename(anno = `Anno di evento`)

# Dataviz ----------------------------------------------------------------------

## TFT
fecdf |> 
  filter(var == 'Tasso di fecondità totale, madri italiane'
         | var == 'Tasso di fecondità totale, madri straniere'
         | var == 'Tasso di fecondità totale, tutte le madri') |> 
  ggplot(aes(x = anno, y = value, col = var)) +
  geom_line() +
  facet_wrap(~Territorio) +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

## Età media al parto
fecdf |> 
  filter(var == 'Età media al parto, madri italiane'
         | var == 'Età media al parto, madri straniere'
         | var == 'Età media al parto, tutte le madri') |> 
  ggplot(aes(x = anno, y = value, col = var)) +
  geom_line() +
  facet_wrap(~Territorio) +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Data export ----
write_rds(fecdf, 'Dati/Export/fecdf.rds')
