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

FXit <- read_excel("Dati/Fx italiane.xlsx", 
                                  skip = 1) |> 
  filter(Territorio == 'Umbria') |> 
  select(!Note & !`Fino a 14 anni` & !`50 anni e oltre`) |> 
  mutate(citt = 'Madri italiane')
FXstra <- read_excel("Dati/Fx straniere.xlsx", 
                     skip = 1) |> 
  filter(Territorio == 'Umbria') |> 
  select(!Note & !`Fino a 14 anni` & !`50 anni e oltre`) |> 
  mutate(citt = 'Madri straniere')

## Merge
fecdf <- full_join(tftCitt, etaCitt) |> 
  select(!Note) |> 
  pivot_longer(cols = `Tasso di fecondità totale, madri italiane`:`Età media al parto, tutte le madri`,
               names_to = 'var') |> 
  rename(anno = `Anno di evento`)

FX <- bind_rows(FXit, FXstra) |> 
  pivot_longer(cols = `15 anni`:`49 anni`, names_to = 'Età', values_to = 'FX') |> 
  mutate(Età = as.numeric(gsub(' anni', '', Età)))

rm(etaCitt, FXit, FXstra, tftCitt)
# Dataviz ----------------------------------------------------------------------

## TFT
fecdf |> 
  filter(var == 'Tasso di fecondità totale, madri italiane'
         | var == 'Tasso di fecondità totale, madri straniere'
         | var == 'Tasso di fecondità totale, tutte le madri') |> 
  mutate(var = gsub('Tasso di fecondità totale, ', '', var, fixed = T)) |> 
  ggplot(aes(x = anno, y = value)) +
  geom_line(data = ~. |> filter(var == 'Tutte le madri'), aes(linetype = var)) +
  geom_line(data = ~. |> filter(var != 'Tutte le madri'), aes(col = var)) +
  labs(title = 'Tasso di fecondità totale') +
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)) +
  facet_wrap(~Territorio) +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

## Età media al parto
fecdf |> 
  filter(var == 'Tasso di fecondità totale, madri italiane'
         | var == 'Tasso di fecondità totale, madri straniere'
         | var == 'Tasso di fecondità totale, tutte le madri') |> 
  mutate(var = gsub('Età media al parto, m', 'M', var, fixed = T),
         var = gsub('Età media al parto, t', 'T', var, fixed = T)) |> 
  ggplot(aes(x = anno, y = value)) +
  geom_line(data = ~. |> filter(var == 'Tutte le madri'), aes(linetype = var)) +
  geom_line(data = ~. |> filter(var != 'Tutte le madri'), aes(col = var)) +
  labs(title = 'Tasso di fecondità totale') +
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2023)) +
  labs(title = 'Tasso di fecondità totale',
       subtitle = 'Serie storica 2002-2023',
       caption = 'Dati Demo.Istat') +
  facet_wrap(~Territorio) +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.title = element_blank())

## FX ita/stra ----

FX |>
  filter(`Anno di evento` == 2013 | `Anno di evento` == 2023) |> 
  ggplot(aes(x = Età, y = FX, col = citt, group = citt)) +
  geom_line() +
  facet_wrap(~`Anno di evento`) +
  labs(title = 'Tassi specifici di fecondità per età',
       subtitle = 'Regione Umbria',
       caption = 'Dati Demo.Istat') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Data export ----
write_rds(fecdf, 'Dati/Export/fecdf.rds')
write_rds(FX, 'Dati/Export/FX.rds')