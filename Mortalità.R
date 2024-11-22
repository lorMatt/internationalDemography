if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, readr, ggiraph, patchwork)

# Importazione dati ------------------------------------------------------------

tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1974.csv") |> 
  mutate(anno = '1974')
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1979.csv") |> 
  mutate(anno = '1979') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1984.csv") |> 
  mutate(anno = '1984') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1989.csv") |> 
  mutate(anno = '1989') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1994.csv") |> 
  mutate(anno = '1994') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti1999.csv") |> 
  mutate(anno = '1999') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2004.csv") |> 
  mutate(anno = '2004') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2009.csv") |> 
  mutate(anno = '2009') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2014.csv") |> 
  mutate(anno = '2014') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2019.csv") |> 
  mutate(anno = '2019') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2020.csv") |> 
  mutate(anno = '2020') |> 
  bind_rows(tavMort)
tavMort <- read_csv("Dati/Tavole mortalità/datiregionalicompleti2023.csv") |> 
  mutate(anno = '2023') |> 
  select(!Informazioni) |> 
  bind_rows(tavMort)

## filtro Umbria ----
umbMort <- tavMort |> 
  filter(Regione == 'Umbria')

# Dataviz ----------------------------------------------------------------------
## Speranza di vita alla nascita per sesso -------------------------------------
### Umbria ----
umbe0 <- umbMort |> 
  filter(Sesso == 'Maschi' & Età == 0 | Sesso == 'Femmine' & Età == 0) |> 
  ggplot(aes(x = anno, y = `Speranza di vita`, col = Sesso,
             dataid = anno, tooltip = `Speranza di vita`)) +
  geom_path(aes(group = Sesso)) +
  labs(title = 'Speranza di vita alla nascita',
       subtitle = 'Regione Umbria') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

## Speranza di vita a 65 anni per sesso -----------------------------------------
### Umbria ----
umbe65 <- umbMort |> 
  filter(Sesso == 'Maschi' & Età == 65 | Sesso == 'Femmine' & Età == 65) |> 
  ggplot(aes(x = anno, y = `Speranza di vita`, col = Sesso,
             dataid = anno, tooltip = `Speranza di vita`)) +
  geom_path(aes(group = Sesso)) +
  labs(title = 'Speranza di vita a 65 anni',
       subtitle = 'Regione Umbria') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

## Probabilità di morte alla nascita per sesso ---------------------------------
### Umbria ----
umbq0 <- umbMort |> 
  filter(Sesso == 'Maschi' & Età == 0 | Sesso == 'Femmine' & Età == 0) |> 
  ggplot(aes(x = anno, y = `Probabilità di morte (per mille)`,
             col = Sesso, dataid = anno, tooltip = `Probabilità di morte (per mille)`)) +
  geom_path(aes(group = Sesso)) +
  labs(title = 'Probabilità di morte alla nascita',
       subtitle = 'Regione Umbria') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

## Curva sopravviventi anno per anno ----
### Umbria ----
umbMort |> 
  filter(Sesso != 'Maschi e femmine') |> 
  filter(anno == 1974 | anno == 1994 | anno == 2014) |>
  ggplot(aes(x = Età, y = Sopravviventi, col = anno, group = anno)) +
  geom_line() +
  facet_wrap(~Sesso) +
  labs(title = 'Curva di sopravvivenza per anno',
       subtitle = 'Regione Umbria',
       caption = 'Dati Demo.Istat') +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
## Patchwork --------------------------------------------------------------------

### Umbria ----
umbe0 + umbe65 + umbq0

# Data export ------------------------------------------------------------------
write_rds(umbMort, 'Dati/Export/umbMort.rds')

