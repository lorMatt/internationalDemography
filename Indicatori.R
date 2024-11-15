if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, readxl, plotly)

# Importazione dati ------------------------------------------------------------
nat <- read_excel("Dati/Indicatori_demografici.xls", 
                  sheet = "quoziente_di_natalità", skip = 1) |> 
       rename('geo' = '...1')
mort <- read_excel("Dati/Indicatori_demografici.xls", 
                   sheet = "quoziente_di_mortalità", skip = 1) |>
        rename('geo' = '...1')
cresc <- read_excel("Dati/Indicatori_demografici.xls", 
                    sheet = "crescita_naturale", skip = 1) |> 
         rename('geo' = '...1')
migInt <- read_excel("Dati/Indicatori_demografici.xls", 
                     sheet = "saldo_migratorio_interno", skip = 1) |> 
          rename('geo' = '...1')
migEst <- read_excel("Dati/Indicatori_demografici.xls", 
                     sheet = "saldo_migratorio_con_l_estero", skip = 1) |> 
          rename('geo' = '...1')
mig <- read_excel("Dati/Indicatori_demografici.xls", 
                  sheet = "saldo_migratorio_totale", skip = 1) |> 
       rename('geo' = '...1')

## Tibble Umbria (merge) -------------------------------------------------------
umdf <- nat |>
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Quoziente di natalità')
umdf <- mort |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Quoziente di mortalità') |> 
  bind_rows(umdf)
umdf <- cresc |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Crescita naturale') |> 
  bind_rows(umdf)
umdf <- mig |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Saldo migratorio') |> 
  bind_rows(umdf)
umdf <- migInt |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Saldo migratorio interno') |> 
  bind_rows(umdf)
umdf <- migEst |> 
  filter(geo == 'Umbria') |> 
  mutate(geo = 'Saldo migratorio con l\'estero') |> 
  bind_rows(umdf)


umdf2 <- data.frame(t(umdf[-1])) # trasposiz
colnames(umdf2) <- umdf$geo # nomi variabili
umdf <- umdf2
rm(umdf2)
umdf <- rownames_to_column(umdf, var = 'anno')
umdf$anno <- dmy(sprintf("01-01-%s",umdf$anno))

umts <- umdf |> 
  pivot_longer(cols = 2:7, names_to = 'var')

## Tibble Italia (merge) -------------------------------------------------------
itdf <- nat |>
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Quoziente di natalità')
itdf <- mort |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Quoziente di mortalità') |> 
  bind_rows(itdf)
itdf <- cresc |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Crescita naturale') |> 
  bind_rows(itdf)
itdf <- mig |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Saldo migratorio') |> 
  bind_rows(itdf)
itdf <- migInt |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Saldo migratorio interno') |> 
  bind_rows(itdf)
itdf <- migEst |> 
  filter(geo == 'ITALIA') |> 
  mutate(geo = 'Saldo migratorio con l\'estero') |> 
  bind_rows(itdf)
rm(mig, nat, mort, migInt, migEst, cresc)

itdf2 <- data.frame(t(itdf[-1])) # trasposiz
colnames(itdf2) <- itdf$geo # nomi variabili
itdf <- itdf2
rm(itdf2)
itdf <- rownames_to_column(itdf, var = 'anno')
itdf$anno <- dmy(sprintf("01-01-%s",itdf$anno))

itts <- itdf |> 
  pivot_longer(cols = 2:7, names_to = 'var')

# Dataviz ----------------------------------------------------------------------
## Natalità e mortalità ----
### Umbria ----
umts |>
  filter(var == 'Quoziente di mortalità' | var == 'Quoziente di natalità' | var == 'Crescita naturale') |> 
  ggplot(aes(x = anno, y = value, col = var)) +
  geom_line() +
  geom_hline(yintercept = 0, col = 'gray60') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
### Italia ----
itts |>
  filter(var == 'Quoziente di mortalità' | var == 'Quoziente di natalità' | var == 'Crescita naturale') |> 
  ggplot(aes(x = anno, y = value, col = var)) +
  geom_line() +
  geom_hline(yintercept = 0, col = 'gray60') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
## Migrazione ----
### Umbria ----
umts |> 
  filter(var == 'Saldo migratorio con l\'estero' | var == 'Saldo migratorio interno' | var == 'Saldo migratorio') |> 
  ggplot(aes(x = anno, y = value, col = var)) +
  geom_line() +
  geom_hline(yintercept = 0, col = 'gray60') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
### Italia ----
itts |> 
  filter(var == 'Saldo migratorio con l\'estero' | var == 'Saldo migratorio interno' | var == 'Saldo migratorio') |> 
  ggplot(aes(x = anno, y = value, col = var)) +
  geom_line() +
  geom_hline(yintercept = 0, col = 'gray60') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

