if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, readxl, ggplot2)

# Importazione dati ------------------------------------------------------------
umPrev <- read_excel("Dati/Previsioni della popolazione.xlsx", 
                                           skip = 1)
umPrev <- umPrev |> 
  select(Anno, `Limite inferiore intervallo di confidenza al 90% (5° percentile)`,
         `Scenario mediano`,
         `Limite superiore intervallo di confidenza al 90% (95° percentile)`)

# Dataviz ----------------------------------------------------------------------

umPrev |> 
  ggplot(aes(x = Anno)) +
  geom_ribbon(aes(ymin = `Limite inferiore intervallo di confidenza al 90% (5° percentile)`,
                  ymax = `Limite superiore intervallo di confidenza al 90% (95° percentile)`), fill = 'gray90') +
  geom_line(aes(y = `Scenario mediano`), linetype = 1) +
  labs(title = 'Previsioni di popolazione al 2050',
       subtitle = 'Regione Umbria',
       caption = 'Dati Demo.Istat') +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_line(colour = 'gray90'),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Data export ----
write_rds(umPrev, 'Dati/Export/umPrev.rds')
