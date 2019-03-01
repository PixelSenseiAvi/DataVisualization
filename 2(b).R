library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(pander)
library(ggplot2)
require(XLConnect)

wb = loadWorkbook("D:\\DV\\minard-data.xlsx") 
df = readWorksheet(wb, sheet = "Sheet1", header = TRUE)

cities = df[1:20, 1:3]
temps = df[1:9, 4:8]
troops = df[,9:13]

rm(wb)

dd <- paste(as.character(temps$DAY), temps$MON, "1812", sep = "")
dd[5]<- NA
dt <- dmy(dd)
temps$date <- dt

rm(dd, dt, df)

#plot
#"LONC" "LATC" "CITY"
#"LONP" "LATP" "SURV" "DIR"  "DIV" 
#"LONT" "TEMP" "DAYS" "MON"  "DAY"  "date"
plt <- ggplot() +
  geom_path(data = troops, aes(x = LONP, y = LATP, group = DIV, 
                               color = DIR, size = SURV),
            lineend = "round") +
  geom_point(data = cities, aes(x = LONC, y = LATC),
             color = "#DC5B44") +
  geom_text_repel(data = cities, aes(x = LONC, y = LATC, label = CITY),
                  color = "#DC5B44", family = "Open Sans Condensed Bold") +
  scale_size(range = c(0.5, 15)) + 
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  labs(x = NULL, y = NULL) + 
  guides(color = FALSE, size = FALSE) + theme_nothing()

# TODO: map later

library(dplyr)

temps.nice <- temps %>%
  mutate(nice.label = paste0(TEMP, "°, ", MON, ". ", DAY))

temps.plot <- ggplot(data = temps.nice, aes(x = LONT, y = TEMP)) +
  geom_line() +
  geom_label(aes(label = nice.label),
             family = "serif", size = 2.5) + 
  labs(x = NULL, y = "° Celsius") +
  scale_x_continuous(limits = ggplot_build(plt)$layout$panel_ranges[[1]]$x.range) +
  scale_y_continuous(position = "right") +
  coord_cartesian(ylim = c(-35, 5)) +  # Add some space above/below
  theme_bw(base_family = "serif") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks = element_blank(),
        panel.border = element_blank())

plt.combined <- gtable_rbind(ggplotGrob(plt), ggplotGrob(temps.plot))
grid::grid.newpage()
grid::grid.draw(plt.combined)
