library("ggplot2")
require("XLConnect")
require("dplyr")


wb = loadWorkbook("D:\\DV\\data_1.xlsx",) 
df = readWorksheet(wb, sheet = "Sheet1", header = TRUE) 

#resolving headers
header2 <- readWorksheet(wb, sheet = "Sheet1", header = FALSE,
                         startRow = 2,endRow = 2)

h1 = c("", "", "Deaths.", "Deaths.", "Deaths.", "ROM.","ROM.",
       "ROM.")
header <- paste0(h1, header2)
header <- factor(header)

rm(h1, header2)


#making a readable dataframe
data_rows = df %>% slice(2:nrow(df))
names(data_rows) <- header

rm(header, df, wb)


#taking data from only first year
data_rows <- data_rows %>% slice(1:12)

# converting columns to numeric data type
data_rows$`Average size of army` <- as.numeric(data_rows$`Average size of army`)
data_rows$`Deaths.Wounds & injuries` <- as.numeric(data_rows$`Deaths.Wounds & injuries`)
data_rows$`Deaths.All other causes` <- as.numeric(data_rows$`Deaths.All other causes`)
data_rows$`Deaths.Zymotic diseases` <- as.numeric(data_rows$`Deaths.Zymotic diseases`)
data_rows$`ROM.Zymotic diseases` <- as.numeric(data_rows$`ROM.Zymotic diseases`)
data_rows$`ROM.Wounds & injuries` <- as.numeric(data_rows$`ROM.Wounds & injuries`)
data_rows$`ROM.All other causes` <- as.numeric(data_rows$`ROM.All other causes`)


# total number of deaths from various reasons
nDeadCasualties.Zymotic <- sum(data_rows$`Deaths.Zymotic diseases`)
nDeadCasualties.Wounds <- sum(data_rows$`Deaths.Wounds & injuries`)
nDeadCasualties.Other <- sum(data_rows$`Deaths.All other causes`)


#monthly average army size
avgArmySize <- mean(data_rows$`Average size of army`)


#calculating radius for each factor
rad.Zymotic <- sqrt((1000*nDeadCasualties.Zymotic/avgArmySize)/pi)
rad.Wound <- sqrt((1000*nDeadCasualties.Wounds/avgArmySize)/pi)
rad.Other <- sqrt((1000*nDeadCasualties.Other/avgArmySize)/pi)


#calculating new radius
rads.Other <- sqrt((1000*data_rows$`Deaths.All other causes`/avgArmySize)/pi)
rads.Wound <- sqrt((1000*data_rows$`Deaths.Wounds & injuries`/avgArmySize)/pi)
rads.Zymotic <- sqrt((1000*data_rows$`Deaths.Zymotic diseases`/avgArmySize)/pi)

data_rows["rads.Other"] <- rads.Other
data_rows["rads.Wound"] <- rads.Wound
data_rows["rads.Zymotic"] <- rads.Zymotic




## ToDO: try to change fill and and color setting
######test
plot_df <- data_rows
plot_df$Month <-factor(plot_df$Month)
plot_df <- plot_df[,c("Month","rads.Other", "rads.Wound", "rads.Zymotic")]

plt <- ggplot(plot_df, aes(x=plot_df$Month))
image = plt + geom_bar(aes(y = plot_df$rads.Zymotic), color = "black", 
               stat = "identity") + 
  geom_bar(aes(y = plot_df$rads.Wound), fill = "red", stat = "identity") + 
  geom_bar(aes(y = plot_df$rads.Other), fill = "blue", stat = "identity") +
  labs(                                             
    x = "", y = "", fill = "Causes of Mortality",
    title = "Diagram of the Causes of Mortality in the Army in The East
    \n April 1854 to March 1855"       
  ) + theme_bw(base_size = 12) +
  theme(                   
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  coord_polar(theta = "x")

rm(data_rows, plot_df, plt)

require(svglite)
ggsave(filename = "minnard.svg", plot=image, width = 10, height = 8)
