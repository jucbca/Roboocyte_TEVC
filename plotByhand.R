library(tidyr)

list.files()
data = read.csv("DATA.csv")
## Load data file
data = read.delim("V-clamp_tstCell" , sep = "")


unique(data$Oocyte)
unique(data$HoldingV)
 

oocytei = data %>% dplyr::filter(date == 220411 & Oocyte == "A2" & HoldingV == "+10mV")
unique(oocytei$HoldingV)
oocytei = oocytei[,2:length(oocytei)]
head(oocytei)



My_Theme =  theme(
  axis.text.x = element_text(color = "black", size = 20, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
  axis.text.y = element_text(color = "black", size = 20, angle = 0, hjust = 0 , vjust = 0.5, face = "plain"),
  axis.title.x = element_text(color = "black", size = 30, angle = 0, hjust = 0.5, vjust = 0, face = "plain"), #element_blank()
  axis.title.y = element_text(color = "black", size = 30, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
  plot.title = element_blank(),#element_text(color = "black", size = 35, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
  legend.text = element_text(size = 20),
  legend.title = element_text(hjust = 0.1, size = 20),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(), #element_line()
  panel.grid.minor.x = element_blank()) 





tracePlot <- ggplot(oocytei, aes(`Time.s`, `Value`, color = `Solution`)) +
  geom_line() +
  ylab ("I (nA)") + 
  xlab ("Time (s)") + 
  labs(color = "ND96 (mOsm)") +
  ggtitle("Uninjected Oocyte. Voltage Clamp. Vh = -60mV") +
  My_Theme
print(tracePlot)

ggsave( "200411-A2+10mV.png" )

write.csv(A12, "A12.csv", row.names = FALSE)

