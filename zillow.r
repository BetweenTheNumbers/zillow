library(ggplot2)
#library(ggrepel)
values <- read.csv('./data/Zip_Zhvi_AllHomes.csv', header = TRUE, fileEncoding = 'UTF-8-BOM')
rents <- read.csv('./data/Zip_Zri_AllHomesPlusMultifamily_Summary.csv', header = TRUE, fileEncoding = 'UTF-8-BOM')

values$RegionName <- as.factor(values$RegionName)
rents$RegionName <- as.factor(rents$RegionName)

merged <- merge(x = rents, y = values, by.x = 'RegionName', by.y = 'RegionName')

merged$X2020.01 <- merged$X2020.01 / 1000
merged$Zri <- merged$Zri / 1000
merged$VtoR <- merged$X2020.01 / (merged$Zri * 12)
merged$County <- gsub(' County', '', merged$County)

selection <- merged[merged$State.x == 'MD' 
                    & merged$County %in% c('Howard', 'Anne Arundel', 'Harford', 'Baltimore City', 'Baltimore') 
                    & merged$X2020.01 <= 500,]

mid <- median(selection$VtoR)

ggplot(selection, aes(x = Zri * 1000, y = VtoR, col = County)) + 
  geom_text(aes(label = paste(City.x, ' ', RegionName)), size = 3.5) +
  geom_smooth(method = "lm", col = "black", size = 0.5) + 
  labs(title = "Rent Indicies vs. Value:Rent Ratio", subtitle = "by Zip Code", y = "Value:Rent Ratio", x = "Rent Index", col = 'County', caption = "source:    zillow.com/research/data")
