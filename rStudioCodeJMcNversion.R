# Calendar Heatmaps for 30-day ROI and 30-day Volatility

# install and load required packages

# install.packages('fansi', dependencies = TRUE) 
# install.packages('ggplot2', dependencies = TRUE)
# install.packages("readr", type="mac.binary", dependencies=TRUE)
# install.packages("plyr", dependencies=TRUE)
# install.packages("plotly", dependencies=TRUE)
# install.packages("rstatix",dependencies=TRUE)
# install.packages("ggstatsplot", dependencies=TRUE)
# install.packages("ggthemes", dependencies=TRUE)
# install.packages("chron", dependencies=TRUE)
# install.packages("gridExtra", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)


library(fansi)
library(readr)
library(ggplot2)
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
library(plyr)
library(plotly)
library(rstatix)
library(ggstatsplot)
library(ggthemes)
library(chron)
library(gridExtra)
library(dplyr)


heatmap_ROI <- read_csv(
  "/Users/lulujonathan/CryptoPriceResearchProject/BTC-seasonality-analysis/heatmap ROI.csv", 
  col_types = cols(
    ROI = col_number(), 
    date = col_date(format = "%d/%m/%Y")
    )
  )

View(heatmap_ROI) 

r2g <- c("#D61818", "#B5E384")
calendarHeat(
  heatmap_ROI$date, 
  heatmap_ROI$ROI, 
  ncolors = 2, 
  color = "r2g", 
  varname="Bitcoin 30-day ROI"
  )

heatmap_ROI20102014 = subset(
  heatmap_ROI, 
  date < as.Date('31/12/2014', "%d/%m/%Y")
  )

heatmap_ROI20152020 = subset(
  heatmap_ROI, 
  date >= as.Date('31/12/2014', "%d/%m/%Y")
  )

calendarHeat(
  heatmap_ROI20102014$date, 
  heatmap_ROI20102014$ROI, 
  ncolors = 2, 
  color = "r2g", 
  varname="Bitcoin 30-day ROI 2010-2014"
)

calendarHeat(
  heatmap_ROI20152020$date, 
  heatmap_ROI20152020$ROI,
  ncolors = 2, 
  color = "r2g", 
  varname="Bitcoin 30-day ROI 2015-2020"
) # getting an error here for no apparent reason...


# --------------------------------------------------------------
# Volatility Calendar Heatmap, use "Vty heatmap 2010-2014.csv" data:
  
# import data

heatmap_price1 <- read_csv(
  "/Users/lulujonathan/CryptoPriceResearchProject/BTC-seasonality-analysis/Vty heatmap 2010-2014.csv", 
  col_types = cols(
    VtyDayRet30d = col_number(), 
    date = col_date(format = "%d/%m/%Y")
    )
  )

View(heatmap_price1)

# create heatmap (2010-2014)

calendarHeat(
  heatmap_price1$date, 
  heatmap_price1$VtyDayRet30d, 
  ncolors = 2, 
  color = "r2g", 
  varname="Bitcoin 30-day Volatility (2010-14)"
  )


# ----------------------------------
# Volatility Heatmap part 2, use "Vty heatmap 2015-2020.csv" data:
  
# import data

heatmap_price2 <- read_csv(
  "/Users/lulujonathan/CryptoPriceResearchProject/BTC-seasonality-analysis/Vty heatmap 2015-2020.csv", 
  col_types = cols(
    VtyDayRet30d = col_number(), 
    Date = col_date(format = "%d/%m/%Y")
    )
  )

View(heatmap_price2)

# plot volatility heatmap (2015-2020)

calendarHeat(
  heatmap_price2$Date, 
  heatmap_price2$VtyDayRet30d, 
  ncolors = 2, 
  color = "r2g", 
  varname="Bitcoin 30-day Volatility (2015-20)"
  )


# ---------------------------------
# Monthly plots

# import data

month <- read_csv(
  "/Users/lulujonathan/CryptoPriceResearchProject/BTC-seasonality-analysis/Month.csv", 
  col_types = cols(
    Month = col_factor(
      levels = c(
        "January", 
        "February", 
        "March", 
        "April", 
        "May", 
        "June", 
        "July", 
        "August", 
        "September", 
        "October", 
        "November", 
        "December"
        )
      ), 
      PriceChange = col_number(),
      Year = col_factor(
        levels = c(
          "2010", 
          "2011", 
          "2012", 
          "2013", 
          "2014", 
          "2015", 
          "2016", 
          "2017", 
          "2018", 
          "2019", 
          "2020"
          )
        )
    )
  )

# plot without outliers

retMonthWithoutOutliers <- ggplot(
  month, 
  aes(
    x=Month, 
    y=PriceChange, 
    fill=Month
    )
  ) + 
  geom_boxplot(
    outlier.shape = NA, 
    alpha=0.3
    ) + 
  theme(legend.position="none") + 
  scale_y_continuous(limits = c(-30, 75)) + 
  ylab("% Price Change") + 
  ggtitle("Monthly Returns for Bitcoin, Oct 2010:Jan2020 - outliers excluded")

show(retMonthWithoutOutliers)

# plot with outliers

retMonthWithOutliers <- ggplot(
  month, 
  aes(
    x=Month, 
    y=PriceChange, 
    fill=Month
    )
  ) + 
  geom_violin() + 
  ylab("% Price Change") + 
  stat_summary(
    fun=mean, 
    geom="point", 
    shape=23, 
    size=2, 
    colour="blue"
    ) + 
  ggtitle("Monthly Bitcoin Returns Aug 2010-Jan 2020")

show(retMonthWithOutliers)

#--------------------------------------
#  Day-of-the-week plot

# import data

weekday <- read_csv(
  "/Users/lulujonathan/CryptoPriceResearchProject/BTC-seasonality-analysis/weekday.csv", 
  col_types = cols(
    Change = col_number(), 
    PriceUSD = col_number(),  
    DayOfTheWeek = col_factor(
      levels = c(
        "Sunday", 
        "Monday", 
        "Tuesday", 
        "Wednesday", 
        "Thursday", 
        "Friday", 
        "Saturday"
        )
      ), 
    date = col_date(format = "%d/%m/%Y")
    )
  )

# Plot returns by day of the week

retDyOfWk <- ggplot(
  weekday, 
  aes(
    x=DayOfTheWeek, 
    y=Change, 
    fill=DayOfTheWeek
    )
  ) + 
  geom_violin() + 
  ylab("% Price Change") + 
  stat_summary(
    fun=mean, 
    geom="point", 
    shape=23, 
    size=2, 
    colour="blue"
    ) + 
  ggtitle("Daily Bitcoin Returns 18 Jul 2010-31 Jan 2020")

show(retDyOfWk)

# ------------------------------------------------------------------------------------------------------
#  1-hour/4-hour heatmaps for Average Returns, Volatility and Candle Body Ratio

# BitMEX data is from Cryptodatum.io/TradingView - both require subscription or involve a cost. 

# Import the data into R Studio, bitmex1h is the name of the csv file for the hourly price data for BTC-USD:

bitmex1h <- read_csv(
  "/Users/lulujonathan/CryptoPriceResearchProject/BTC-seasonality-analysis/bitmex1h.csv", 
  col_types = cols(
    Candle_Body_Ratio = col_number(), 
    Change = col_number(), 
    Close = col_skip(), 
    Date = col_date(format = "%d/%m/%Y"), 
    High = col_skip(), 
    Low = col_skip(), 
    Open = col_skip(), 
    Time = col_time(format = "%H:%M"), 
    Volatility = col_number(), 
    Volume = col_skip(), 
    Weekday = col_factor(
      levels = c(
        "Sunday", 
        "Monday", 
        "Tuesday", 
        "Wednesday", 
        "Thursday", 
        "Friday", 
        "Saturday"
        )
      )
    )
  )

View(bitmex1h)

# Plot 1-hour Average Return Heatmap for BTC-USD:

resultbm1h <- bitmex1h %>% convert_as_factor(Time)
resultbm1h <- resultbm1h %>% set_ref_level(
  "Time",
  ref = "00:00:00"
  )
resultbm1h <- resultbm1h %>% reorder_levels(
  "Time", 
  order = c(
    "00:00:00", "01:00:00", "02:00:00", 
    "03:00:00", "04:00:00", "05:00:00", 
    "06:00:00", "07:00:00", "08:00:00", 
    "09:00:00", "10:00:00", "11:00:00", 
    "12:00:00", "13:00:00", "14:00:00", 
    "15:00:00", "16:00:00", "17:00:00", 
    "18:00:00", "19:00:00", "20:00:00", 
    "21:00:00", "22:00:00", "23:00:00"
    )
  )

# removing outliers

Qbm1h <- quantile(
  resultbm1h$Change, 
  probs=c(.25, .75), 
  na.rm = FALSE
  )
iqrbm1h <- IQR(resultbm1h$Change)
upbm1h <-  Qbm1h[2]+1.5*iqrbm1h # Upper Range  
lowbm1h <- Qbm1h[1]-1.5*iqrbm1h # Lower Range
eliminatedRet1h <- subset(
  resultbm1h, 
  resultbm1h$Change > (lowbm1h) & resultbm1h$Change < (upbm1h)
  )

ret1h <- ggplot(
  eliminatedRet1h, 
  aes(
    x=Time, 
    y=Weekday, 
    fill = Change
    )
  ) +
  geom_tile(
    color = "white", 
    size = 0.1
    ) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis_c(
    name="Mean Price Change", 
    option = "plasma"
    ) + 
  coord_equal() +  
  theme_tufte(base_family="Helvetica") +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      vjust = 1, 
      hjust = 1
      )
    )

show(ret1h)

# Plot Average Hourly Volatility Heatmap:

resultvol1h <- bitmex1h %>% convert_as_factor(Time)
resultvol1h <- resultvol1h %>% set_ref_level(
  "Time", 
  ref = "00:00:00"
  )
resultvol1h <- resultvol1h %>% reorder_levels(
  "Time", 
  order = c(
    "00:00:00", "01:00:00", "02:00:00", 
    "03:00:00", "04:00:00", "05:00:00", 
    "06:00:00", "07:00:00", "08:00:00", 
    "09:00:00", "10:00:00", "11:00:00", 
    "12:00:00", "13:00:00", "14:00:00", 
    "15:00:00", "16:00:00", "17:00:00", 
    "18:00:00", "19:00:00", "20:00:00", 
    "21:00:00", "22:00:00", "23:00:00"
    )
  )

# removing outliers

Qvol1h <- quantile(
  resultvol1h$Volatility, 
  probs=c(.25, .75), 
  na.rm = FALSE
  )
iqrvol1h <- IQR(resultvol1h$Volatility)
upvol1h <-  Qvol1h[2]+1.5*iqrvol1h # Upper Range  
lowvol1h <- Qvol1h[1]-1.5*iqrvol1h # Lower Range
eliminatedVol1h <- subset(
  resultvol1h, 
  resultvol1h$Volatility > (lowvol1h) & resultvol1h$Volatility < (upvol1h)
  )
  
vol1h <- ggplot(
  eliminatedVol1h, 
  aes(
    x=Time, 
    y=Weekday, 
    fill = Volatility
    )
  ) + 
  geom_tile(
    color = "white", 
    size = 0.1
    ) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis_c(
    name="Average Price Volatility", 
    option = "plasma"
    ) + 
  coord_equal() +  
  theme_tufte(base_family="Helvetica") +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      vjust = 1, 
      hjust = 1
      )
    )

show(vol1h)

# Plot Average Hourly Candle Body Ratio Heatmap:

resultbm1h <- bitmex1h %>% convert_as_factor(Time)
resultbm1h <- resultbm1h %>% set_ref_level(
  "Time", 
  ref = "00:00:00"
  )
resultbm1h <- resultbm1h %>% reorder_levels(
  "Time", 
  order = c(
    "00:00:00", "01:00:00", "02:00:00", 
    "03:00:00", "04:00:00", "05:00:00", 
    "06:00:00", "07:00:00", "08:00:00", 
    "09:00:00", "10:00:00", "11:00:00", 
    "12:00:00", "13:00:00", "14:00:00", 
    "15:00:00", "16:00:00", "17:00:00", 
    "18:00:00", "19:00:00", "20:00:00", 
    "21:00:00", "22:00:00", "23:00:00"
    )
  )

View(resultbm1h)

cbr1h <- ggplot(
  resultbm1h, 
  aes(
    x=Time, 
    y=Weekday, 
    fill = Candle_Body_Ratio
    )
  ) +
  geom_tile(
    color = "white", 
    size = 0.1
    ) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis_c(
    name="Average Candle Body Ratio", 
    option = "plasma"
    ) + 
  coord_equal() +  
  theme_tufte(base_family="Helvetica") +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      vjust = 1, 
      hjust = 1
      )
    )

show(cbr1h)

# import 4-hour price data:

bitmex4h <- read_csv(
  "/Users/lulujonathan/CryptoPriceResearchProject/BTC-seasonality-analysis/bitmex4h.csv", 
  col_types = cols(
    Candle_Body_Ratio = col_number(), 
    Change = col_number(), 
    Close = col_skip(), 
    Date = col_date(format = "%d/%m/%Y"), 
    High = col_skip(),
    Time = col_time(format = "%H:%M"),
    Low = col_skip(), 
    Open = col_skip(), 
    Volatility = col_number(), 
    Volume = col_skip(), 
    Weekday = col_factor(
      levels = c(
        "Sunday", 
        "Monday", 
        "Tuesday", 
        "Wednesday", 
        "Thursday", 
        "Friday", 
        "Saturday"
        )
      )
    )
  )

View(bitmex4h)

# Plot Average 4-hour Return Heatmap:

resultbm4h <- bitmex1h %>% convert_as_factor(Time)
resultbm4h <- resultbm4h %>% set_ref_level(
  "Time", 
  ref = "00:00:00"
  )
resultbm4h <- resultbm4h %>% reorder_levels(
  "Time", 
  order = c(
    "00:00:00", "04:00:00", "08:00:00", 
    "12:00:00", "16:00:00", "20:00:00"
    )
  )

# removing outliers

Qbm4h <- quantile(
  resultbm4h$Change, 
  probs=c(.25, .75), 
  na.rm = FALSE
  )
iqrbm4h <- IQR(resultbm4h$Change)
upbm4h <-  Qbm4h[2]+1.5*iqrbm4h # Upper Range  
lowbm4h <- Qbm4h[1]-1.5*iqrbm4h # Lower Range
eliminatedRet4h <- subset(
  resultbm4h, 
  resultbm4h$Change > (lowbm4h) & resultbm4h$Change < (upbm4h)
  )

ret4h <- ggplot(
  eliminatedRet4h, 
  aes(
    x=Time, 
    y=Weekday, 
    fill = Change
    )
  ) +  
  geom_tile(
    color = "white", 
    size = 0.1
    ) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis_c(
    name="Mean Price Change", 
    option = "plasma"
    ) + 
  coord_equal() +  
  theme_tufte(base_family="Helvetica") +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      vjust = 1, 
      hjust = 1
      )
    )

show(ret4h)

# Plot Average 4-hour volatility:
  
resultvol4h <- bitmex4h %>% convert_as_factor(Time)
resultvol4h <- resultvol4h %>% set_ref_level(
  "Time", 
  ref = "00:00:00"
  )
resultvol4h <- resultvol4h %>% reorder_levels(
  "Time", 
  order = c(
    "00:00:00", "04:00:00", "08:00:00", 
    "12:00:00", "16:00:00", "20:00:00"
    )
  )

View(resultvol4h)

# removing outliers

Qvol4h <- quantile(
  resultvol4h$Volatility, 
  probs=c(.25, .75), 
  na.rm = FALSE
  )
iqrvol4h <- IQR(resultvol4h$Volatility)
upvol4h <-  Qvol4h[2]+1.5*iqrvol4h # Upper Range  
lowvol4h <- Qvol4h[1]-1.5*iqrvol4h # Lower Range
eliminatedVol4h <- subset(
  resultvol4h, 
  resultvol4h$Volatility > (lowvol4h) & resultvol4h$Volatility < (upvol4h)
  )

vol4h <- ggplot(
  eliminatedVol4h, 
  aes(
    x=Time, 
    y=Weekday, 
    fill = Volatility
    )
  ) + 
  geom_tile(
    color = "white", 
    size = 0.1
    ) +
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis_c(
    name="Average Price Volatility", 
    option = "plasma"
    ) + 
  coord_equal() +  
  theme_tufte(base_family="Helvetica") +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      vjust = 1, 
      hjust = 1
      )
    )

show(vol4h)

# Plot Average 4-hour Candle Body Ratio:

resultbm4h <- bitmex4h %>% convert_as_factor(Time)
resultbm4h <- resultbm4h %>% set_ref_level(
  "Time", 
  ref = "00:00:00"
  )
resultbm4h <- resultbm4h %>% reorder_levels(
  "Time", 
  order = c(
    "00:00:00", "04:00:00", "08:00:00", 
    "12:00:00", "16:00:00", "20:00:00"
    )
  )

View(resultbm4h)
  
cbr4h <- ggplot(
  resultbm4h, 
  aes(
    x=Time, 
    y=Weekday, 
    fill = Candle_Body_Ratio
    )
  ) +  
  geom_tile(
    color = "white", 
    size = 0.1
    ) + 
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) + 
  scale_fill_viridis_c(
    name="Average Candle Body Ratio", 
    option = "plasma"
    ) + 
  coord_equal() +  
  theme_tufte(base_family="Helvetica") +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      vjust = 1, 
      hjust = 1
      )
    )

show(cbr4h)




