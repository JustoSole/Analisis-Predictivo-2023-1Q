# Importacion paquetes ----
library(forecast)
library(tidyverse)
library(magrittr)
library(Metrics)
library(lubridate)
library(cowplot)
library('purrr')
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(DataExplorer)
library(corrplot)
library(tidyr)
library(RColorBrewer)
library(gridExtra) # Load the package

# importacion de datasets  ----

feriados = read.csv("C:/Users/Justo/OneDrive/Documents/R/store-sales-time-series-forecasting/holidays_events.csv")
oil = read.csv("C:/Users/Justo/OneDrive/Documents/R/store-sales-time-series-forecasting/oil.csv")
stores = read.csv("C:/Users/Justo/OneDrive/Documents/R/store-sales-time-series-forecasting/stores.csv")
train = read.csv("C:/Users/Justo/OneDrive/Documents/R/store-sales-time-series-forecasting/train.csv")
transactions  = read.csv("C:/Users/Justo/OneDrive/Documents/R/store-sales-time-series-forecasting/transactions.csv")

## _ ====


#BASIC EDA ----

## _ ====

head(train)

summary(train)

train$date = as.Date(train$date)
oil$date <- as.Date(oil$date)
transactions$date <- as.Date(transactions$date)


train$day <- train$date %>% day()
train$month <- train$date %>% month()
train$month_lab <- train$date %>% month(label = TRUE)
train$year <- train$date %>% year()
train$week <- train$date %>% week()
train$week_day <- train$date %>% wday(week_start = getOption("lubridate.week.start", 1), label = TRUE)


map_dbl(train, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

map_dbl(feriados, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

map_dbl(oil, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

map_dbl(stores, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()

map_dbl(transactions, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()



## OUTLIERS ----

#function to create histograms, taking two arguments: x=data and y=title
hist_function <- function(x, y){
  x_omit <- na.omit(x)
  
  hist(x_omit,
       main = y,
       xlab = NULL,
       breaks = 20,
       col = 'green',
       probability = T)
  
  #generating density curve overlay
  lines(density(x_omit), lwd = 2, col = "dark blue")
  
  #getting summary stats
  five_num <- summary(x_omit)
  #displaying five number summary (not showing the mean) 
  five_num <- as.matrix(five_num[-4])
  t(five_num)
}

#creating four histograms 
hist_function(df1$transactions, 'Transactions')
hist_function(df1$sales, 'Sales')
hist_function(df1$dcoilwtico, 'Oil Price')
hist_function(df1$onpromotion, 'On Promotion')


df1 %>% 
  arrange(desc(onpromotion)) %>%
  head(10)
  
unique(df1$onpromotion)

## OIL ----
### NA's OIL ----

oil %>% filter(is.na(oil$dcoilwtico) == TRUE)

# interpolar valores faltantes
oil <- oil %>%
  mutate(dcoilwtico = na.approx(dcoilwtico, rule = 2))

#### graficos oil ----

ggplot(oil, aes(x = date, y = dcoilwtico)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = "Año", y = "dcoilwtico")

oilseries <- ts(oil$dcoilwtico, start = c(2013, 1), end = c(2017, 12), frequency = 52*4)

# Decompose the time series
decomp <- stl(oilseries, s.window = "periodic")

# Plot the decomposition
autoplot(decomp) +
  labs(title = "Time Series Decomposition del oil price con frequencia semanal")
## _ ====

## FERIADOS ==== 

holidayevents = feriados

holidayevents[is.na(holidayevents$type)]

###NA's  ====

#populating missing holiday values with 'none' to indicate it wasnt a holiday
holidayevents[is.na(holidayevents$holiday),] <- 'None'

#renaming column 
holidayevents <- holidayevents %>%
  rename(holiday = type)

#changing holidays that were transferred to 'none', so that they are not correlated as a holiday
holidayevents <- holidayevents %>%
  mutate(holiday = ifelse(transferred == "True",
                          "None",
                          holiday))

#changing 'transfer' to 'holiday' becaues these are the actual days certain holidays were celebrated on
holidayevents <- holidayevents %>%
  mutate(holiday = ifelse(holiday == 'Transfer',
                          'holiday',
                          holiday))

holidayevents$date <- as.Date(holidayevents$date)

str(holidayevents)
### graficos ----

p1 <- feriados %>% ggplot()+geom_bar(aes(x = locale, fill = locale), colour = 'black', size = 1)+
  scale_fill_viridis_d()+
  labs(title = "Holiday locale")+
  guides(fill="none")

p2 <- feriados %>% ggplot()+geom_bar(aes(x = type, fill = type), colour = 'black', size = 1)+
  scale_fill_viridis_d()+
  labs(title = "Holiday type")+
  guides(fill="none")

plot_grid(plotlist = list(p1,p2), nrow = 1)

## _ ====

## Train/Sales ----

train$family %>% table() %>% as.data.frame()

#hay 33 familias con 90936 productos en cada una 

fam_data <- train %>% group_by(family) %>% summarise(mean_sales = round(mean(sales, na.rm=TRUE),2),
                                                     median_sales = round(median(sales, na.rm = TRUE),2),
                                                     IQR_sales = IQR(sales),
                                                     max_sales = max(sales),
                                                     total_sales = sum(sales))
fam_data


#histograma de ventas por familia

fam_data$family <- reorder(fam_data$family, fam_data$mean_sales, FUN = function(x) -x)

plot <- ggplot(fam_data, aes(x = family, y = mean_sales, fill = family)) +
  geom_col() +
  scale_color_manual(values = rainbow(length(unique(fam_data$family)))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Familia") +
  ylab("Ventas Medias") +
  ggtitle("Histograma de Ventas por Familia")

ggplotly(plot)

#10 mayores familias en base a la proporción de las ventas totales

library(plotly)
library(dplyr)
library(tidyr)

fam_data <- fam_data %>%
  mutate(prop_sales = total_sales / sum(total_sales)) %>%
  arrange(desc(prop_sales)) %>%
  head(10)

plot_ly(fam_data, x = ~prop_sales, y = ~family, type = "bar", 
        marker = list(color = ~prop_sales, colorscale = "Viridis"),
        text = ~paste("Proporción de Ventas Totales:", round(prop_sales * 100, 2), "%"),
        hoverinfo = "text") %>%
  layout(title = "10 Mayores Familias por Proporción de Ventas Totales",
         xaxis = list(title = "Proporción de Ventas Totales"),
         yaxis = list(title = "Familia"),
         font = list(size = 14),
         margin = list(l = 120, r = 40, b = 60, t = 80),
         height = 500,
         showlegend = FALSE)


# Promedio de ventas por día
daily_sales <- aggregate(train$sales, by = list(train$date), FUN = mean)

plt2 <- train %>% 
  group_by(date) %>% 
  summarise(avg_daily_sales = mean(sales),
            wday = week_day, .groups = "keep") %>% 
  filter(date <= '2013-04-01') %>% 
  ggplot()+
  geom_col(aes(x = date, y = avg_daily_sales, fill=wday))+
  scale_fill_viridis_d()+
  labs(x = 'Date',
       y = 'Averge daily sales',
       fill = 'Day of week')


# Promedio de ventas por semana
weekly_sales <- aggregate(train$sales, by = list(train$year, train$week), FUN = mean)

plt1 <- train %>% 
  group_by(week_day) %>% 
  summarise(avg_sales_by_day = mean(sales)) %>% 
  ggplot()+
  geom_col(aes(x=week_day, y=avg_sales_by_day, fill = week_day), size=1, colour="black")+
  scale_fill_viridis_d()+
  labs(title = "Average sales by day of the week",
       x = "Day of the week",
       y = "Average daily sales",
       fill = "Day of week")+
  guides(fill = "none")

plt2 <- train %>% 
  group_by(month_lab) %>% 
  summarise(avg_sales_by_day = mean(sales)) %>% 
  ggplot()+
  geom_col(aes(x=month_lab, y=avg_sales_by_day, fill = month_lab), size=1, colour="black")+
  scale_fill_viridis_d()+
  labs(title = "Average sales by month of the year",
       x = "Month",
       y = "Average monthly sales",
       fill = "Month")+
  guides(fill = "none")

plt3 <- train %>% 
  group_by(year) %>% 
  summarise(avg_sales_by_day = mean(sales)) %>% 
  ggplot()+
  geom_col(aes(x=year, y=avg_sales_by_day, fill = as_factor(year)), size=1, colour="black")+
  scale_fill_viridis_d()+
  labs(title = "Average sales by year",
       x = "Year",
       y = "Average monthly sales",
       fill = "Year")+
  guides(fill = "none")

plt4 <- train %>% 
  group_by(week) %>% 
  summarise(avg_sales_by_day = mean(sales)) %>% 
  ggplot()+
  geom_col(aes(x=week, y=avg_sales_by_day, fill = as_factor(week)), size=1, colour="black")+
  scale_fill_viridis_d()+
  labs(title = "Average sales by week",
       x = "Week",
       y = "Average weekly sales",
       fill = "Week")+
  guides(fill = "none")

plot_grid(plotlist = list(plt1,plt2, plt3, plt4), nrow = 2)

## Sales by state and type of store

df1 %>% 
  filter(train_test ==  'train') %>%
  group_by(type) %>%
  summarize(sales = sum(sales)) %>%
  ggplot(aes(x = type, y = sales, fill = type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_number(suffix = "M", scale  = 1e-06)) +
  geom_text(aes(label = paste(round(sales/ 1e6, 1), "M")), cex = 4, vjust = -.2) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  labs(title = 'Total Sales by Type', x = 'Type', y = 'Sales') 

#value counts of sales by state
df1 %>% 
  filter(train_test ==  'train') %>%
  group_by(state, type) %>%
  summarize(sales = sum(sales), .groups = 'drop') %>%
  ggplot(aes(x = reorder(state, -sales), y = sales, fill = type)) +
  geom_bar(stat = "identity", color = 'black') +
  scale_y_continuous(labels = label_number(suffix = "M", scale  = 1e-06)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  labs(title = 'Total Sales by State', x = 'State', y = 'Sales')

#time series descomposition de las ventas mensulaes 

daily_avg_sales <- train %>%
  group_by(date) %>%
  summarize(avg_sales = mean(sales))

# Create the time series
trainsales <- ts(daily_avg_sales$avg_sales, frequency = 12 * 4)

# Decompose the time series
decomp <- stl(trainsales, s.window = "periodic")

# Plot the decomposition
autoplot(decomp) +
  labs(title = "TSD de las ventas promedio diarias con fequencia mensual")

## Stores  ----

stores$type %>% 
  table() %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_col(aes(y = Freq, x = ., fill = as_factor(.)), colour='black', size=1)+
  scale_fill_viridis_d()+
  labs(title = 'Distribution of store types',
       fill = 'Type:',
       x = 'Store type')

left_join(stores, transactions,train, by = 'store_nbr') %>% group_by(type) %>% summarise(avg_trans = mean(transactions),
                                                                                   total_trans = sum(transactions),
                                                                                   sd_trans = sd(transactions),
                                                                                   total_sales = sum(sales))

df <- left_join(train, stores, by = 'store_nbr') 

df <- df %>% 
  mutate(cluster = as.factor(cluster), 
         type = as.factor(type))

ggplot(df, aes(x = cluster, y = reorder(sales, -sales), fill = type)) +
  geom_col() +
  labs(title = "Ventas por cluster y tipo",
       fill = "Type",
       x = "Cluster",
       y = "Sales") +
  scale_fill_viridis_d()


### boxplots para los stores por type ----

options(jupyter.plot_scale=1.5)

bp<- stores %>% ggplot(aes(x=type, y=total, fill=type)) +
  geom_boxplot() +
  labs(title='Boxplot Plot for Sales by Type',
       x='Store Type',
       y='Daily Total Sales',
       caption='Store Price')

sp<- stores %>%
  ggplot(aes(x=date, y=total, color=type)) +
  geom_point(size=2, alpha=0.25)  +
  geom_smooth(formula = total ~ date, method = "lm") + 
  #     scale_color_hue(l=40, c=35)+
  labs(title='Scatter Plot for Sales by Type',
       x='Date',
       y='Daily Total Sales',
       caption='Store Price')

grid.arrange(bp, sp)

## _ ====

## Outliers ----

### Stores sin vtas ----
missing_sales <- train %>%
  group_by(store_nbr) %>%
  summarize(missing_days = sum(diff(as.Date(date)) > 1), 
            start_date = as.Date(min(date)) + 1*!as.logical(sum(diff(as.Date(date)) > 1)))
unique(missing_sales$start_date)


### Familias sin vtas ----
#CUALES SON LAS FAMILIAS QUE NO SON VENDIDAAS POR ALGUNOS NEGOCIOS. 
#EL FORECAST PARA ESTOS NEGOCIOS VA A SER 0 VENTAS EN LOS PROX 15 DIAS

# Agrupar los datos por nbr_store y family, y sumar las ventas
df_sum <- train %>% 
  group_by(store_nbr, family) %>% 
  summarise(total_sales = sum(sales))

# Filtrar los negocios que no han vendido nada de alguna familia
df_no_sales <- df_sum %>% 
  filter(total_sales == 0)

df_no_sales

ggplot(df_no_sales, aes(x=df_no_sales$family)) +
  geom_bar()

## _------------------------- ====

## Correlaciones ----

### Variables numericas ----

options(repr.plot.width = 14, repr.plot.height = 8)

#choosing numeric data after removing incomplete observations
c1 <- cor(df1 %>% 
            filter(complete.cases(df1)) %>%
            select(sales, onpromotion, dcoilwtico, transactions, Year))

#creating correlation matrix                  
corrplot(c1,
         method = c('color'),
         addCoef.col = "black",
         addgrid.col = "black",
         tl.col = "black",
         order = 'hclust')

### Feriados/ventas ----

train$holiday <- 0
holidayevents$date <- as.Date(holidayevents$date, format="%Y-%m-%d")
for (i in 1:nrow(train)) {
  for (j in 1:nrow(holidayevents)) {
    if (train$date[i]==holidayevents$date[j] && holidayevents$holiday[j] == "Holiday") {
      train$holiday[i] <- 1
    }
  }
}
# Calcular los promedios
groupA <- train[train$holiday == 0,]
groupB <- train[train$holiday == 1,]
groupA_mean <- mean(groupA$sales)
groupB_mean <- mean(groupB$sales)
groupA_median <- median(groupA$sales)
groupB_median <- median(groupB$sales)

# Comparar los promedios
test <- wilcox.test(groupA$sales, groupB$sales)

# Imprimir los resultados
output <- data.frame("Feature"="holidays",
                     "Test Type"="Non-Parametric",
                     "AB Hypothesis"=test$p.value<0.05,
                     "p-value"=test$p.value,
                     "Comment"=ifelse(test$p.value<0.05, "A/B groups are not similar!", "A/B groups are similar!"),
                     "GroupA_mean"=groupA_mean,
                     "GroupB_mean"=groupB_mean,
                     "GroupA_median"=groupA_median,
                     "GroupB_median"=groupB_median)
print(output)

###correlacion sales/oil ----



### Correlacion entre stores a traves de sales ----
a = df1

a <- train %>% select(store_nbr, sales) %>%
  mutate(ind = 1) %>%
  group_by(store_nbr) %>%
  mutate(ind = cumsum(ind)) %>%
  pivot_wider(id_cols = ind, names_from = store_nbr, values_from = sales) %>%
  cor()

color_scale <- colorRampPalette(c("blue", "white", "red"))

# create the correlation matrix plot
ggplot(data = reshape2::melt(a)) + 
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradientn(colors = color_scale(100), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Store Correlation Matrix",
       x = "Store", y = "Store")

#Que pasa con los stores q presentan correlaciones irregulares

# vector of store numbers to visualize
stores_to_show <- c(20, 21, 22, 29, 35, 42, 52)

a <- train %>%
  select(date, store_nbr, sales) %>%
  group_by(store_nbr, date) %>%
  summarise(sales = sum(sales)) %>%
  filter(store_nbr %in% stores_to_show) %>%
  ungroup()

# convert date column to Date object
a$date <- as.Date(a$date)

p <- ggplot(a, aes(x = date, y = sales, color = as.factor(store_nbr))) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Daily total sales of the stores",
       x = "Date", y = "Total Sales", color = "Store Number") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  ggtitle("Daily total sales of the stores") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = setNames(brewer.pal(n = 7, name = "Set1"), stores_to_show)) +
  ggplot2::facet_wrap(~store_nbr, scales = "free_y", ncol = 3) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 8)) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(panel.spacing.x = unit(0.1, "cm")) +
  ggplot2::theme(panel.spacing.y = unit(0.1, "cm")) +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(p, tooltip = c("x", "y", "color")) %>% 
  layout(title = "Daily total sales of the stores",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Total Sales"),
         legend = list(title = "Store Number"))

# Quito los valores de los stores que estaban cerrados

train <- train[!((train$store_nbr == 52) & (train$date < "2017-04-20")), ]
train <- train[!((train$store_nbr == 22) & (train$date < "2015-10-09")), ]
train <- train[!((train$store_nbr == 42) & (train$date < "2015-08-21")), ]
train <- train[!((train$store_nbr == 21) & (train$date < "2015-07-24")), ]
train <- train[!((train$store_nbr == 29) & (train$date < "2015-03-20")), ]
train <- train[!((train$store_nbr == 20) & (train$date < "2015-02-13")), ]
train <- train[!((train$store_nbr == 53) & (train$date < "2014-05-29")), ]
train <- train[!((train$store_nbr == 36) & (train$date < "2013-05-09")), ]

# DF DE TODAS LAS TABLAS ----
#hacerlo luego de procesar na's y outliers
df1 <- train

str(df1)

df1 <- left_join(df1, stores, 
                 by="store_nbr")
df1 <- left_join(df1, oil,
                 by = 'date')
df1 <- left_join(df1, transactions,
                 by = c('date', 'store_nbr'))
df1 <- left_join(df1, holidayevents %>%
                   select(-description, -transferred),
                 by = c('date'))

str(oil)
summary(df1)

#viewing missing values as a list 
as.list(colSums(is.na(df1)))

#changing to factor type
df1$store_nbr <- as.factor(df1$store_nbr)

#date needs to be date format
df1$date <- as_date(df1$date)

#viewing structure to see results
str(df1)

#mutate() of dplyr package allows for easy adding and manipulation of columns
df1 <- df1 %>%
  mutate(Dayofweek = weekdays(df1$date),
         Monthofyear = months(df1$date),
         Year = year(df1$date),
         YrMonth = format(df1$date, "%Y-%m"))

#setting months to factors with ordered levels
df1$Monthofyear <- factor(df1$Monthofyear, levels = month.name)

#day of week to factor as well
df1$Dayofweek <- factor(df1$Dayofweek, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))


