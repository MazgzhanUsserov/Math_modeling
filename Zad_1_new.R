#задание 1

# Усеров М. 121 группа –  для региона 30 рассчитайте урожайность пшеницы в 2001 году, 
# взяв для рассчета средние суммы активных температур за предыдущие 8 лет, с 12 ближайших 
# метеостанций но рассчитав колонку di самостоятельно, как долю месяца, когда среднедневные 
# температуры были выше 8 градусов, но учитывая, что посев не может начаться раньше середины апреля, 
# а вегетация составляет 3 месяца 
# 30 регион - это Астраханская область

rm(list=ls()) 

install.packages(c("tidyverse", "rnoaa"))
install.packages("lubridate")
setwd()
getwd()

library(tidyverse)# загружаем пакеты
library(rnoaa)#загружаем пакеты
library(lubridate)#загружаем пакеты

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# Загрузим данные о метеостанциях:
station_data = ghcnd_stations()
# Сохраним в файл "station_data.сsv".
write.csv(station_data, "station_data.csv")

# Сохраним данные в вектор для дальнейшей работы:
station_data = read.csv("station_data.csv")

#Задаем название и координаты столицы региона 30:
astrahan= data.frame # Делаем поиск 12 ближайших метеостанций:
astrahan = data.frame(id = "ASTRAHAN", latitude = 46.347869,  longitude = 48.033574)
astrahan_around = meteo_nearby_stations(lat_lon_df = astrahan, station_data = station_data,
                                        limit = 12, var = "TAVG",
                                        year_min = 1993, year_max = 2000)
# Создадим таблицу, в которую будем сохранять данные со всех станций:
all_data = tibble()
di = tibble()

for (i in 1:12)
{
  # Определим станцию из 12 ближайших:
  astrahan_id = astrahan_around[["ASTRAHAN"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = astrahan_id,
                          var="TAVG",
                          date_min="1993-01-01",
                          date_max="2000-12-31")
  
  all_data = bind_rows(all_data, data %>%
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         summarise (tavg = sum(tavg[tavg>50])/10 )
  )
  
 }
#преобразуем дату в месяц и день и добавим столбики
all_astrahan_data = mutate(data, year = year(date), month = month(date), day = day(date))
all_astrahan_data_without0 = all_astrahan_data #сохраним резервную таблицу без обнуления

#обнулим значение температуры в невегетативный период
all_astrahan_data[(all_astrahan_data$month < 4),"tavg"] = 0
all_astrahan_data[(all_astrahan_data$month > 7),"tavg"] = 0
all_astrahan_data[(all_astrahan_data$month == 4 & all_astrahan_data$day <= 15),"tavg"] = 0
all_astrahan_data[(all_astrahan_data$month == 7 & all_astrahan_data$day >= 15),"tavg"] = 0

#Сгруппируем по годам и месяцам 
all_astrahan_data = all_astrahan_data %>% group_by(month)
all_astrahan_data_without0 = all_astrahan_data_without0 %>% group_by(month)




#Вычислим di для каждого месяца
di = summarize(all_astrahan_data, di = length(tavg[tavg>80])/length(tavg))[,-1]


#Сгруппируем по годам и месяцам 
all_data = all_data %>% group_by(month)

#Вычислим сумму температур больше 5 градусов в каждом месяце 
St = summarize(all_data, St = mean(tavg, na.rm = TRUE))[,-1]
#Вычислим di для каждого месяца

#Найдем урожаность по формуле:
Fi = af + bf * 1.0 * St
yield = sum(Fi*di*Kf/(Qj*Lj*(100-Ej)))
#Согласно расчету, урожайность пшеницы в Астраханской области в 2001 году составила 18.3 (ц/га):
yield

