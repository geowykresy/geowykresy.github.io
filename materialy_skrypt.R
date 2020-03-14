remotes::install_github("j")

download.file("https://github.com/statisticspoland/R_Package_to_API_BDL/releases/download/1.0.0/bdl.maps.RData",
              destfile = "bdl.maps.RData",
              mode = "wb")



library(tmap)
library(forcats)
library(wbstats)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)
library(writexl)
library(bdl)



##########################WYKRESY_LINIOWE##################################################################################

## 6-75 Zmiany liczby osób młodych, w wieku średnim i osób starszych w Unii Europejskiej w latach 1960 - 2018
df675x1 = wb(country = "EU",
             indicator = "SP.POP.0014.TO",
             startdate = 1960,
             enddate = 2500)

df675x2 = wb(country = "EU",
             indicator = "SP.POP.1564.TO",
             startdate = 1960,
             enddate = 2500)

df675x3 = wb(country = "EU",
             indicator = "SP.POP.65UP.TO",
             startdate = 1960,
             enddate = 2500)


w675 = ggplot(data = df675x1, aes(as.numeric(date), value)) + 
  geom_line(aes(color="0-14")) +
  geom_line(data = df675x2,aes(color="15-65")) +
  geom_line(data = df675x3,aes(color="65+")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "mln os.", 
       title = paste("Zmiany liczby osób młodych, w wieku średnim i osób starszych\nw Unii Europejskiej w latach 1960 do ", max(df675x1$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Grupa wiekowa", labels=c("osoby w wieku poniżej 14 lat", "osoby w wieku od 15 do 64 lat", "osoby w wieku 65 lat i więcej"),values=c("#c6dbef", "#4292c6", "#08306b")) +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x1$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(30000000, 350000000, 50000000), limits=c(30000000, 350000000), 
                     expand = c(0, 0), labels=c("0","50","100", "150", "200", "250", "300"))



## 6-156 Zmiany liczy ludności Ukrainy w latach 1960-2018
df6156 = wb(country = "UA",
            indicator = "SP.POP.TOTL",
            startdate = 1960,
            enddate = 2500)

w6156 = ggplot(df6156, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mld os.", 
       title = paste("Zmiany liczy ludności Ukrainy w latach 1960 do", max(df6156$date))) +
  scale_color_manual(name = "Liczba ludności", labels=c("Ukraina"),values=c("#4292c6")) +
  theme_bw() + labs(x = paste("rok"),
                    subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df6156$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(4.2e+07, 5.3e+07, 0.2e+07), limits=c(4.2e+07, 5.25e+07), 
                     expand = c(0, 0), labels = c("42", "44", "46", "48", "50","52")) 



## 7-89 wykres 1= Liczba ludności w Europie i na świecie w latach 1960 do 2018
df787x1 = wb(country = c("EU", "1W"),
             indicator = "SP.POP.TOTL",
             startdate = 1960,
             enddate = 2500)


w787x1 = ggplot(df787x1, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mld os.", 
       title = paste("Liczba ludności w Europie i na świecie w latach 1960 do", max(df787x1$date))) +
  scale_color_manual(name = "Ludność", labels=c("Europa", "Świat"),values=c("#4292c6","#08306b")) +
  theme_bw() + labs(x = paste("rok"),
                    subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x1$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0e+09, 8e+09, 1e+09), limits=c(0e+09, 8e+09), 
                     expand = c(0, 0), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8")) 



## 7-89 wykres 1-1 = Liczba ludności w Europie w latach 1960 do 2018
df787x11 = wb(country = "EU",
              indicator = "SP.POP.TOTL",
              startdate = 1960,
              enddate = 2500)


w787x11 = ggplot(df787x11, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mln os.", 
       title = paste("Liczba ludności w Europie w latach 1960 do ",max(df787x11$date))) +
  scale_color_manual(name = "Ludność", labels="Europa",values = "#4292c6") +
  theme_bw() + labs(x = paste("rok"),
                    subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x11$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(400000000, 520000000, 20000000,), limits=c(400000000, 520000000), 
                     expand = c(0, 0), labels=c("400", "420", "440", "460", "480","500", "520"))



## 7-89 wykres 1-2 = Liczba ludności na świecie w latach 1960 do 2018
df787x12 = wb(country = "1W",
              indicator = "SP.POP.TOTL",
              startdate = 1960,
              enddate = 2500)


w787x12 = ggplot(df787x12, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mld os.", 
       title = paste("Liczba ludności na świecie w latach 1960 do", max(df787x12$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Ludność", labels="świat",values = "#4292c6") +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x12$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(2e+09, 8e+09, 1e+09), limits=c(2e+09, 8e+09), 
                     expand = c(0, 0), labels=c("2", "3", "4", "5", "6", "7", "8"))



## 7-89 wykres 2 = Liczba ludności w Polsce w latach 1960 do 2018
df787x2 = wb(country = "POL",
             indicator = "SP.POP.TOTL",
             startdate = 1960,
             enddate = 2500)


w787x2 = ggplot(d787x2, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mln os.", 
       title = paste("Liczba ludności w Polsce w latach 1960 do", max(df787x2$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Ludność", labels="Polska",values = "#4292c6") +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x2$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(30000000, 39000000, 2000000), limits=c(29000000, 39000000), 
                     expand = c(0, 0), labels=c("30", "32", "34", "36", "38"))



## 7-92 wykres 1= Średnia długość trwania życia w Polsce wg płci w latach 1960 do 2017
df792x1male = wb(country = "POL",
                 indicator = "SP.DYN.LE00.MA.IN",
                 startdate = 1960,
                 enddate = 2500)

df792x1female = wb(country = "POL",
                   indicator = "SP.DYN.LE00.FE.IN",
                   startdate = 1960,
                   enddate = 2500)

w792x1 = ggplot(data = df792x1male, aes(as.numeric(date), value)) + 
  geom_line(aes(color="mężczyźni")) +
  geom_line(data = df792x1female,aes(color="kobiety")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "wiek", 
       title = paste("Średnia długość trwania życia w Polsce według płci w latach 1960 do", max(df792x1male$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Płeć", labels=c("kobiety", "mężczyźni"),values=c("#4292c6","#08306b")) +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df792x1male$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(66, 82, 4), limits=c(64, 82.5), 
                     expand = c(0, 0), labels=c("66", "70", "74", "78", "82"))



## 7-92 wykres 1-1= Średnia długość trwania życia w Polsce mężczyzn w latach 1960 do 2017
w792x11 = ggplot(df792x1male, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "wiek", 
       title = paste("Średnia długość trwania życia w Polsce mężczyzn w latach 1960 do", max(df792x1male$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Płeć", labels="mężczyźni",values = "#4292c6") +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df792x1male$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(64, 74, 2), limits=c(64, 74), 
                     expand = c(0, 0), labels=c("64", "66", "68", "70", "72", "74"))



## 7-92 wykres 1-2= Średnia długość trwania życia w Polsce kobiet w latach 1960 do 2017
w792x12 = ggplot(df792x1female, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "wiek", 
       title = paste("Średnia długość trwania życia w Polsce kobiet w latach 1960 do", max(df792x1female$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Płeć", labels="kobiety",values = "#4292c6") +
  theme_bw() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df792x1male$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(70, 82, 2), limits=c(70, 82.2), 
                     expand = c(0, 0), labels=c("70", "72", "74",  "76",  "78", "80",  "82"))



#7-99x1 Wielkość migracji zagranicznych (na pobyt stały) w Polsce w latach 1999 - 2018
df799emigracja = get_data_by_variable("55294", unitLevel = 0)
df799migracja = get_data_by_variable("55298", unitLevel = 0) 


w799 = ggplot(data = df799emigracja, aes(as.numeric(year), val)) + 
  geom_line(aes(color="emigracja")) +
  geom_line(data = df799migracja,aes(color="migracja")) +
  labs(color="Legenda") +
  labs(x = "rok", y = "liczba os.", 
       title = paste("Wielkość migracji zagranicznych (na pobyt stały) w Polsce w latach 1999 do", max(df799emigracja$year)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Legenda", labels=c("emigracja", "migracja"),values=c("#4292c6","#08306b")) +
  theme_bw() +
  labs(caption="(dane pobrane z bdl.stat.gov.pl)") +
  scale_x_continuous(breaks=c(seq(1999, 2020, 2)), limits=c(1999, as.numeric(max(df799emigracja$year))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0, 50000, 10000), limits=c(0, 50000), 
                     expand = c(0, 0), labels=c("0", "10000", "20000", "30000", "40000","50000"))



## 7-108 Struktura zatrudnienia w Polsce w latach 1991 - 2019 ?????????????????????????????????????????????????????????????????????????????????????????????????????????????
df7108x1usl = wb(country = "PL",
                 indicator = "SL.SRV.EMPL.ZS",
                 startdate = 1960,
                 enddate = 2500)

df7108x2prz = wb(country = "PL",
                 indicator = "SL.IND.EMPL.ZS",
                 startdate = 1960,
                 enddate = 2500)

df7108x3rol = wb(country = "PL",
                 indicator = "SL.AGR.EMPL.ZS",
                 startdate = 1960,
                 enddate = 2500)


w7108 = ggplot(data = df7108x1usl, aes(as.numeric(date), value)) + 
  geom_line(aes(color="usługi")) +
  geom_line(data = df7108x2prz,aes(color="przemysł")) +
  geom_line(data = df7108x3rol,aes(color="rolnictwo")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "%", 
       title = paste("Struktura zatrudnienia w Polsce w latach 1991 do ", max(df7108x1usl$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Struktura", labels=c("przemysł", "rolnictwo" ,"usługi"),values=c("#c6dbef", "#4292c6", "#08306b")) +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1991, 2021, 5)), limits=c(1991, as.numeric(max(df7108x1usl$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(9,59, 10), limits=c(9, 60), 
                     expand = c(0, 0), labels=c("9","19", "29", "39", "49","59"))


## 7-110 wykres 1 = Stopa bezrobocia zarejestrowana w Polsce w latach 1991 do 2018
df7110 = wb(country = "POL",
            indicator = "SL.UEM.TOTL.ZS",
            startdate = 1991,
            enddate = 2500)


w7110 = ggplot(df7110, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "%", 
       title = paste("Stopa bezrobocia zarejestrowana w Polsce w latach 1991 do", max(df7110$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "stopa bezrobocia", labels="Polska",values = "#4292c6") +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1991, 2018, 3)), limits=c(1991, as.numeric(max(df7110$date))),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(2, 20, 4), limits=c(2, 20.3), 
                     expand = c(0, 0), labels=c("4", "8", "12", "16", "20"))



## 7-116 wykres 1 = Wskaźnik urbanizacji w Polsce w latach 1960 do 2018
df7116x1 = wb(country = "POL",
              indicator = "SP.URB.TOTL.IN.ZS",
              startdate = 1960,
              enddate = 2500)


w7116x1 = ggplot(df7116x1, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "%", 
       title = paste("Wskaźnik urbanizacji w Polsce w latach 1960 do", max(df7116x1$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "wskaźnik urbanizacji", labels="Polska",values = "#4292c6") +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df7116x1$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(46, 64, 2), limits=c(47, 63), 
                     expand = c(0, 0))



## 8-31 Struktura zatrudnienia w Japonii w latach 1991 - 2019 ??????????????????????????????????????????????????????????????????????????
df831x1usl = wb(country = "JP",
                indicator = "SL.SRV.EMPL.ZS",
                startdate = 1991,
                enddate = 2500)

df831x2prz = wb(country = "JP",
                indicator = "SL.IND.EMPL.ZS",
                startdate = 1960,
                enddate = 2500)

df831x3rol = wb(country = "JP",
                indicator = "SL.AGR.EMPL.ZS",
                startdate = 1991,
                enddate = 2500)


w831 = ggplot(data = df831x1usl, aes(as.numeric(date), value)) + 
  geom_line(aes(color="usługi")) +
  geom_line(data = df831x2prz,aes(color="przemysł")) +
  geom_line(data = df831x3rol,aes(color="rolnictwo")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "%", 
       title = paste("Struktura zatrudnienia w Japonii w latach 1991 do ", max(df831x1usl$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Struktura zatrudnienia",values=c("#c6dbef", "#4292c6", "#08306b")) +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1991, 2021, 5)), limits=c(1991, as.numeric(max(df7108x1usl$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0, 80, 10), limits=c(0, 75), 
                     expand = c(0, 0), labels=c("5", "15", "25", "35","45","55","65","75","80"))



##8-39  Zmiany wielkości PKB Chin na tle USA i Japonii w latach 1960 - 2018 (w mld USD)
df839 = wb(country = c("US","CN","JP"),
           indicator = "NY.GDP.MKTP.CD",
           startdate = 1960,
           enddate = 2050)


w839 = ggplot(df839, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "[mld usd]", 
       title = paste("Zmiany wielkości PKB Chin na tle USA i Japonii w latach 1960  do ", max(df839$date)),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_manual(name = "Kraje", labels=c("Chiny", "Japonia", "USA"),values=c("#c6dbef", "#4292c6", "#08306b")) +
  theme_bw() +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df839$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0e+00, 2.1e+13, 4e+12), limits=c(0e+00, 2.1e+13), 
                     expand = c(0, 0), labels=c("0","4000", "8000", "12000","16000","20000"))



## 8-123 Liczba ludności Ameryki w latach 1960-2018 
df8123x1 = wb(country = "ZJ",
              indicator = "SP.POP.TOTL",
              startdate = 1960,
              enddate = 2050)

df8123x2 = wb(country = "XU",
              indicator = "SL.SRV.EMPL.ZS",
              startdate = 1991,
              enddate = 2500)



############################################################################################################################################################
pykpyk1 = ggplot(data = df675x1, aes(as.numeric(date), value)) + 
  geom_line(aes(color="0-14")) +
  geom_line(data = df675x2,aes(color="15-65")) +
  geom_line(data = df675x3,aes(color="65+")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "mln os.") +
  scale_color_discrete(name = "Grupa wiekowa", labels=c("osoby w wieku poniżej 14 lat", "osoby w wieku od 15 do 64 lat", "osoby w wieku 65 lat i więcej")) +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df675x1$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(30000000, 350000000, 50000000), limits=c(30000000, 350000000), 
                     expand = c(0, 0), labels=c("0","50","100", "150", "200", "250", "300"))

pyk1 = pykpyk1 + theme(legend.position = "none") +
  scale_color_manual(values=c("#c6dbef", "#4292c6", "#08306b"))

pykpyk2 = ggplot(df6156, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mld os.") +
  scale_color_manual(name = "Liczba ludności", labels=c("Ukraina"),values=c("#4292c6")) +
  theme_light() + labs(x = paste("rok")) +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df6156$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(4.2e+07, 5.3e+07, 0.2e+07), limits=c(4.2e+07, 5.25e+07), 
                     expand = c(0, 0), labels = c("42", "44", "46", "48", "50","52")) 

pyk2 = pykpyk2 + theme(legend.position = "none") 

pykpyk3 = ggplot(df787x1, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mld os.") +
  scale_color_manual(name = "Ludność", labels=c("Europa", "Świat"),values=c("#4292c6","#08306b")) +
  theme_light() + labs(x = paste("rok")) +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x1$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0e+09, 8e+09, 1e+09), limits=c(0e+09, 8e+09), 
                     expand = c(0, 0), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8")) 

pyk3 = pykpyk3 + theme(legend.position = "none") 

pykpyk4 =ggplot(df787x11, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mln os.") +
  scale_color_manual(name = "Ludność", labels="Europa",values = "#4292c6") +
  theme_light() + labs(x = paste("rok")) +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x11$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(400000000, 520000000, 20000000,), limits=c(400000000, 520000000), 
                     expand = c(0, 0), labels=c("400", "420", "440", "460", "480","500", "520"))

pyk4 = pykpyk4 + theme(legend.position = "none") 

pykpyk5 = ggplot(df787x12, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mld os.") +
  scale_color_manual(name = "Ludność", labels="świat",values = "#4292c6") +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x12$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(2e+09, 8e+09, 1e+09), limits=c(2e+09, 8e+09), 
                     expand = c(0, 0), labels=c("2", "3", "4", "5", "6", "7", "8"))

pyk5 = pykpyk5 + theme(legend.position = "none") 

pykpyk6 = ggplot(df787x2, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "mln os.") +
  scale_color_manual(name = "Ludność", labels="Polska",values = "#4292c6") +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df787x2$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(30000000, 39000000, 2000000), limits=c(29000000, 39000000), 
                     expand = c(0, 0), labels=c("30", "32", "34", "36", "38"))

pyk6 = pykpyk6 + theme(legend.position = "none") 

pykpyk7 = ggplot(data = df792x1male, aes(as.numeric(date), value)) + 
  geom_line(aes(color="mężczyźni")) +
  geom_line(data = d792x1female,aes(color="kobiety")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "wiek") +
  scale_color_manual(name = "Płeć", labels=c("kobiety", "mężczyźni"),values=c("#4292c6","#08306b")) +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df792x1male$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(66, 82, 4), limits=c(64, 82.5), 
                     expand = c(0, 0), labels=c("66", "70", "74", "78", "82"))

pyk7 = pykpyk7 + theme(legend.position = "none") 

pykpyk8 = ggplot(df792x1male, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "wiek") +
  scale_color_manual(name = "Płeć", labels="mężczyźni",values = "#4292c6") +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df792x1male$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(64, 74, 2), limits=c(64, 74), 
                     expand = c(0, 0), labels=c("64", "66", "68", "70", "72", "74"))

pyk8 = pykpyk8 + theme(legend.position = "none") 

pykpyk9 = ggplot(df792x1female, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "wiek") +
  scale_color_manual(name = "Płeć", labels="kobiety",values = "#4292c6") +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df792x1female$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(70, 82, 2), limits=c(70, 82.2), 
                     expand = c(0, 0), labels=c("70", "72", "74",  "76",  "78", "80",  "82"))

pyk9 = pykpyk9 + theme(legend.position = "none") 

pykpyk10 = ggplot(data = df799emigracja, aes(as.numeric(year), val)) + 
  geom_line(aes(color="emigracja")) +
  geom_line(data = df799migracja,aes(color="migracja")) +
  labs(color="Legenda") +
  labs(x = "rok", y = "liczba os.") +
  scale_color_manual(name = "Legenda", labels=c("emigracja", "migracja"),values=c("#4292c6","#08306b")) +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1999, 2020, 2)), limits=c(1999, as.numeric(max(df799emigracja$year))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0, 50000, 10000), limits=c(0, 50000), 
                     expand = c(0, 0), labels=c("0", "10000", "20000", "30000", "40000","50000"))

pyk10 = pykpyk10 + theme(legend.position = "none") 

pykpyk11 = ggplot(data = df7108x1usl, aes(as.numeric(date), value)) + 
  geom_line(aes(color="usługi")) +
  geom_line(data = df7108x2prz,aes(color="przemysł")) +
  geom_line(data = df7108x3rol,aes(color="rolnictwo")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "%") +
  scale_color_manual(name = "Struktura", labels=c("przemysł", "rolnictwo" ,"usługi"),values=c("#c6dbef", "#4292c6", "#08306b")) +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1991, 2021, 5)), limits=c(1991, as.numeric(max(df7108x1usl$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(9,59, 10), limits=c(9, 60), 
                     expand = c(0, 0), labels=c("9","19", "29", "39", "49","59"))

pyk11 = pykpyk11 + theme(legend.position = "none") 

pykpyk12 = ggplot(df7110, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "%") +
  scale_color_manual(name = "stopa bezrobocia", labels="Polska",values = "#4292c6") +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1991, 2018, 3)), limits=c(1991, as.numeric(max(df7110$date))),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(2, 20, 4), limits=c(2, 20.3), 
                     expand = c(0, 0), labels=c("4", "8", "12", "16", "20"))

pyk12 = pykpyk12 + theme(legend.position = "none") 

pykpyk13 = ggplot(df7116x1, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "%") +
  scale_color_manual(name = "wskaźnik urbanizacji", labels="Polska",values = "#4292c6") +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df7116x1$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(46, 64, 2), limits=c(47, 63), 
                     expand = c(0, 0))

pyk13 = pykpyk13 + theme(legend.position = "none") 

pykpyk14 = ggplot(data = df831x1usl, aes(as.numeric(date), value)) + 
  geom_line(aes(color="usługi")) +
  geom_line(data = df831x2prz,aes(color="przemysł")) +
  geom_line(data = df831x3rol,aes(color="rolnictwo")) +
  labs(color="Legend text") +
  labs(x = "rok", y = "%") +
  scale_color_manual(name = "Struktura zatrudnienia",values=c("#c6dbef", "#4292c6", "#08306b")) +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1991, 2021, 5)), limits=c(1991, as.numeric(max(df7108x1usl$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0, 80, 10), limits=c(0, 75), 
                     expand = c(0, 0), labels=c("5", "15", "25", "35","45","55","65","75","80"))

pyk14 = pykpyk14 + theme(legend.position = "none") 

pykpyk15 = ggplot(df839, aes(as.numeric(date), value, color = country)) + geom_line() +
  labs(x = "rok", y = "[mld usd]") +
  scale_color_manual(name = "Kraje", labels=c("Chiny", "Japonia", "USA"),values=c("#c6dbef", "#4292c6", "#08306b")) +
  theme_light() +
  scale_x_continuous(breaks=c(seq(1960, 2020, 5)), limits=c(1960, as.numeric(max(df839$date))), expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(0e+00, 2.1e+13, 4e+12), limits=c(0e+00, 2.1e+13), 
                     expand = c(0, 0), labels=c("0","4000", "8000", "12000","16000","20000"))

pyk15 = pykpyk15 + theme(legend.position = "none") 


############################################################################################################################################################

# w675 Klasa 6, strona 75 
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl6str75.png", w675, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/1.png", pyk1, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df675x1, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl6str75-0-14.xlsx")
writexl::write_xlsx(df675x1, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl6str75-15-64.xlsx")
writexl::write_xlsx(df675x1, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl6str75-65plus.xlsx")

# w6156 Klasa 6, strona 156
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl6str156.png", w6156, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/2.png", pyk2, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df6156, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl6str156.xlsx")

# w787x1 Klasa 7, strona 87
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str87.png", w787x1, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/3.png", pyk3, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df787x1, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str87.xlsx")

# w787x11 Klasa 7, strona 87
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str87x.png", w787x11, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/4.png", pyk4, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df787x11, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str87x.xlsx")

# w787x12 Klasa 7, strona 87
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str87xx.png", w787x12, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/5.png", pyk5, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df787x2, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str87xx.xlsx")

# w787x2 Klasa 7, strona 87
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str87wyk2.png", w787x2, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/6.png", pyk6, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df787x2, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str87wyk2.xlsx")

# w792x1 Klasa 7, strona 92
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str92.png", w792x1, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/7.png", pyk7, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df792x1male, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str92mez.xlsx")
writexl::write_xlsx(df792x1female, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str92kob.xlsx")

# w792x11 Klasa 7, strona 92
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str92x.png", w792x11, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/8.png", pyk8, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df792x1male, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str92mez.xlsx")

# w792x12 Klasa 7, strona 92
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str92xx.png", w792x12, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/9.png", pyk9, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df792x1female, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str92kob.xlsx")

# w799 Klasa 7, strona 99
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str99.png", w799, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/10.png", pyk10, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df799emigracja, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str99emigracja.xlsx")
writexl::write_xlsx(df799migracja, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str99migracja.xlsx")

# w7108 Klasa 7, strona 108
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str108.png", w7108, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/11.png", pyk11, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7108x1usl, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str108uslugi.xlsx")
writexl::write_xlsx(df7108x2prz, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str108przemysl.xlsx")
writexl::write_xlsx(df7108x3rol, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str108rolnictwo.xlsx")

# w7110 Klasa 7, strona 110
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str110.png", w7110, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/12.png", pyk12, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7110, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str110.xlsx")

# w7116x1 Klasa 7, strona 116
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl7str116.png", w7116x1, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/13.png", pyk13, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7116x1, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl7str116.xlsx")

# w831 Klasa 8, strona 31
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl8str31.png", w831, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/14.png", pyk14, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df831x1usl, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl8str31uslugi.xlsx")
writexl::write_xlsx(df831x2prz, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl8str31przemysl.xlsx")
writexl::write_xlsx(df831x3rol, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl8str31rolnictwo.xlsx")

# w839 Klasa 8, strona 39
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/pelne/kl8str39.png", w839, width=70, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/liniowe/niepelne/15.png", pyk15, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df839, "C:/Users/adams/Desktop/ary/plots/liniowe/dane/kl8str39.xlsx")


###############################################################################################################################################



#########################################WYKRESY_SLUPKOWE#########################################################


## 6-110x1 Struktura zatrudnienia we Francji w 2017r. WORLDBANK
df6110x1 = data.frame(x=c("usługi", "przemysł","rolnictwo"),
                      procent=c(76.8, 20.6, 2.6))

w6110x1 = ggplot(data=df6110x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia we Francji w 2017r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane pobrane z Worldbanku)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo1 = ggplot(data=df6110x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 6-110x1 Struktura wytwarzania PKB we Francji w 2017r. WORLDBANK
df6110x2 = data.frame(x=c("usługi", "przemysł","rolnictwo"),
                      procent=c(81.2, 17.2, 1.6))

w6110x2 = ggplot(data=df6110x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura wytwarzania PKB we Francji w 2017r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane pobrane z Worldbanku)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo2 = ggplot(data=df6110x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 6-164 Strunktura zatrudnienia w Rosji w 2017r. WORLDBANK
df6164 = data.frame(x=c("usługi", "przemysł","rolnictwo"),
                    procent=c(67.1, 27, 5.9))

w6164 = ggplot(data=df6164, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Rosji w 2017r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane pobrane z Worldbanku)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo3 = ggplot(data=df6164, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 7-74 Struktura powierzchni lasów w Polsce wg składu gatunkowego w 2016r.
df774 = data.frame(kraj=c("sosna", "dąb", "buk","brzoza","świerk","olsza","jodła","pozostałe"),
                   procent=c(69,8,5.8,4.9,4.9,4.5,2.3,0.6))

w774 = ggplot(data=df774, aes(x = fct_inorder(kraj, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura powierzchni lasów w Polsce wg składu gatunkowego w 2016r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light()

demo4 = ggplot(data=df774, aes(x = fct_inorder(kraj, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  theme_light()



## 7-99x2 Główne kierunki imigracji do Polski (na pobyt stały) w 2014 r.
df799 = data.frame(kraj=c("inny", "Wielka\nBrytania", "Niemcy", "Stany\nZjednoczone", "Ukraina", "Irlandia"),
                   procent=c(37.6, 23.9, 19.1, 8.6, 6.1, 4.7))

w799x2 = ggplot(data=df799, aes(x = fct_reorder(kraj, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Strunktura zatrudnienia w Rosji w 2017r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo5 = ggplot(data=df799, aes(x = fct_reorder(kraj, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 7-101 Struktura narodowościowa w Polsce w latach 1931 i 2011
df7101 = data.frame(narodowość=rep(c("Polacy", "mniejszość narodowa", "narodowość nieustalona"), each=2),
                    rok=rep(c("1931", "2011"),1),
                    procent=c(64, 97.1, 36, 1.5, 0, 1.4))

w7101 = ggplot(data=df7101, aes(x=rok, y=procent, fill=narodowość)) +
  geom_col() +
  scale_fill_manual(values = c("#08306b", "#c6dbef", "#4292c6")) +
  labs(x = NULL,title = paste("Struktura narodowościowa w Polsce w latach 1931 i 2011"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demodemo6 = ggplot(data=df7101, aes(x=rok, y=procent, fill=narodowość)) +
  geom_col() +
  scale_fill_manual(values = c("#08306b", "#c6dbef", "#4292c6")) +
  theme_light() + 
  coord_flip()

demo6 = demodemo6 + theme(legend.position = "none") 



## 7-104 Struktura narodowościowa w Belgii
df7104x1 = data.frame(x = c("Flamingowie", "Walończycy","potomkowie\nobu grup\nnarodowościowych\ni pozostali"),
                      procent = c(58, 31, 11))

w7104x1 = ggplot(data = df7104x1, aes(x = fct_reorder(x, procent), y = procent)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = NULL,title = paste("Struktura narodowościowa w Belgii"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo7 = ggplot(data = df7104x1, aes(x = fct_reorder(x, procent), y = procent)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-104 Struktura narodowościowa w Niemczech
df7104x2= data.frame(x=c("Niemcy", "Turcy","pozostali"),
                     procent=c(58, 31, 11))

w7104x2 = ggplot(data=df7104x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura narodowościowa w Niemczech"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo8 =ggplot(data=df7104x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 7-104 Struktura narodowościowa w Litwie
df7104x3= data.frame(x=c("Litwini", "Polacy","Rosjanie","Białorusini","pozostali"),
                     procent=c(84.1, 6.6, 5.8,1.2,2.3))

w7104x3  = ggplot(data=df7104x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura narodowościowa w Litwie"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo9 =  ggplot(data=df7104x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 7-105 Struktura wyznaniowa w Polsce
df7105x1= data.frame(x=c("katolicy", "prawosławni","protestanci","pozostali"),
                     procent=c(87.6, 0.4, 0.3, 11.7))

w7105x1 = ggplot(data=df7105x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura wyznaniowa w Polsce"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo10 = ggplot(data=df7105x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 7-105 Struktura wyznaniowa w Rumunii
df7105x2= data.frame(x=c("prawosławni","pozostali", "protestanci","katolicy"),
                     procent=c(81.9,7.4, 6.4, 4.3))

w7105x2 = ggplot(data=df7105x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura wyznaniowa w Rumunii"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo11 = ggplot(data=df7105x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 7-105 Struktura wyznaniowa w Niemczech
df7105x3 = data.frame(x=c("protestanci", "katolicy","pozostali","muzułmanie"),
                      procent=c(34,34,28.3,3.7))

w7105x3 = ggplot(data=df7105x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura wyznaniowa w Niemczech"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo12 =  ggplot(data=df7105x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL) +
  coord_flip() +
  theme_light()



## 7-108 Struktura zatrudnienia w Polsce w 1950 r.
df7108x1= data.frame(x=c("rolnictwo", "przmysł\ni budownictwo","usługi"),
                     procent=c(57, 23, 20))

w7108x1  = ggplot(data=df7108x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Polsce w 1950 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo13 = ggplot(data=df7108x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-108 Struktura zatrudnienia w Polsce w 1970 r.
df7108x2= data.frame(x=c("rolnictwo", "przmysł\ni budownictwo","usługi"),
                     procent=c(37, 35, 28))

w7108x2  =  ggplot(data=df7108x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Polsce w 1970 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo14 = ggplot(data=df7108x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-108 Struktura zatrudnienia w Polsce w 1989 r.
df7108x3= data.frame(x=c("rolnictwo", "przmysł\ni budownictwo","usługi"),
                     procent=c(28.6, 35.7, 35.7))

w7108x3  =  ggplot(data=df7108x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Polsce w 1989 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo15 = ggplot(data=df7108x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-108 Struktura zatrudnienia w Polsce w 2016 r.
df7108x4= data.frame(x=c("rolnictwo", "przmysł\ni budownictwo","usługi"),
                     procent=c(10.5, 31.3, 58.2))

w7108x4  =  ggplot(data=df7108x4, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Polsce w 2016 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo16 = ggplot(data=df7108x4, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-116 wykres 1 = Wskaźnik urbanizacji w wybranych krajach w 2015r.
d7116x2 = wb(country = c("BE","FI","ES","FR","DE","BG","RO","SI"),
             indicator = "SP.URB.TOTL.IN.ZS",
             startdate = 2015,
             enddate = 2015)

w7116x2 = ggplot(data=d7116x2, aes(x=fct_reorder(iso3c, value, .desc = TRUE), y=value)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "rok", y = "procent", 
       title = paste("Wskaźnik urbanizacji w wybranych krajach w 2015r. "),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  scale_color_discrete(name = "stopa bezrobocia", labels="Polska") +
  theme_light() +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), color="white", size=3) +
  labs(caption="(dane pobrane z Worldbanku)") +
  scale_y_continuous(breaks=seq(0, 100, 25), limits=c(0, 100), expand = c(0, 0)) +
  scale_x_discrete(breaks=c("BEL","FIN","FRA","ESP","DEU","BGR","ROU","SVN"),
                   labels=c("Belgia", "Finlandia", "Francja","Hiszpania","Niemcy","Bułgaria","Rumunia","Słowienia"))

demo17 = ggplot(data=d7116x2, aes(x=fct_reorder(iso3c, value, .desc = TRUE), y=value)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "rok", y = "procent") +
  scale_color_discrete(name = "stopa bezrobocia", labels="Polska") +
  theme_light() +
  scale_y_continuous(breaks=seq(0, 100, 25), limits=c(0, 100), expand = c(0, 0)) +
  scale_x_discrete(breaks=c("BEL","FIN","FRA","ESP","DEU","BGR","ROU","SVN"),
                   labels=c("Belgia", "Finlandia", "Francja","Hiszpania","Niemcy","Bułgaria","Rumunia","Słowienia"))



## 7-117 Miasta Polski wg grup wielkościowych w 2016 r.
df7117 = data.frame(x=c("poniżej 5","10-20", "5-10",  "20-50", "50-100", "100-200", "powyżej 100"),
                    procent=c(36.3, 20.3, 19.6, 14.5, 5.1, 2.5, 1.7))

w7117 = ggplot(data=df7117, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Miasta Polski wg grup wielkościowych w 2016 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo18 = ggplot(data=df7117, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = paste("tysięcy osób")) +
  coord_flip()

## 7-125 Struktura wielkościowa gospodarstw rolnych w Polsce w 2015 r.
df7125 = data.frame(x=c("2-5", "5-10","1-2", "10-15", "20 i więcej", "15-20", "0-1"),
                    procent=c( 32.6, 22.9, 18, 10.3, 9.5, 5.1, 2.0))

w7125 = ggplot(data=df7125, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "ha", y = "procent",title = paste("Struktura wielkościowa gospodarstw rolnych w Polsce w 2015 r"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo19 = ggplot(data=df7125, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "ha", y = "procent") +
  theme_light() +
  coord_flip()



## 7-128 Struktura powierzchni upraw w Polsce w 2016 r.
df7128 = data.frame(x=c("zboża", "rośliny\npastewne", "rośliny\nrzemysłowe", "pozostałe", "ziemniaki"),
                    procent=c(70.6, 10.4, 10.3, 5.8, 2.9))

w7128  = ggplot(data=df7128, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura powierzchni upraw w Polsce w 2016 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo20 = ggplot(data=df7128, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  coord_flip() +
  labs(x = NULL)



## 7-131x1 Struktura produkcji przemysłowej w Polsce w 1990 
df7131x1 = data.frame(x=c("przemysł\nelektromaszynowy", "przemysł\nspożywczy", "przemysł\nhutniczy", 
                          "przemysł\nchemiczny", "przymysł\nlekki","górnictwo", "przemysł drzwewny\ni papierniczy","inne"),
                      procent=c(22, 18.6, 10.3, 8.5, 8.1, 7.9, 3.5, 21.1))

w7131x1 = ggplot(data=df7131x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura produkcji przemysłowej w Polsce w 1990 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo21 = ggplot(data=df7131x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



# 7-131x2 Struktura produkcji przemysłowej w Polsce w 2015  
df7131x2 = data.frame(x=c("przemysł\nelektromaszynowy", "przemysł chemiczny\ni wyrobów gumowych", "przemysł\nspożywczy", 
                          "przemysł drzwewny\ni papierniczy", "górnictwo", "przemysł\nmineralny","przemysł\nhutniczy",
                          "przemysł\nlekki", "inne"),
                      procent=c(31.5, 16.5, 16.5, 5.7, 3.7, 3.7, 3.3, 2.2, 16.9))

w7131x2 = ggplot(data=df7131x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura produkcji przemysłowej w Polsce w 2015 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo22 = ggplot(data=df7131x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-139 Struktura produkcji energii elektrycznej w Polsce wg źródeł w 2015 r.
df7139 = data.frame(x=c("węgiel\nkamienny", "węgiel\nbrunatny", "odnawialne\nźródła\nenergii", "gaz\nziemny", "inne"),
                    procent=c(48, 33, 13.8, 3.8, 1.4))

w7139 = ggplot(data=df7139, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura produkcji przemysłowej w Polsce w 2015 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo23 = ggplot(data=df7139, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-144 Struktura polskich połowóW ryb i innych organizmów morskich w 2015 r.
df7144 = data.frame(x=c("szproty", "ostroboki", "śledzie", "dorsze", "ryby płaskie", "pozostałe"),
                    procent=c(34.3, 21.2, 21.2, 9.9, 5.2, 8.2))

w7144 = ggplot(data=df7144, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura polskich połowóW ryb i innych organizmów morskich w 2015 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  coord_flip() +
  theme_light()

demo24 = ggplot(data=df7144, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-159 Głowni partnerzy Polski w handlu międzynarodowym w 2015 roku - import
df7159x1 = data.frame(x=c("Niemcy", "Chiny", "Rosja", "Włochy", "Holandia", "Francja", "Czechy", "Wielka\nBrytania", "pozostałe"),
                      procent=c(22.9, 11.4, 7.3, 5.4, 3.8, 3.8, 3.5, 2.7, 39.2))

w7159x1 = ggplot(data=df7159x1, aes(x = fct_inorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Głowni partnerzy Polski w handlu międzynarodowym w 2015 roku - import"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light()

demo25 = ggplot(data=df7159x1, aes(x = fct_inorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL)



## 7-159 Głowni partnerzy Polski w handlu międzynarodowym w 2015 roku - ekport
df7159x2 = data.frame(x=c("Niemcy", "Wielka\nBrytania", "Czechy", "Francja", "Włochy", "Holandia", "Rosja", "Szwecja", "pozostałe"),
                      procent=c(27.1, 6.7, 6.6, 5.5, 4.8, 4.4, 2.9, 2.7, 39.3))

w7159x2 = ggplot(data=df7159x2, aes(x = fct_inorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Głowni partnerzy Polski w handlu międzynarodowym w 2015 roku - eksport"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light()

demo26 = ggplot(data=df7159x2, aes(x = fct_inorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL)



## 7-167 Stan jakości wód dorzeczy Wisły w latach 2011-2013
df7167x1 = data.frame(klasa=c("bardzo dobry\n(I)", "dobry\n(II)", "umiarkowany\n(III)", "słaby\n(IV)", "zły\n(V)"),
                      procent=c(2.5, 21.7, 49.5, 20.1, 6.2))

w7167x1 = ggplot(data=df7167x1, aes(x = fct_reorder(klasa, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Stan jakości wód dorzeczy Wisły w latach 2011-2013"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()


demo27 = ggplot(data=df7167x1, aes(x = fct_reorder(klasa, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 7-167 Stan jakości wód dorzeczy Odry w latach 2011-2013
df7167x2 = data.frame(klasa=c("bardzo dobry\n(I)", "dobry\n(II)", "umiarkowany\n(III)", "słaby\n(IV)", "zły\n(V)"),
                      procent=c(3.4, 33.4, 46.7, 12.8, 3.7))

w7167x2 = ggplot(data=df7167x2, aes(x = fct_reorder(klasa, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Stan jakości wód dorzeczy Odry w latach 2011-2013"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo28 = ggplot(data=df7167x2, aes(x = fct_reorder(klasa, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-31 Struktura zatrudnienia w Japonii w roku 1920

df831x1 = data.frame(x=c("usługi", "przemysł","rolnictwo"),
                     procent=c(24, 22, 54))

w831x1 = ggplot(data=df831x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Japonii w roku 1920"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo29 =  ggplot(data=df831x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-31 Struktura zatrudnienia w Japonii w roku 2015
df831x2 = data.frame(x=c("usługi", "przemysł","rolnictwo"),
                     procent=c(70.9, 26.2, 2.9))

w831x2 = ggplot(data=df831x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Japonii w roku 2015"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo30 = ggplot(data=df831x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-46 Struktura religijna Indii w 2016 r.
df846 = data.frame(x=c("hinduiści", "muzułmanie","chrześcijanie","sikhowie","animiści","buddyści","pozostałe\nreligie"),
                   procent=c(74.3, 14.2, 5.8, 1.9, 1.4, 0.8, 1.6))

w846 = ggplot(data=df846, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura religijna Indii w 2016 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo31 = ggplot(data=df846, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-49 Struktura zatrudnienia w Indiach w 2014 r.
df849x1 = data.frame(x=c("usługi", "przemysł","rolnictwo"),
                     procent=c(35, 13, 52))

w849x1 = ggplot(data=df849x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura zatrudnienia w Indiach w 2014 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo32 = ggplot(data=df849x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-49 Struktura PKB w Indiach w 2014 r.
df849x2 = data.frame(x=c("usługi", "przemysł","rolnictwo"),
                     procent=c(53.7, 29, 17.3))

w849x2 = ggplot(data=df849x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura PKB w Indiach w 2014 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo33 = ggplot(data=df849x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-54 Zasoby ropy naftowej na świecie w 2016 r. 
df854 = data.frame(x=c("Bliski\nWschód", "Ameryka\nPołudniowa\ni Ameryka\nŚrodkowa","Ameryka\nPółnocna",
                       "Eurazja","Afryka","Daleki\nWschód\nz Australią\ni Oceanią"),
                   procent=c(47.7, 19.2, 13.3, 9.5, 7.5, 2.8))

w854 = ggplot(data=df854, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Zasoby ropy naftowej na świecie w 2016 r. "),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo34 = ggplot(data=df854, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-137 Struktura użytkowania ziemi w Kanadzie 
df8137 = data.frame(x=c("nieużytki", "lasy","grunty orne","łąki i pastwiska"),
                    procent=c(53, 38.2, 7.2,1.6))

w8137 = ggplot(data=df8137, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura użytkowania ziemi w Kanadzie "),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo35 = ggplot(data=df8137, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-141 Udział wybranych państw w tworzeniu produktu światowego brutto w 2016 r.
df8141x1 = data.frame(x=c("USA", "Chiny","Japonia","Niemcy","Wielka\nBrytania","Polska","pozostałe"),
                      procent=c(24.6, 14.8, 6.5, 4.6, 3.5, 0.6, 45.4))

w8141x1 = ggplot(data=df8141x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Udział wybranych państw w tworzeniu produktu światowego brutto w 2016 r. "),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo36 = ggplot(data=df8141x1, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-141 Udział wybranych państw w światowym imporcie w 2015 r.
df8141x2 = data.frame(x=c("USA", "Chiny","Niemcy","Japonia","Wielka\nBrytania","Polska","pozostałe"),
                      procent=c(14,10.2,6.4,3.9,3.6,1.2,60.7))

w8141x2 = ggplot(data=df8141x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Udział wybranych państw w światowym imporcie w 2015 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo37 = ggplot(data=df8141x2, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-141 Udział wybranych państw w światowym eksporcie w 2015 r.
df8141x3 = data.frame(x=c("Chiny", "USA","Niemcy","Japonia","Korea Płd.","Polska","pozostałe"),
                      procent=c(13.8,9.1,8,3.8,3.2,1.2,60.9))

w8141x3 = ggplot(data=df8141x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Udział wybranych państw w światowym eksporcie w 2015 r."),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo38 = ggplot(data=df8141x3, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-143 Struktura użytkowania ziemi w Stanach Zjednoczonych 
df8143 = data.frame(x=c("lasy", "łąki i pastwiska","nieużytki","grunty orne"),
                    procent=c(33.8, 27.4, 20.9, 17.9))

w8143 = ggplot(data=df8143, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura użytkowania ziemi w Stanach Zjednoczonych "),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo39 = ggplot(data=df8143, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-145 Najczęściej marnowane produkty żywnościowe w Stanach Zjednoczonych
df8145 = data.frame(x=c("sałatki", "owoce\ni warzywa","chleb\ni wyroby\npiekarskie","mięso\ni ryby"),
                    procent=c(50, 22, 18,10))

w8145 = ggplot(data=df8145, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Najczęściej marnowane produkty żywnościowe w Stanach Zjednoczonych"),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()

demo40 = ggplot(data=df8145, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



## 8-164 Struktura użytkowania ziemi w Australii 
df8164 = data.frame(x=c("łąki\ni pastwiska", "nieużytki","lasy","grunty\norne"),
                    procent=c(46.7, 30.9, 16.2 ,6.2))

w8164 = ggplot(data=df8164, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = NULL,title = paste("Struktura użytkowania ziemi w Australii "),
       subtitle = paste("Data pobrania:", Sys.Date())) +
  labs(caption="(dane z podręcznika „Planeta Nowa”)") +
  geom_text(aes(label=procent), position = position_stack(vjust = 0.5), color="white", size=3) +
  theme_light() +
  coord_flip()


demo41 = ggplot(data=df8164, aes(x = fct_reorder(x, procent), y=procent)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_light() +
  labs(x = NULL) +
  coord_flip()



########################################################################################################################################################
# 6-110x1  Klasa 6, strona 110 
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl6str110.png", w6110x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/1.png", demo1, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df6110x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl6str110.xlsx")
## 6-110x2 Klasa 6, strona 110
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl6str110x.png", w6110x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/2.png", demo2, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df6110x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl6str110x.xlsx")
# 6-164 Klasa 6, strona 
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl6str164.png", w6164, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/3.png", demo3, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df6164, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl6str164.xlsx")
# 7-74 Klasa 7, strona 74
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str74.png", w774, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/4.png", demo4, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df774, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str74.xlsx")
# 7-99x2 Klasa 7, strona 99
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str99.png", w799x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/5.png", demo5, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df799, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str99.xlsx")
# 7-101 Klasa 7, strona 101
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str101.png", w7101, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/6.png", demo6, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7101, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str101.xlsx")
# 7-104 Klasa 7, strona 104
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str104.png", w7104x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/7.png", demo7, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7104x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str104.xlsx")
# 7-104 Klasa 7, strona 104
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str104x.png", w7104x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/8.png", demo8, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7104x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str104x.xlsx")
# 7-104 Klasa 7, strona 104
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str104xx.png", w7104x3, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/9.png", demo9, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7104x3, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str104xx.xlsx")
# 7-105 Klasa 7, strona 105
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str105.png", w7105x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/10.png", demo10, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7105x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str105.xlsx")
# 7-105 Klasa 7, strona 105
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str105x.png", w7105x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/11.png", demo11, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7105x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str105x.xlsx")
# 7-105 Klasa 7, strona 105
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str105xx.png", w7105x3, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/12.png", demo12, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7105x3, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str105xx.xlsx")
# 7-108 Klasa 7, strona 108
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str108.png", w7108x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/13.png", demo13, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7108x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str108.xlsx")
# 7-108 Klasa 7, strona 108
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str108x.png", w7108x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/14.png", demo14, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7108x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str108x.xlsx")
# 7-108 Klasa 7, strona 108
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str108xx.png", w7108x3, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/15.png", demo15, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7108x3, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str108xx.xlsx")
# 7-108 Klasa 7, strona 108
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str108xxx.png", w7108x4, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/16.png", demo16, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7108x4, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str108xxx.xlsx")
# 7-116 Klasa 7, strona 116
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str116.png", w7116x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/17.png", demo17, width=30, height=20, units = "cm", scale = 0.45)
writexl::write_xlsx(d7116x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str116.xlsx")
# 7-117 Klasa 7, strona 117
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str117.png", w7117, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/18.png", demo18, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7117, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str117.xlsx")
# 7-125 Klasa 7, strona 125
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str125.png", w7125, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/19.png", demo19, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7125, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str125.xlsx")
# 7-128 Klasa 7, strona 128
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str128.png", w7128, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/20.png", demo20, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7128, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str128.xlsx")
# 7-131x1 Klasa 7, strona 131
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str131.png", w7131x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/21.png", demo21, width=30, height=20, units = "cm", scale = 0.45)
writexl::write_xlsx(df7131x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str131.xlsx")
# 7-131x1 Klasa 7, strona 131
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str131x.png", w7131x2, width=60, height=40, units = "cm", scale = 0.35)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/22.png", demo22, width=30, height=20, units = "cm", scale = 0.5)
writexl::write_xlsx(df7131x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str131x.xlsx")
# 7-139 Klasa 7, strona 139
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str139.png", w7139, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/23.png", demo23, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7139, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str139.xlsx")
# 7-144 Klasa 7, strona 144
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str144.png", w7144, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/24.png", demo24, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7144, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str144.xlsx")
# 7-159 Klasa 7, strona 159
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str159.png", w7159x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/25.png", demo25, width=30, height=20, units = "cm", scale = 0.5)
writexl::write_xlsx(df7159x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str159.xlsx")
# 7-159 Klasa 7, strona 159
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str159x.png", w7159x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/26.png", demo26, width=30, height=20, units = "cm", scale = 0.5)
writexl::write_xlsx(df7159x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str159x.xlsx")
# 7-167 Klasa 7, strona 167
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str167.png", w7167x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/27.png", demo27, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7167x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str167.xlsx")
# 7-167 Klasa 7, strona 167
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl7str167x.png", w7167x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/28.png", demo28, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df7167x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl7str167x.xlsx")
# 8-31 Klasa 8, strona 31
  ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str31.png", w831x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/29.png", demo29, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df831x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str31.xlsx")
# 8-31 Klasa 8, strona 31
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str31x.png", w831x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/30.png", demo30, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df831x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str31x.xlsx")
# 8-46 Klasa 8, strona 46
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str46.png", w843, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/31.png", demo31, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df846, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str46.xlsx")
# 8-49 Klasa 8, strona 49
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str49.png", w849x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/32.png", demo32, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df849x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str49.xlsx")
# 8-49 Klasa 8, strona 49
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str49x.png", w849x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/33.png", demo33, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df849x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str49x.xlsx")
# 8-54 Klasa 8, strona 54
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str54.png", w854, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/34.png", demo34, width=30, height=20, units = "cm", scale = 0.5)
writexl::write_xlsx(df854, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str54.xlsx")
# 8-137 Klasa 8, strona 137
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str137.png", w8137, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/35.png", demo35, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df8137, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str137.xlsx")
# 8-141 Klasa 8, strona 141
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str141.png", w8141x1, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/36.png", demo36, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df8141x1, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str141.xlsx")
# 8-141 Klasa 8, strona 141
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str141x.png", w8141x2, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/37.png", demo37, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df8141x2, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str141x.xlsx")
# 8-141 Klasa 8, strona 141
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str141xx.png", w8141x3, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/38.png", demo38, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df8141x3, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str141xx.xlsx")
# 8-143 Klasa 8, strona 143
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str143.png", w8143, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/39.png", demo39, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df8143, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str14.xlsx")
# 8-145 Klasa 8, strona 145
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str145.png", w8145, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/40.png", demo40, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df8145, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str145.xlsx")
# 8-164 Klasa 8, strona 164
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/pelne/kl8str164.png", w8164, width=60, height=40, units = "cm", scale = 0.3)
ggsave("C:/Users/adams/Desktop/ary/plots/slupkowe/niepelne/41.png", demo41, width=30, height=20, units = "cm", scale = 0.4)
writexl::write_xlsx(df8164, "C:/Users/adams/Desktop/ary/plots/slupkowe/dane/kl8str164.xlsx")
########################################################################################################################################################



####################################################MAPY_STATYSTYCZNE#############################################################################

df774lasy = get_data_by_variable("194828", unitLevel = 2)
df7112bezrobocie = get_data_by_variable("60270", unitLevel = 2)
df7116ludnosc = get_data_by_variable("63367", unitLevel = 2)

load("bdl.maps.RData")
wojewodztwa = bdl.maps[[2]]

#Udział lasów w powierzchni województw
lasy2015 = df774lasy %>% 
  filter(year == 2015)

wojewodztwa_lasy2015 = wojewodztwa %>% 
  left_join(lasy2015)

w774 = tm_shape(wojewodztwa_lasy2015) +
  tm_polygons("val", title = "Udział lasów\nw powierzchni\nwojewództw\n(%)",
              palette = "Greens") +
  tm_compass(position = c("LEFT", "TOP")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(legend.outside = TRUE)

mapademo1 = tm_shape(wojewodztwa_lasy2015) +
  tm_polygons("val", title = "Bezrobocie w roku 2016 (%)",
              palette = "Greens", legend.show = FALSE)  

#Udział mieszkańców miast w ogólnej liczbie ludności województwa
ludnosc2016 = df7116ludnosc %>% 
  filter(year == 2016)

wojewodztwa_ludnosc2016 = wojewodztwa %>% 
  left_join(ludnosc2016)

w7116 = tm_shape(wojewodztwa_ludnosc2016) +
  tm_polygons("val", title = "Udział mieszkańców\nmiast w ogólnej liczbie\nludności województwa\n(%)",
              palette = "Oranges") +
  tm_compass(position = c("LEFT", "TOP")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(legend.outside = TRUE)

mapademo2 = tm_shape(wojewodztwa_ludnosc2016) +
  tm_polygons("val", title = "Bezrobocie w roku 2016 (%)",
              palette = "Oranges", legend.show = FALSE)  


#Stopa bezrobocia w Polsce
wojewodztwa_bezrobocie = wojewodztwa %>% 
  inner_join(bezrobocie)

#zbiorczy
w112 = tm_shape(wojewodztwa_bezrobocie) +
  tm_polygons("val", title = "Stopa\nbezrobocia\nrejestrowanego\nw Polsce (%)",
              palette = "Blues") +
  tm_facets(by = "year") +
  tm_layout(legend.outside = TRUE)

#animacja 
tm_anim = tm_shape(wojewodztwa_bezrobocie) +
  tm_polygons("val", title = "Stopa\nbezrobocia\nrejestrowanego\nw Polsce (%)",
              palette = "Blues", legend.show = FALSE) +
  tm_facets(along = "year") +
  tm_layout(legend.outside = TRUE)


##############################################################################################################
tmap_save(w774, "C:/Users/adams/Desktop/ary/plots/mapy/kl7str74lasy.png", width=2125, height=1417, scale = 1.5, asp=0)
tmap_save(mapademo1, "C:/Users/adams/Desktop/ary/plots/mapy/jeden.png", width=1000, height=1080, asp=0)
writexl::write_xlsx(df774lasy, "C:/Users/adams/Desktop/ary/plots/mapy/kl7str74lasy.xlsx")

tmap_save(w7116, "C:/Users/adams/Desktop/ary/plots/mapy/kl7str116.png", width=2125, height=1417, scale = 1.5, asp=0)
tmap_save(mapademo2, "C:/Users/adams/Desktop/ary/plots/mapy/trzy.png", width=1000, height=1080, asp=0)
writexl::write_xlsx(df774lasy, "C:/Users/adams/Desktop/ary/plots/mapy/kl7str116mieszkancy.xlsx")

tmap_save(w112, "C:/Users/adams/Desktop/ary/plots/mapy/kl7str112.png", width=2125, height=1417, scale = 1.5, asp=0)
tmap_animation(tm_anim, filename = "C:/Users/adams/Desktop/ary/plots/mapy/animacja.gif", width = 1000, height = 1000)
writexl::write_xlsx(df7112bezrobocie, "C:/Users/adams/Desktop/ary/plots/mapy/kl7str112bezrobocie.xlsx")
