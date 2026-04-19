#codice Rstudio

#evoluzione atleti
# Creare un vettore con gli anni
anni <- c(1896, 1900, 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 
  1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 
  1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024)
# vettore con il numero di atleti
numero_atleti <- c(241, 997, 691, 2008, 2587, 0, 2626, 3089, 2883, 1332, 3963, 
  0, 0, 4104, 4955, 3314, 5338, 5151, 5516, 7134, 6084, 5179, 
  6829, 8391, 9356, 10318, 10651, 10625, 10942, 10500, 11238, 11315, 10500)

#  serie temporale
olympic_ts <- ts(numero_atleti, start = 1896, frequency = 1/4)
plot(olympic_ts, type = "p", col = "lightblue3", lwd = 2, pch = 19, 
  xlab = "Anno", ylab = "Numero di Atleti", xaxt = "n")
axis(1, at = c(1900, 1920, 1940, 1960, 1980, 2000, 2020), 
  labels = c(1900, 1920, 1940, 1960, 1980, 2000, 2020), las = 2)
#top 20 medagliati
library(dplyr)
library(ggplot2)
library(tidyr)
medals_data <- data.frame(
  Paese = c("Stati Uniti", "Totale paesi russi*", "Totale paesi tedeschi**", "Gran Bretagna", 
    "Francia", "Cina", "Italia", "Australia", "Ungheria", "Svezia", 
    "Giappone", "Canada", "Paesi Bassi", "Romania", "Finlandia", 
    "Polonia", "Corea del Sud", "Cuba", "Bulgaria", "Svizzera"),
  Oro = c(1061, 609, 438, 285, 222, 262, 216, 167, 181, 148, 
    169, 71, 95, 90, 101, 72, 96, 85, 54, 53),
  Argento = c(836, 514, 455, 316, 253, 199, 188, 174, 154, 176, 
    150, 108, 104, 98, 85, 89, 91, 71, 88, 79),
  Bronzo = c(738, 504, 490, 315, 274, 173, 213, 212, 176, 179, 
    178, 147, 122, 122, 119, 137, 100, 85, 82, 73)
)


medals_data <- medals_data %>%
  mutate(Totale = Oro + Argento + Bronzo)

#qui li metto in formato lungo
medals_data_long <- medals_data %>%
  pivot_longer(cols = c(Oro, Argento, Bronzo), names_to = "Tipo", values_to = "Conteggio")

# creo il grafico
ggplot(medals_data_long, aes(x = reorder(Paese, Totale), y = Conteggio, fill = Tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Distribuzione delle Medaglie per i 20 Migliori Paesi (1896-2020)", 
    x = "Paese", 
    y = "Numero di Medaglie") +
  scale_fill_manual(values = c("Oro" = "#FFD700", "Argento" = "#C0C0C0", "Bronzo" = "#CD7F32")) +
  theme_minimal() +
  geom_text(aes(label = Conteggio), position = position_stack(vjust = 0.5), color = "black") +
  theme(legend.position = "bottom")
###################
#evoluzione per continente sovrapposte

anni=c(1894,1900,1904,1908,1912,1916,1920,1924,1928,1932,1936,1940,1944,1948,
  1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,
  2008,2012,2016,2020,2024)

n_europa=c(11,16,8,15,19,0,18,25,29,22,28,0,0,25,29,26,30,28,31,32,34,27,28,33,
  38,47,47,47,48,48,49,49,50)

n_america=c(2,6,3,3,3,0,5,10,9,8,12,0,0,17,18,17,20,21,32,32,21,15,36,37,39,
  42,42,42,42,41,41,37,32)

n_asia=c(0,3,0,2,3,0,2,4,3,4,5,0,0,13,16,19,19,22,22,24,17,15,28,36,34,44,
  43,45,44,45,44,45,49)

n_africa=c(0,0,1,1,2,0,2,2,3,1,2,0,0,2,4,7,12,21,24,29,16,22,41,42,45,52,53,52,53,53,
  54,54,55)

n_oceania=c(1,1,1,1,1,0,2,2,2,2,2,0,0,2,2,3,3,2,3,3,3,2,7,11,11,12,14,15,17,
  17,17,17,16)
# Definizione dei colori pastello e delle forme più piccole
colori <- c('#A6CEE3', '#FDBF6F', '#B2DF8A', '#FB9A99', '#CAB2D6')  # Colori pastello omogenei
forme <- c(20, 17, 18, 19, 15)  # qui concateno le forme pch

# Creazione del grafico sovrapposto 
plot(anni, n_europa, type='p', col=colori[1], pch=forme[1], xlab='Anno', ylab='Numero di Paesi',
  ylim=c(0, max(n_europa, n_america, n_asia, n_africa, n_oceania)), 
  cex=1, lwd=1.5)

# Aggiunta dei punti per gli altri continenti 
points(anni, n_america, type='p', col=colori[2], pch=forme[2], cex=1, lwd=1.5)
points(anni, n_asia, type='p', col=colori[3], pch=forme[3], cex=1, lwd=1.5)
points(anni, n_africa, type='p', col=colori[4], pch=forme[4], cex=1, lwd=1.5)
points(anni, n_oceania, type='p', col=colori[5], pch=forme[5], cex=1, lwd=1.5)

# Aggiunta della legenda
legend("topright", legend=c("Europa", "America", "Asia", "Africa", "Oceania"), 
  col=colori, pch=forme, cex=0.5, box.lwd=0.5, box.col="black", 
  inset=c(0.8, 0.02), xpd=TRUE)
######
#numero di paesi partecipanti nel tempo
# Dati
Anno <- c(1896, 1900, 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2021, 2024)
Paesi <- c(14, 24, 12, 22, 28, 0, 29, 44, 46, 37, 49, 0, 0, 59, 69, 72, 83, 93, 112, 121, 92, 80, 140, 159, 169, 197, 199, 201, 201, 204, 207, 206, 206)

# Aumenta il margine destro per la legenda
par(mar = c(5, 4, 4, 7) + 0.1)

# Crea il grafico a dispersione (senza unire i puntini)
plot(Anno, Paesi, type='p', col='darkgreen', pch=16,
  xlab='Anno', ylab='Numero di Paesi Partecipanti',
  ylim=range(Paesi))
####################
#completo
#capitolo 2 elaborazioni
#il dataset di 16 variabili costruito da zero
maratoneti=data.frame(nome=c("Hassan Sifan", "Assefa Tigst", "Obiri Hellen", "Lokedi Sharon", "Shankule Amane Beriso",
  "Shauri Magdalena", "Ocampo Daiana", "Navarrete Esther", "Chelimo Rose", "Cheptegei Rebecca",
  "Steyn Gerda", "Evans Clara", "Galbadrakh Khishigsaikhan", "Bayartsogt Munkhzaya", "Tiyouri Maor", 
  "Bai Li", "Mukandanga Clementine", "Harvey Rose", "Shrestha Shantoshi", "Lhamo Kinzang",
  #2020
  "Peres Jepchirchir", "Brigid Kosgei", "Molly Seidel", "Roza Dereje", "Volha Mazuronak",
  "Darya Maslova", "Marta Galimany", "Susan Jeptooo", "Stephanie Davis", "Jovana De La Cruz",
  "Rosa Chacha", "Yevheniya Prokofyeva", "Nazret Weldu", "Andrea Deelstra", "Bayartsogtyn Mönkhzaya",
  "Juliet Chekwel", "Catarina Ribeiro", "Jess Piasecki", "Sharon Firisua", "Dayna Pidhoresky", 
  #2016
  "Jemima Jelagat Sumgong", "Eunice Jepkirui Kirwa", "Mare Dibaba", "Tirfi Tsegaye", "Volha Mazuronak", 
  "Svitlana Stanko-Klymenko", "Mayada Sayyad", "Nyakisi Adero", "Adriana Aparecida Da Silva", 
  "Gyeong-Hui Im", "Beverly Ramos", "Munkhzaya Bayartsogt", "Erika Abril", "Manuela Soccol", 
  "Alina Armas", "Niluka Rajasekara", "Natthaya Thanaronnawat", "Jie Shi Neo", "Sarah Attar", "Nary Ly",
  #2024
  "Tola Tamirat", "Abdi Bashir", "Kipruto Benson", "Cairess Emile", 
  "Geleta Deresa", "TALBI Zouhair", "LEVINS Cameron", "KIPLANGAT Victor", 
  "ABRAHAM Tadesse", "BEKELE Kenenisa", "WU Xiangdong", "ROJO Yago", 
  "GABRIYESOS Tachlowini", "FANIEL Eyob", "AMARE Girmaw", "HE Jie", 
  "NOVALES Tariku", "IVANOVSKI Dario", "BETOUDJI Valentin", "BAT-OCHIR Ser-Od",
  #2020
  "Eulid Kipchoge", "Abdi Nageeye", "Bashir Abdi", "Lawrence Cherono", 
  "Ayad Lamdassem", "Adam Nowicki", "Olivier Irabaruta", "Sondre Nordstad Moen", 
  "Abdi Abdirahman", "Tomas Hilifa Rainhold", "Derlys Ayala", "Fred Musobo", 
  "Sidi-Hassan Chahdi", "Ben Preisner", "Yassine El Fathaoui", "Cam Levins", 
  "Yuma Hattori", "Jesus Arturo Esparza", "Jorge Castelblanco", "Ivan Zarco Alvarez", 
  #2016
  "Eliud Kipchoge", "Feyisa Lilesa", "Galen Rupp", "Ghirmay Ghebreslassie", 
  "Alphonce Felix Simbu", "Mynhardt Mbeumuna Kawanivi", "Julian Flugel", 
  "Daviti Kharazishvili", "Rachid Kisri", "Maru Teferi", "Remigijus Kancys", 
  "Christian Kreienbuhl", "Mohamed Hrezi", "Solonei Da Silva", "Andres Ruiz", 
  "Derlys Ayala", "Federico Bruno", "Jungsub Shim", "Kuniaki Takizaki", "Methkal Abu Drais"),
  eta=c(31, 30, 34, 30, 32, 
    28, 33, 34, 35, 33, 
    34, 30, 34, 30, 34, 
    28, 38, 32, 31,26,
    #2020
    27, 27, 27, 24, 32, 
    26, 35, 34, 30, 29, 
    38, 26, 31, 36, 27,31, 
    31, 31, 27, 34, 
    #2016
    31, 32, 26,31,27,40, 23, 30, 35,33, 28,22, 
    38,28,32,34, 37, 31, 23, 44,
    33, 35, 33, 26, 28, 29, 35, 24, 42, 42, 30, 29, 26, 31, 36, 25, 26, 27, 33, 42,
    #2020
    36, 32, 32, 33, 39, 30, 30, 31, 44, 30, 31, 27, 32, 41, 34, 29, 30, 36, 30, 30,
    #2016
    32, 26, 30, 20, 24, 32, 30, 24, 41, 24, 29, 35, 25, 34, 28, 26, 23, 25, 39, 32),
  medaglia=c(1,1,1,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    #2020
    1,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    #2016
    1,1,1,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  partecipazioni = c(3, 2, 4, 1, 1,
    1, 1, 1, 2, 1,
    2, 1, 1, 3, 3, 2, 1, 1, 1,
    1,
    #2020
    1, 1, 1, 1,2,
    2, 1, 1, 1, 2,
    3, 1, 1,1, 2, 2,1,1,
    2, 1,
    #2016
    1, 1, 2,1,
    1, 1, 1, 1,2, 2,
    2, 1, 2, 1,
    1, 1, 1, 1, 2, 1,
    2, 3, 1, 1, 1, 1, 3, 1, 3, 4, 1, 1, 2, 2, 2, 1, 1, 1, 1, 6,
    4, 2, 2, 1, 3, 1, 3, 2, 5, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1,
    3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 2
  ),
  altezza=c(170,168,160,168,165,157,164,162,153,166,168,164,159,160,148,155,158,168,160,159,
    #2020
    152,152,165,168,165,165,169,163,157,161,155,168,160,164,160,165,170,170,152,165,
    #2016
    160,155,156,162,165,168,160,160,166,163,168,160,164,157,166,155,158,160,165,160,
    
    
    181,170,173,180,174,174,180,168,178,165,177,177,174,174,172,166,178,178,171,169,
    167,165,176,165,172,175,170,178,178,178,178,170,169,178,169,178,176,170,
    169,179,
    
    167,175,181,167,175,178,183,171,179,164,173,186,180,172,164,178,185,168,
    151,168),
  peso=c(49,53,50,47,45,45,50,49,43,54,54,53,53,43,44,48,53,58,50,50,
    #2020
    43,42,52,55,49,44,52,46,50,53,48,50,50,46,43,52,54,54,46,50,
    #2016
    45,49,45,52,49,47,58,50,52,43,52,43,52,42,53,42,47,50,52,55,
    
    59,56,52,50,62,66,65,55,61,56,73,68,64,53,60,65,59,69,63,60,
    59,57,54,56,52,52,60,62,62,61,62,75,57,60,60,53,65,61,58,66,
    57,67,61,57,55,59,64,68,60,52,62,68,66,65,64,75,66,58,55,67
  ),
  continente=c("Europa","Africa","Africa","Africa","Africa","Africa","America","Europa","Asia","Africa",
    "Africa","Europa","Asia","Asia","Asia","Asia","Africa","Europa","Asia","Asia",
    
    "Africa","Africa","America","Africa","Europa","Asia","Europa","Europa","Europa","America",
    "America","Europa","Africa","Europa","Asia","Africa","Europa","Europa","Africa","Europa",
    
    "Africa","Africa","Africa","Africa","Europa","Europa","Asia","Africa","America","Asia",
    "America","Asia","America","Europa","Africa","Asia","Asia","Asia","Asia","Asia",
    "Africa","Europa","Africa","Europa","Africa","Africa","America","Africa","Europa","Africa",
    "Asia","Europa","Africa","Europa","Asia","Asia","Europa","Europa","Europa","Asia",
    #2020
    "Africa","Europa","Europa","Africa","Europa","Europa","Africa","Europa","America","Africa","America",
    "Africa","Europa","America","Europa","America","Asia","America","America","America",
    #2016
    "Africa","Africa","America","Africa","Africa","Africa","Europa","Europa","Africa","Europa","Europa",
    "Europa","Africa","America","America","America","America","Asia","Asia","Asia"),
  temperatura=c(rep(19.8,20),rep(26.3,20),rep(21,20),rep(17.3,20),rep(27,20),rep(22,20)),
  dislivello=c(rep(436,20),rep(108,20),rep(40,20),rep(436,20),rep(108,20),rep(40,20)),
  pendenza_max=c(rep(13.5,20),rep(2,20),rep(2.5,20),rep(13.5,20),rep(2,20),rep(2.5,20)),
  umidita=c(rep(65,20),rep(76,20),rep(83,20),rep(73,20),rep(78,20),rep(94,20)),
  velocita_vento=c(rep(5.4,20),rep(11.1,20),rep(9.4,20),rep(7.6,20),rep(22.2,20),rep(11.2,20)),
  BMI=c(16.95502, 18.77834, 19.53125, 16.65249, 16.52893, 18.25632, 18.59012, 18.67093, 18.36900, 19.59646, 19.13265, 
    19.70553, 20.96436, 16.79687, 20.08766, 19.97919, 21.23057, 20.54989, 19.53125, 19.77770, 18.61150, 18.17867, 
    19.10009, 19.48696, 17.99816, 16.16162, 18.20665, 17.31341, 20.28480, 20.44674, 19.97919, 17.71542, 19.53125, 
    17.10291, 16.79687, 19.10009, 18.68512, 18.68512, 19.90997, 18.36547, 17.57812, 20.39542, 18.49112, 19.81405, 
    17.99816, 16.65249, 22.65625, 19.53125, 18.87066, 16.18427, 18.42404, 16.79687, 19.33373, 17.03923, 19.23356, 
    17.48179, 18.82711, 19.53125, 19.10009, 21.48437,18.00922, 19.37716, 17.37445, 15.43210, 20.47827, 21.79945, 20.06173, 19.48696, 19.25262, 20.56933, 23.30109, 
    21.70513, 21.13886, 17.50562, 20.28123, 23.58833, 18.62139, 21.77755, 21.54509, 21.00767, 21.15529, 20.93664, 
    17.43285, 20.56933, 17.57707, 16.97959, 20.76125, 19.56824, 19.56824, 19.25262, 19.56824, 25.95156, 19.95728, 
    18.93700, 21.00767, 16.72769, 20.98399, 21.10727, 20.30741, 20.59861, 20.43817, 21.87755, 18.61970, 20.43817, 
    17.95918, 18.62139, 19.11075, 23.25502, 18.72601, 19.33373, 20.71569, 19.65545, 20.37037, 21.97134, 23.79536, 
    23.67125, 19.28415, 20.54989, 24.12175, 23.73866),
  rapporto_altezza_peso=c(3.469, 3.170, 3.200, 3.574, 3.667, 3.489, 3.280, 3.306, 3.558, 3.074, 
    3.111, 3.094, 3.000, 3.721, 3.364, 3.229, 2.981, 2.897, 3.200, 3.180, 3.535, 3.619, 3.173,
    3.055, 3.367, 3.750, 3.250, 3.543, 3.140, 3.038, 3.229, 3.360, 3.200, 3.565, 3.721, 3.173, 
    3.148, 3.148, 3.304, 3.300, 3.556, 3.163, 3.467, 3.115, 3.367, 3.574, 2.759, 3.200, 3.192, 
    3.791, 3.231, 3.721, 3.154, 3.738, 3.132, 3.690, 3.362, 3.200, 3.173, 2.909,
    
    3.067797, 3.035714, 3.326923, 3.600000, 2.806452, 2.636364, 2.769231, 3.054545, 2.918033, 2.946429, 2.424658, 
    2.602941, 2.718750, 3.283019, 2.866667, 2.553846, 3.016949, 2.579710, 2.714286, 2.816667, 2.830508, 2.894737, 
    3.259259, 2.946429, 3.307692, 3.365385, 2.833333, 2.870968, 2.870968, 2.918033, 2.870968, 2.266667, 2.964912, 
    2.966667, 2.816667, 3.358491, 2.707692, 2.786885, 2.913793, 2.712121, 2.929825, 2.611940, 2.967213, 2.929825, 
    3.181818, 3.016949, 2.859375, 2.514706, 2.983333, 3.153846, 2.790323, 2.735294, 2.727273, 2.646154, 2.562500, 
    2.373333, 2.803030, 2.896552, 2.745455, 2.507463),
  tempo=c("2:22:55", "2:22:58", "2:23:10", "2:23:14", "2:23:57",
    "2:31:58", "2:32:02", "2:32:07", "2:32:08", "2:32:14",
    "2:32:51", "2:33:01", "2:33:26", "2:33:27", "2:33:37", 
    "2:44:44", "2:45:40", "2:51:03", "2:55:06","3:52:59",
    #2020
    "2:27:20", "2:27:36", "2:27:46", "2:28:38", "2:29:06",
    "2:35:35", "2:35:39", "2:36:29", "2:36:33", "2:36:38",
    "2:36:44", "2:36:47", "2:37:01", "2:37:05", "2:37:08",
    "2:53:40", "2:55:01", "2:55:39", "3:02:10", "3:03:10",
    #2016
    "2:24:04", "2:24:13", "2:24:30", "2:24:47", "2:24:48",
    "2:42:26", "2:42:28", "2:42:39", "2:43:22", "2:43:31",
    "2:43:52", "2:43:55", "2:44:05", "2:44:18", "2:44:20", 
    "3:11:05", "3:11:31", "3:15:18", "3:16:11", "3:20:20",
    
    "2:06:26", "2:06:47", "2:07:00", "2:07:29", "2:07:31", "2:11:51", 
    "2:11:56", "2:11:59", "2:12:22", "2:12:24", "2:12:34", "2:12:43", 
    "2:12:47", "2:12:50", "2:12:51", "2:22:31", "2:25:50", "2:28:15", 
    "2:32:11", "2:42:33", "2:08:38", "2:09:58", "2:10:00", "2:10:02", 
    "2:10:16", "2:17:19", "2:17:44", "2:17:59", "2:18:27", "2:18:28", 
    "2:18:34", "2:18:39", "2:18:40", "2:19:27", "2:19:44", "2:28:43", 
    "2:30:08", "2:31:51", "2:33:22", "2:44:36", "2:08:44", "2:09:54", 
    "2:10:05", "2:11:04", "2:11:15", "2:20:45", "2:20:47", "2:20:47", 
    "2:21:00", "2:21:06", "2:21:10", "2:21:13", "2:21:17", "2:22:05", 
    "2:22:09", "2:22:09", "2:40:05", "2:42:42", "2:39:55", "2:46:18"),
  sesso=c(rep("F",60),rep("M",60)))

#qui trasformo il tempo,era formato stringa
convert_time=function(time_string){
  time_parts=strsplit(time_string,":")[[1]]
  return(as.numeric(time_parts[1])*3600+as.numeric(time_parts[2])*60+as.numeric((time_parts[3]))*1)
}
maratoneti$tempo=sapply(maratoneti$tempo,convert_time)

# Converto le variabili numeriche
maratoneti$eta <- as.numeric(maratoneti$eta)
maratoneti$medaglia <- as.numeric(maratoneti$medaglia)
maratoneti$partecipazioni <- as.numeric(maratoneti$partecipazioni)
maratoneti$altezza <- as.numeric(maratoneti$altezza)
maratoneti$peso <- as.numeric(maratoneti$peso)
maratoneti$temperatura <- as.numeric(maratoneti$temperatura)
maratoneti$dislivello <- as.numeric(maratoneti$dislivello)
maratoneti$pendenza_max <- as.numeric(maratoneti$pendenza_max)
maratoneti$umidita <- as.numeric(maratoneti$umidita)
maratoneti$velocita_vento <- as.numeric(maratoneti$velocita_vento)
maratoneti$BMI <- as.numeric(maratoneti$BMI)
maratoneti$rapporto_altezza_peso <- as.numeric(maratoneti$rapporto_altezza_peso)
maratoneti$tempo <- as.numeric(maratoneti$tempo)
#######
maratoneti_donne <- subset(maratoneti, sesso == "F") #qui prendo il dataset delle donne
maratoneti_donne_numeric <- maratoneti_donne[, sapply(maratoneti_donne, is.numeric)]

# Creazione del dataset per uomini
maratoneti_uomini <- subset(maratoneti, sesso == "M") #e qui degli uomini
maratoneti_uomini_numeric <- maratoneti_uomini[, sapply(maratoneti_uomini, is.numeric)]
######

X_numeric <- maratoneti[, sapply(maratoneti, is.numeric)]


#
boxplot(eta ~ medaglia, data = maratoneti_uomini, 
  main = "Boxplot dell'età per medagliati e non medagliati",
  xlab = "Medagliato (0 = No, 1 = Sì)", ylab = "Età",
  col = c("lightblue", "lightgreen"))


boxplot(rapporto_altezza_peso~medaglia,data=maratoneti_uomini,
  xlab="medagliato (0=No, 1=Si)",ylab="rapporto_alt_peso",
  col=c("lightblue","lightgreen"))
boxplot(BMI~medaglia,data=maratoneti_uomini,
  xlab="medagliato (0=No, 1=Si)",ylab="BMI",
  col=c("lightblue","lightgreen"))
#boxplot bmi
boxplot(BMI~medaglia,data=maratoneti_uomini,
  xlab="medagliato (0=No, 1=Si)",ylab="BMI",
  col=c("lightblue","lightgreen"))

#tempo

boxplot(tempo~medaglia,data=maratoneti_uomini,
  xlab="medagliato (0=No, 1=Si)",ylab="Tempo",
  col=c("lightblue","lightgreen"))


####boxplot donne
boxplot(eta ~ medaglia, data = maratoneti_donne, 
  main = "Boxplot dell'età per medagliati e non medagliati",
  xlab = "Medagliato (0 = No, 1 = Sì)", ylab = "Età",
  col = c("lightblue", "lightgreen"))


boxplot(rapporto_altezza_peso~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="rapporto_alt_peso",
  col=c("lightblue","lightgreen"))
boxplot(BMI~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="BMI",
  col=c("lightblue","lightgreen"))

boxplot(tempo~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="Tempo",
  col=c("lightblue","lightgreen"))
###summary
summary(maratoneti_uomini$eta)
summary(maratoneti_uomini$tempo)
summary(maratoneti_uomini$rapporto_altezza_peso)
summary(maratoneti_uomini$BMI)
summary(maratoneti_uomini$partecipazioni)
#età medagliati e non
summary(maratoneti_uomini[maratoneti_uomini$medaglia==1,]$eta)
summary(maratoneti_uomini[maratoneti_uomini$medaglia==0,]$eta)
#tempo medagliati e non
summary(maratoneti_uomini[maratoneti_uomini$medaglia==1,]$tempo)
summary(maratoneti_uomini[maratoneti_uomini$medaglia==0,]$tempo)
#rapporto alt peso
summary(maratoneti_uomini[maratoneti_uomini$medaglia==1,]$rapporto_altezza_peso)
summary(maratoneti_uomini[maratoneti_uomini$medaglia==0,]$rapporto_altezza_peso)
#BMI
summary(maratoneti_uomini[maratoneti_uomini$medaglia==1,]$BMI)
summary(maratoneti_uomini[maratoneti_uomini$medaglia==0,]$BMI)
#partecipazioni
summary(maratoneti_uomini[maratoneti_uomini$medaglia==1,]$partecipazioni)
summary(maratoneti_uomini[maratoneti_uomini$medaglia==0,]$partecipazioni)

#continente
summary(maratoneti_uomini$continente)
summary(maratoneti_uomini[maratoneti_uomini$medaglia==1,]$continente)
summary(maratoneti_uomini[maratoneti_uomini$medaglia==0,]$continente)


# Dati per i maratoneti medagliati (medaglia == 1)
maratoneti_medagliati <- c(5, 1, 3)  # Africa, America, Asia, Europa
labels_medagliati <- c("Africa", "America", "Europa")
colors_medagliati <- c("black", "red", "green")

# Calcoliamo le percentuali
percentuali_medagliati <- round(maratoneti_medagliati / sum(maratoneti_medagliati) * 100, 1)

# Etichette con percentuali
labels_medagliati_perc <- paste(labels_medagliati, "\n", percentuali_medagliati, "%")

# Creazione del grafico a torta per i maratoneti medagliati
pie(maratoneti_medagliati, 
  labels = labels_medagliati_perc, 
  col = colors_medagliati, 
  main = "Maratoneti Medagliati per Continente",
  border = "white")

# Dati per i maratoneti non medagliati (medaglia == 0)
maratoneti_non_medagliati <- c(14, 12, 8, 17)  # Africa, America, Asia, Europa
labels_non_medagliati <- c("Africa", "America", "Asia", "Europa")
colors_non_medagliati <- c("black", "red", "yellow", "green")

# Calcoliamo le percentuali
percentuali_non_medagliati <- round(maratoneti_non_medagliati / sum(maratoneti_non_medagliati) * 100, 1)

# Etichette con percentuali
labels_non_medagliati_perc <- paste(labels_non_medagliati, "\n", percentuali_non_medagliati, "%")

# Creazione del grafico a torta per i maratoneti non medagliati
pie(maratoneti_non_medagliati, 
  labels = labels_non_medagliati_perc, 
  col = colors_non_medagliati, 
  main = "Maratoneti Non Medagliati per Continente",
  border = "white")


############################ Donne
boxplot(eta ~ medaglia, data = maratoneti_donne, 
  main = "Boxplot dell'età per medagliati e non medagliati",
  xlab = "Medagliata (0 = No, 1 = Sì)", ylab = "Età",
  col = c("lightblue", "lightgreen"))


boxplot(rapporto_altezza_peso~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="rapporto_alt_peso",
  col=c("lightblue","lightgreen"))
boxplot(BMI~medaglia,data=maratoneti_donne,
  xlab="medagliata (0=No, 1=Si)",ylab="BMI",
  col=c("lightblue","lightgreen"))

boxplot(tempo~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="Tempo",
  col=c("lightblue","lightgreen"))


####boxplot donne
boxplot(eta ~ medaglia, data = maratoneti_donne, 
  main = "Boxplot dell'età per medagliati e non medagliati",
  xlab = "Medagliato (0 = No, 1 = Sì)", ylab = "Età",
  col = c("lightblue", "lightgreen"))


boxplot(rapporto_altezza_peso~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="rapporto_alt_peso",
  col=c("lightblue","lightgreen"))
boxplot(BMI~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="BMI",
  col=c("lightblue","lightgreen"))

boxplot(tempo~medaglia,data=maratoneti_donne,
  xlab="medagliato (0=No, 1=Si)",ylab="Tempo",
  col=c("lightblue","lightgreen"))
###summary
summary(maratoneti_donne$eta)
summary(maratoneti_donne$tempo)
summary(maratoneti_donne$rapporto_altezza_peso)
summary(maratoneti_donne$BMI)
summary(maratoneti_donne$partecipazioni)
#età medagliati e non
summary(maratoneti_donne[maratoneti_donne$medaglia==1,]$eta)
summary(maratoneti_donne[maratoneti_donne$medaglia==0,]$eta)
#tempo medagliati e non
summary(maratoneti_donne[maratoneti_donne$medaglia==1,]$tempo)
summary(maratoneti_donne[maratoneti_donne$medaglia==0,]$tempo)
#rapporto alt peso
summary(maratoneti_donne[maratoneti_donne$medaglia==1,]$rapporto_altezza_peso)
summary(maratoneti_donne[maratoneti_donne$medaglia==0,]$rapporto_altezza_peso)
#BMI
summary(maratoneti_donne[maratoneti_donne$medaglia==1,]$BMI)
summary(maratoneti_donne[maratoneti_donne$medaglia==0,]$BMI)
#partecipazioni
summary(maratoneti_donne[maratoneti_donne$medaglia==1,]$partecipazioni)
summary(maratoneti_donne[maratoneti_donne$medaglia==0,]$partecipazioni)

#continente
summary(maratoneti_donne$continente)
summary(maratoneti_donne[maratoneti_donne$medaglia==1,]$continente)
summary(maratoneti_donne[maratoneti_donne$medaglia==0,]$continente)

##########GRAFICI A TORTA MEDAGLIATI E NON
# Dati per le maratonete medagliate
medagliate <- c(Africa = 7, America = 1, Europa = 1)
# Calcolo delle percentuali
percentuali_medagliate <- round(100 * medagliate / sum(medagliate), 1)

# Creazione del grafico a torta per le medagliate
pie(medagliate, 
  labels = paste(names(medagliate), percentuali_medagliate, "%"), 
  col = c("black", "red", "green"), 
  main = "Maratonete Medagliate per Continente")

# Dati per le maratonete non medagliate
non_medagliate <- c(Africa = 13, America = 6, Asia = 17, Europa = 15)
# Calcolo delle percentuali
percentuali_non_medagliate <- round(100 * non_medagliate / sum(non_medagliate), 1)

# Creazione del grafico a torta per le non medagliate
pie(non_medagliate, 
  labels = paste(names(non_medagliate), percentuali_non_medagliate, "%"), 
  col = c("black", "red", "yellow", "green"), 
  main = "Maratonete Non Medagliate per Continente")

#analisi sulle performance
library(ggplot2)
library(dplyr)

performance_data <- data.frame(
  atleta = c(rep("Bashir Abdi", 10), 
    rep("Tamirat Tola", 10), 
    rep("Benson Kipruto", 10), 
    rep("Eliud Kipchoge", 10), 
    rep("Abdi Nageeye", 10), 
    rep("Feyisa Lilesa", 10), 
    rep("Galen Rupp", 10)),
  eta = c(30, 30, 31, 32, 33, 33, 33, 34, 34, 35,
    22, 25, 26, 30, 30, 30, 31, 31, 32, 33,
    26, 26, 27, 27, 28, 29, 31, 32, 32, 33,
    28, 29, 30, 31, 32, 33, 34, 37, 37, 38,
    28, 28, 30, 30, 31, 32, 33, 34, 34, 35,
    20, 20, 22, 22, 23, 24, 25, 25, 26, 28,
    29, 30, 31, 31, 32, 32, 35, 35, 36, 37),
  tempo = c("2:07:03", "2:06:14", "2:04:49", "2:03:36", "2:05:23", "2:06:48", "2:05:19", "2:03:47", "2:04:32", "2:06:47",
    "2:06:17", "2:04:11", "2:04:06", "2:03:39", "2:05:36", "2:04:14", "2:03:40", "2:04:59", "2:04:58", "2:06:26",
    "2:09:51", "2:07:21", "2:07:11", "2:07:24", "2:05:13", "2:06:42", "2:04:24", "2:04:02", "2:02:16", "2:07:00",
    "2:04:05", "2:04:11", "2:04:00", "2:03:05", "2:03:32", "2:01:39", "2:02:37", "2:02:40", "2:01:09", "2:02:42",
    "2:09:34", "2:08:16", "2:06:17", "2:07:39", "2:07:09", "2:09:58", "2:04:56", "2:05:32", "2:10:21", "2:04:45",
    "2:05:23", "2:08:10", "2:08:20", "2:04:52", "2:07:46", "2:08:26", "2:06:35", "2:06:57", "2:06:56", "2:07:30",
    "2:11:13", "2:10:05", "2:09:20", "2:06:07", "2:06:21", "2:09:20", "2:11:41", "2:06:35", "2:09:36", "2:08:48"),
  stringsAsFactors = FALSE
)

performance_data$tempo_sec <- as.numeric(sapply(strsplit(performance_data$tempo, ":"), function(x) as.numeric(x[1]) * 3600 + as.numeric(x[2]) * 60 + as.numeric(x[3])))








ggplot(performance_data, aes(x = atleta, y = tempo_sec, fill = atleta)) +
  geom_boxplot() +
  scale_y_reverse(labels = function(x) format(as.POSIXct(x, origin="1970-01-01", tz="UTC"), "%H:%M:%S")) +
  labs(title = "Distribuzione delle Performance per Atleta",
    x = "Atleta",
    y = "Tempo (hh:mm:ss)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Pacchetti necessari
library(ggplot2)
library(dplyr)

# Dati di performance (per atleta, con tempi in formato "hh:mm:ss")
performance_data <- data.frame(
  atleta = c("Hassan Sifan", "Hassan Sifan", "Hassan Sifan", "Hassan Sifan", 
    "Assefa Tigst", "Assefa Tigst", "Assefa Tigst", "Assefa Tigst", "Assefa Tigst", 
    "Obiri Hellen", "Obiri Hellen", "Obiri Hellen",
    "Peres Jepchirchir", "Peres Jepchirchir", "Peres Jepchirchir", "Peres Jepchirchir", "Peres Jepchirchir", 
    "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei", 
    "Molly Seidel", "Molly Seidel", "Molly Seidel", "Molly Seidel", "Molly Seidel",
    "Jenima Jelet Sumgong", "Jenima Jelet Sumgong", "Jenima Jelet Sumgong", "Jenima Jelet Sumgong", "Jenima Jelet Sumgong",
    "Eunice Kirwa", "Eunice Kirwa", "Eunice Kirwa", "Eunice Kirwa", "Eunice Kirwa", "Eunice Kirwa", 
    "Mare Dibaba", "Mare Dibaba", "Mare Dibaba", "Mare Dibaba", "Mare Dibaba", "Mare Dibaba", "Mare Dibaba", "Mare Dibaba"),
  
  tempo = c("02:13:44", "02:18:05", "02:18:33", "02:22:55",
    "02:11:53", "02:15:37", "02:16:23", "02:22:58", "02:34:01", 
    "02:23:10", "02:25:49", "02:27:23", 
    "02:16:16", "02:17:16", "02:18:38", "02:22:39", "02:26:51",
    "02:14:04", "02:16:02", "02:18:20", "02:18:35", "02:18:40", "02:18:58", "02:19:02",
    "02:23:07", "02:24:42", "02:25:13", "02:27:31", "02:27:46",
    "02:20:48", "02:22:58", "02:23:27", "02:24:04", "02:24:23",
    "02:21:17", "02:21:41", "02:22:08", "02:22:40", "02:23:34", "02:24:13",
    "02:19:52", "02:19:52", "02:20:21", "02:21:25", "02:21:36", "02:23:25", "02:24:09", "02:24:30")
)

# Funzione per convertire tempo in secondi
convert_to_seconds <- function(time_str) {
  hms <- as.numeric(unlist(strsplit(time_str, ":")))
  return(hms[1] * 3600 + hms[2] * 60 + hms[3])
}

# Aggiungo colonna con tempo in secondi
performance_data <- performance_data %>%
  mutate(tempo_sec = sapply(tempo, convert_to_seconds))

# Creazione del boxplot
ggplot(performance_data, aes(x = atleta, y = tempo_sec, fill = atleta)) +
  geom_boxplot() +
  scale_y_reverse(labels = function(x) format(as.POSIXct(x, origin="1970-01-01", tz="UTC"), "%H:%M:%S")) +
  labs(title = "Distribuzione delle Performance per Atleta",
    x = "Atleta",
    y = "Tempo (hh:mm:ss)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Pacchetti necessari
library(ggplot2)
library(dplyr)

# Dati di performance (per atleta, con tempi in formato "hh:mm:ss")
performance_data <- data.frame(
  atleta = c(
    "Assefa Tigst", "Assefa Tigst", "Assefa Tigst", "Assefa Tigst", "Assefa Tigst", 
    
    "Peres Jepchirchir", "Peres Jepchirchir", "Peres Jepchirchir", "Peres Jepchirchir", "Peres Jepchirchir", 
    "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei", "Brigid Kosgei",
    "Molly Seidel", "Molly Seidel", "Molly Seidel", "Molly Seidel", "Molly Seidel",
    "Jenima Jelet Sumgong", "Jenima Jelet Sumgong", "Jenima Jelet Sumgong", "Jenima Jelet Sumgong", "Jenima Jelet Sumgong",
    "Eunice Kirwa", "Eunice Kirwa", "Eunice Kirwa", "Eunice Kirwa", "Eunice Kirwa",
    "Mare Dibaba", "Mare Dibaba", "Mare Dibaba", "Mare Dibaba", "Mare Dibaba"),
  
  tempo = c(
    "02:11:53", "02:15:37", "02:16:23", "02:22:58", "02:34:01", 
    "02:16:16", "02:17:16", "02:18:38", "02:22:39", "02:26:51",
    "02:14:04", "02:16:02", "02:18:20", "02:18:35", "02:18:40", 
    "02:23:07", "02:24:42", "02:25:13", "02:27:31", "02:27:46",
    
    "02:20:48", "02:22:58", "02:23:27", "02:24:04", "02:24:23",
    
    "02:21:17", "02:21:41", "02:22:08", "02:22:40", "02:23:34",
    "02:19:52", "02:19:52", "02:20:21", "02:21:25", "02:21:36")
)

# Funzione per convertire tempo in secondi
convert_to_seconds <- function(time_str) {
  hms <- as.numeric(unlist(strsplit(time_str, ":")))
  return(hms[1] * 3600 + hms[2] * 60 + hms[3])
}

# Aggiungo colonna con tempo in secondi
performance_data <- performance_data %>%
  mutate(tempo_sec = sapply(tempo, convert_to_seconds))

# Creazione del boxplot
ggplot(performance_data, aes(x = atleta, y = tempo_sec, fill = atleta)) +
  geom_boxplot() +
  scale_y_reverse(labels = function(x) format(as.POSIXct(x, origin="1970-01-01", tz="UTC"), "%H:%M:%S")) +
  labs(title = "Distribuzione delle Performance per Atleta",
    x = "Atleta",
    y = "Tempo (hh:mm:ss)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##########################
#regressione logistica:applicazione ai dati
#nricarichiamo il dataset
maratoneti=data.frame(nome=c("Hassan Sifan", "Assefa Tigst", "Obiri Hellen", "Lokedi Sharon", "Shankule Amane Beriso",
  "Shauri Magdalena", "Ocampo Daiana", "Navarrete Esther", "Chelimo Rose", "Cheptegei Rebecca",
  "Steyn Gerda", "Evans Clara", "Galbadrakh Khishigsaikhan", "Bayartsogt Munkhzaya", "Tiyouri Maor", 
  "Bai Li", "Mukandanga Clementine", "Harvey Rose", "Shrestha Shantoshi", "Lhamo Kinzang",
  #2020
  "Peres Jepchirchir", "Brigid Kosgei", "Molly Seidel", "Roza Dereje", "Volha Mazuronak",
  "Darya Maslova", "Marta Galimany", "Susan Jeptooo", "Stephanie Davis", "Jovana De La Cruz",
  "Rosa Chacha", "Yevheniya Prokofyeva", "Nazret Weldu", "Andrea Deelstra", "Bayartsogtyn Mönkhzaya",
  "Juliet Chekwel", "Catarina Ribeiro", "Jess Piasecki", "Sharon Firisua", "Dayna Pidhoresky", 
  #2016
  "Jemima Jelagat Sumgong", "Eunice Jepkirui Kirwa", "Mare Dibaba", "Tirfi Tsegaye", "Volha Mazuronak", 
  "Svitlana Stanko-Klymenko", "Mayada Sayyad", "Nyakisi Adero", "Adriana Aparecida Da Silva", 
  "Gyeong-Hui Im", "Beverly Ramos", "Munkhzaya Bayartsogt", "Erika Abril", "Manuela Soccol", 
  "Alina Armas", "Niluka Rajasekara", "Natthaya Thanaronnawat", "Jie Shi Neo", "Sarah Attar", "Nary Ly",
  #2024
  "Tola Tamirat", "Abdi Bashir", "Kipruto Benson", "Cairess Emile", 
  "Geleta Deresa", "TALBI Zouhair", "LEVINS Cameron", "KIPLANGAT Victor", 
  "ABRAHAM Tadesse", "BEKELE Kenenisa", "WU Xiangdong", "ROJO Yago", 
  "GABRIYESOS Tachlowini", "FANIEL Eyob", "AMARE Girmaw", "HE Jie", 
  "NOVALES Tariku", "IVANOVSKI Dario", "BETOUDJI Valentin", "BAT-OCHIR Ser-Od",
  #2020
  "Eulid Kipchoge", "Abdi Nageeye", "Bashir Abdi", "Lawrence Cherono", 
  "Ayad Lamdassem", "Adam Nowicki", "Olivier Irabaruta", "Sondre Nordstad Moen", 
  "Abdi Abdirahman", "Tomas Hilifa Rainhold", "Derlys Ayala", "Fred Musobo", 
  "Sidi-Hassan Chahdi", "Ben Preisner", "Yassine El Fathaoui", "Cam Levins", 
  "Yuma Hattori", "Jesus Arturo Esparza", "Jorge Castelblanco", "Ivan Zarco Alvarez", 
  #2016
  "Eliud Kipchoge", "Feyisa Lilesa", "Galen Rupp", "Ghirmay Ghebreslassie", 
  "Alphonce Felix Simbu", "Mynhardt Mbeumuna Kawanivi", "Julian Flugel", 
  "Daviti Kharazishvili", "Rachid Kisri", "Maru Teferi", "Remigijus Kancys", 
  "Christian Kreienbuhl", "Mohamed Hrezi", "Solonei Da Silva", "Andres Ruiz", 
  "Derlys Ayala", "Federico Bruno", "Jungsub Shim", "Kuniaki Takizaki", "Methkal Abu Drais"),
  eta=c(31, 30, 34, 30, 32, 
    28, 33, 34, 35, 33, 
    34, 30, 34, 30, 34, 
    28, 38, 32, 31,26,
    #2020
    27, 27, 27, 24, 32, 
    26, 35, 34, 30, 29, 
    38, 26, 31, 36, 27,31, 
    31, 31, 27, 34, 
    #2016
    31, 32, 26,31,27,40, 23, 30, 35,33, 28,22, 
    
    38,28,32,34, 37, 31, 23, 44,
    
    #2024
    33, 35, 33, 26, 28, 29, 35, 24, 42, 42, 30, 29, 26, 31, 36, 25, 26, 27, 33, 42,
    #2020
    36, 32, 32, 33, 39, 30, 30, 31, 44, 30, 31, 27, 32, 41, 34, 29, 30, 36, 30, 30,
    #2016
    32, 26, 30, 20, 24, 32, 30, 24, 41, 24, 29, 35, 25, 34, 28, 26, 23, 25, 39, 32),
  medaglia=c(1,1,1,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    #2020
    1,1,1,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    #2016
    1,1,1,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    0,0,0,0,0,
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  partecipazioni = c(3, 2, 4, 1, 1,
    1, 1, 1, 2, 1,
    2, 1, 1, 3, 3, 2, 1, 1, 1,
    1,
    #2020
    1, 1, 1, 1,2,
    2, 1, 1, 1, 2,
    3, 1, 1,1, 2, 2,1,1,
    2, 1,
    #2016
    1, 1, 2,1,
    1, 1, 1, 1,2, 2,
    2, 1, 2, 1,
    1, 1, 1, 1, 2, 1,
    2, 3, 1, 1, 1, 1, 3, 1, 3, 4, 1, 1, 2, 2, 2, 1, 1, 1, 1, 6,
    4, 2, 2, 1, 3, 1, 3, 2, 5, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1,
    3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 2
  ),
  
  continente=c("Europa","Africa","Africa","Africa","Africa","Africa","America","Europa","Asia","Africa",
    "Africa","Europa","Asia","Asia","Asia","Asia","Africa","Europa","Asia","Asia",
    
    "Africa","Africa","America","Africa","Europa","Asia","Europa","Europa","Europa","America",
    "America","Europa","Africa","Europa","Asia","Africa","Europa","Europa","Africa","Europa",
    
    "Africa","Africa","Africa","Africa","Europa","Europa","Asia","Africa","America","Asia",
    "America","Asia","America","Europa","Africa","Asia","Asia","Asia","Asia","Asia",
    
    "Africa","Europa","Africa","Europa","Africa","Africa","America","Africa","Europa","Africa",
    "Asia","Europa","Africa","Europa","Asia","Asia","Europa","Europa","Europa","Asia",
    #2020
    "Africa","Europa","Europa","Africa","Europa","Europa","Africa","Europa","America","Africa","America",
    "Africa","Europa","America","Europa","America","Asia","America","America","America",
    #2016
    "Africa","Africa","America","Africa","Africa","Africa","Europa","Europa","Africa","Europa","Europa",
    "Europa","Africa","America","America","America","America","Asia","Asia","Asia"),
  dislivello=c(rep(436,20),rep(108,20),rep(40,20),rep(436,20),rep(108,20),rep(40,20)),
  umidita=c(rep(65,20),rep(76,20),rep(83,20),rep(73,20),rep(78,20),rep(94,20)),
  velocita_vento=c(rep(5.4,20),rep(11.1,20),rep(9.4,20),rep(7.6,20),rep(22.2,20),rep(11.2,20)),
  BMI=c(16.95502, 18.77834, 19.53125, 16.65249, 16.52893, 18.25632, 18.59012, 18.67093, 18.36900, 19.59646, 19.13265, 
    19.70553, 20.96436, 16.79687, 20.08766, 19.97919, 21.23057, 20.54989, 19.53125, 19.77770, 18.61150, 18.17867, 
    19.10009, 19.48696, 17.99816, 16.16162, 18.20665, 17.31341, 20.28480, 20.44674, 19.97919, 17.71542, 19.53125, 
    17.10291, 16.79687, 19.10009, 18.68512, 18.68512, 19.90997, 18.36547, 17.57812, 20.39542, 18.49112, 19.81405, 
    17.99816, 16.65249, 22.65625, 19.53125, 18.87066, 16.18427, 18.42404, 16.79687, 19.33373, 17.03923, 19.23356, 
    17.48179, 18.82711, 19.53125, 19.10009, 21.48437,
    18.00922, 19.37716, 17.37445, 15.43210, 20.47827, 21.79945, 20.06173, 19.48696, 19.25262, 20.56933, 23.30109, 
    21.70513, 21.13886, 17.50562, 20.28123, 23.58833, 18.62139, 21.77755, 21.54509, 21.00767, 21.15529, 20.93664, 
    17.43285, 20.56933, 17.57707, 16.97959, 20.76125, 19.56824, 19.56824, 19.25262, 19.56824, 25.95156, 19.95728, 
    18.93700, 21.00767, 16.72769, 20.98399, 21.10727, 20.30741, 20.59861, 20.43817, 21.87755, 18.61970, 20.43817, 
    17.95918, 18.62139, 19.11075, 23.25502, 18.72601, 19.33373, 20.71569, 19.65545, 20.37037, 21.97134, 23.79536, 
    23.67125, 19.28415, 20.54989, 24.12175, 23.73866),
  tempo=c("2:22:55", "2:22:58", "2:23:10", "2:23:14", "2:23:57",
    "2:31:58", "2:32:02", "2:32:07", "2:32:08", "2:32:14",
    "2:32:51", "2:33:01", "2:33:26", "2:33:27", "2:33:37", 
    "2:44:44", "2:45:40", "2:51:03", "2:55:06","3:52:59",
    #2020
    "2:27:20", "2:27:36", "2:27:46", "2:28:38", "2:29:06",
    "2:35:35", "2:35:39", "2:36:29", "2:36:33", "2:36:38",
    "2:36:44", "2:36:47", "2:37:01", "2:37:05", "2:37:08",
    "2:53:40", "2:55:01", "2:55:39", "3:02:10", "3:03:10",
    #2016
    "2:24:04", "2:24:13", "2:24:30", "2:24:47", "2:24:48",
    "2:42:26", "2:42:28", "2:42:39", "2:43:22", "2:43:31",
    "2:43:52", "2:43:55", "2:44:05", "2:44:18", "2:44:20", 
    "3:11:05", "3:11:31", "3:15:18", "3:16:11", "3:20:20",
    
    "2:06:26", "2:06:47", "2:07:00", "2:07:29", "2:07:31", "2:11:51", 
    "2:11:56", "2:11:59", "2:12:22", "2:12:24", "2:12:34", "2:12:43", 
    "2:12:47", "2:12:50", "2:12:51", "2:22:31", "2:25:50", "2:28:15", 
    "2:32:11", "2:42:33", "2:08:38", "2:09:58", "2:10:00", "2:10:02", 
    "2:10:16", "2:17:19", "2:17:44", "2:17:59", "2:18:27", "2:18:28", 
    "2:18:34", "2:18:39", "2:18:40", "2:19:27", "2:19:44", "2:28:43", 
    "2:30:08", "2:31:51", "2:33:22", "2:44:36", "2:08:44", "2:09:54", 
    "2:10:05", "2:11:04", "2:11:15", "2:20:45", "2:20:47", "2:20:47", 
    "2:21:00", "2:21:06", "2:21:10", "2:21:13", "2:21:17", "2:22:05", 
    "2:22:09", "2:22:09", "2:40:05", "2:42:42", "2:39:55", "2:46:18"),
  sesso=c(rep("F",60),rep("M",60)))
convert_time=function(time_string){
  time_parts=strsplit(time_string,":")[[1]]
  return(as.numeric(time_parts[1])*3600+as.numeric(time_parts[2])*60+as.numeric((time_parts[3]))*1)
}
maratoneti$tempo=sapply(maratoneti$tempo,convert_time)



#questa funzione individua il miglior modello basato sul criterio AIC e lo ritona
find_best_model <- function(data, response_var, predictors) {
  
  # Creiamo un vettore per memorizzare i modelli
  model_list <- list()
  aic_values <- c()
  
  # Generiamo tutte le possibili combinazioni di variabili predittive
  for (i in 1:length(predictors)) {
    combs <- combn(predictors, i, simplify = FALSE)
    
    for (comb in combs) {
      # Creiamo la formula del modello
      formula <- as.formula(paste(response_var, "~", paste(comb, collapse = "+")))
      
      # Costruiamo il modello di regressione logistica
      model <- glm(formula, data = data, family = "binomial")
      
      # Memorizziamo il modello e il suo AIC
      model_list[[paste(comb, collapse = "+")]] <- model
      aic_values <- c(aic_values, AIC(model))
    }
  }
  
  # Troviamo il modello con l'AIC più basso
  best_model_index <- which.min(aic_values)
  best_model_name <- names(model_list)[best_model_index]
  best_model <- model_list[[best_model_name]]
  
  # Ritorna il miglior modello e il suo AIC
  return(list(best_model = best_model, 
    best_model_name = best_model_name, 
    best_aic = aic_values[best_model_index]))
}



data <- maratoneti # Carichiamo il dataset
data$medaglia <- as.factor(data$medaglia) # Convertiamo la variabile target in fattore

# Chiamiamo la funzione
risultato <- find_best_model(data = data, 
  response_var = "medaglia", 
  predictors = c("eta","sesso","tempo","partecipazioni","continente","dislivello",
    "umidita","velocita_vento","BMI"))

# Stampiamo il miglior modello e il suo AIC
summary(risultato$best_model)
cat("Il miglior modello è:", risultato$best_model_name, "con AIC =", risultato$best_aic, "\n")
mod=glm(medaglia~sesso+tempo+partecipazioni+dislivello,family = "binomial",data=maratoneti)
summary(mod)

#nuovo dataset per previsioni
new_data <- data.frame(
  eta = c(30, 31, 28), # esempio età
  partecipazioni = c(3, 2, 1), # esempio partec
  continente = factor(c("Africa", "America", "Europa"), levels = levels(maratoneti$continente)) # Example continents
)


maratoneti$continente <- as.factor(maratoneti$continente)

# Modello di regressione logistica
model <- glm(medaglia ~ sesso + tempo + partecipazioni + dislivello, 
  family = binomial, data = maratoneti)

# Controlla il sommario del modello
summary(model)


maratoneti$medaglia <- as.factor(maratoneti$medaglia)
maratoneti$continente <- as.factor(maratoneti$continente)
maratoneti$sesso <- as.factor(maratoneti$sesso)

model_interaction_no <- glm(medaglia ~ tempo + partecipazioni + dislivello + continente+sesso, 
  family = binomial, data = maratoneti)
summary(model_interaction_no)
#####
model_inter_bis=glm(medaglia ~ -1+tempo +partecipazioni + dislivello + continente* sesso, 
  family = binomial, data = maratoneti)
summary(model_inter_bis)
###
plot(model_inter_bis$residuals) #outliers atleti d'elitè, che devono essere inclusi nel modello
plot(model_interaction_no$residuals)
###
X_dummy <- model.matrix(~ sesso + continente - 1, data = maratoneti)
X_numeric <- maratoneti[, c("tempo", "dislivello", "BMI")]
X_combined <- cbind(X_numeric, X_dummy)
cor(X_combined) #regressori stimati
##
prob_med=round(maratoneti$prob_medaglia <- predict(model_inter_bis, newdata = maratoneti, type = "response"),2)
prob_med_mod1=round(maratoneti$prob_medaglia <- predict(model_interaction_no, newdata = maratoneti, type = "response"),2)
#ho esplorato differenze delle stime tra i modelli

model_inter_bis=glm(medaglia ~ -1+tempo + dislivello + continente* sesso, 
  family = binomial, data = maratoneti)
summary(model_inter_bis)
########
result <- data.frame(
  nome = maratoneti$nome,
  prob_medaglia = round(maratoneti$prob_medaglia, 2),
  sesso = maratoneti$sesso  # variabile sesso per distinguere i gruppi
)

# Separiamo i dati per maschi e femmine
result_m <- subset(result, sesso == "M")  # Maschi
result_f <- subset(result, sesso == "F")  # Femmine

# Tracciamo i grafici separati per uomini e donne
plot(result_m$prob_medaglia, 
  col = "blue", 
  pch = 16, 
  xlab = "Atleti", 
  ylab = "Probabilità di Medaglia", 
  main = "Probabilità di Medaglia per Uomini e Donne")
points(result_f$prob_medaglia, 
  col = "red", 
  pch = 16)

##############################################################################################################à

#sono state controllate le correlazioni tra variabili esplicative stimate nel modello->per verificare assenza di multicollinearità
#i residui si distribuiscono tra -2 e 2 con moda in intervallo -2;0 
#si tiene conto della tendenza dei residui nella risposta rispetto al gruppo delle donne e degli uomini
#Y è una variabile dicotomica e quindi si presta bene alla trattazione del fenomeno attraverso la regressione logistica
#la presenza di eventuali outliers potrebbe influenzare i risultati: Si tenga presente che questi spesso 
#rappresentano atleti di elitè difficilmente replicabili e devono essere inclusi nelle nostre osservazioni
#in quanto l'obiettivo è la comprensione dei fattori rilevanti e di eventuali relazioni tra essi nella
#vittoria della medaglia olimpica
#la regressione logistica non richiede la normalità della variabile risposta, ma è richiesta una relazione
#di tipo lineare tra regressori e log-odds della probabilità della vittoria della medaglia.






