#install.packages("readxl")
#install.packages("tidyr")
#install.packages("stats")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("gridExtra")
install.packages("viridis")
install.packages("corrplot")
#install.packages("glmnet")
#install.packages("car")
install.packages("reshape2")
#install.packages("leaps")
#install.packages("bestglm")
install.packages("lars")

library(readxl)
library(stats)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(viridis)
library(glmnet)
library(car)
library("corrplot")
library("RColorBrewer")
library(reshape2)
library(leaps)
library(bestglm)
library(glmnet)
library(lars)

#rm(list=ls())

data <- read_excel("dataset_spotify_final.xlsx")

column_names <- names(data)
sorted_column_names <- sort(column_names)
print(sorted_column_names)

str(data)

data<- na.omit(data)

data$mode <- factor(data$mode, levels = c(0:1), labels = c("minor", "major"))
data$key <- factor(data$key, levels = c(0:11), labels = c("C", "C#","D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"))
data <- unite(data, key_signature, key, mode, sep = " ")

data$track_duration_sec <-data$duration_ms / 1000
data<- data[,-c(18)]
data<- data %>%
  relocate(track_duration_sec, .after = time_signature)

data$loudness <- as.numeric(data$loudness / 1000)

data$artist_genres<-as.factor(data$artist_genres)
data$danceability<-as.numeric(data$danceability)
data$energy<-as.numeric(data$energy)
data$key_signature<-as.factor(data$key_signature)
data$acousticness<-as.numeric(data$acousticness)
data$speechiness<-as.numeric(data$speechiness)
data$instrumentalness<-as.numeric(data$instrumentalness)
data$liveness<-as.numeric(data$liveness)
data$valence<-as.numeric(data$valence)
data$label<-as.factor(data$label)
data$label_group<-as.factor(data$label_group)
data$artist_name<-as.factor(data$artist_name)
data$film <- factor(data$film, levels = c(0:1), labels = c("no", "yes"))

all_genres <- unique(unlist(strsplit(as.character(data$artist_genres), ", ")))
all_genres <- gsub("\\[|'|\\]", "", all_genres)
all_genres <- all_genres[nchar(all_genres) > 0]

for (genre in all_genres) {
  data[[genre]] <- ifelse(grepl(genre, as.character(data$artist_genres)), 1, 0)
}

data <- subset(data, select = -c(artist_genres))

str(data)

# raggruppiamo alcune delle colonne dummy generi in macrogeneri
indici_pop <- grepl("pop", names(data))
indici_pop[c(3, 5)] <- FALSE

data$big_pop <- rowSums(data[, indici_pop])
data$big_pop <- as.numeric(data$big_pop > 0)

indici_rock <- grepl("rock", names(data))

data$big_rock <- rowSums(data[, indici_rock])
data$big_rock <- as.numeric(data$big_rock > 0)

indici_country <- grepl("country", names(data))

data$big_country <- rowSums(data[, indici_country])
data$big_country <- as.numeric(data$big_country > 0)

indici_jazz <- grepl("jazz", names(data))

data$big_jazz <- rowSums(data[, indici_jazz])
data$big_jazz <- as.numeric(data$big_jazz > 0)

indici_punk <- grepl("punk", names(data))

data$big_punk <- rowSums(data[, indici_punk])
data$big_punk <- as.numeric(data$big_punk > 0)

indici_blues <- grepl("blues", names(data))

data$big_blues <- rowSums(data[, indici_blues])
data$big_blues <- as.numeric(data$big_blues > 0)

indici_ska <- grepl("ska", names(data))

data$big_ska <- rowSums(data[, indici_ska])
data$big_ska <- as.numeric(data$big_ska > 0)

indici_folk <- grepl("folk", names(data))

data$big_folk <- rowSums(data[, indici_folk])
data$big_folk <- as.numeric(data$big_folk > 0)

indici_soul <- grepl("soul", names(data))

data$big_soul <- rowSums(data[, indici_soul])
data$big_soul <- as.numeric(data$big_soul > 0)

indici_metal <- grepl("metal", names(data))

data$big_metal <- rowSums(data[, indici_metal])
data$big_metal <- as.numeric(data$big_metal > 0)

pop_columns <- grep("pop", names(data), value = TRUE)
pop_columns <- pop_columns[!pop_columns %in% c("artist_popularity", "track_popularity", "big_pop")]

data <- data[, !names(data) %in% pop_columns]

rock_columns <- grep("rock", names(data), value = TRUE)
rock_columns <- rock_columns[!rock_columns %in% c("big_rock")]

data <- data[, !names(data) %in% rock_columns]

country_columns <- grep("country", names(data), value = TRUE)
country_columns <- country_columns[!country_columns %in% c("big_country")]

data <- data[, !names(data) %in% country_columns]

jazz_columns <- grep("jazz", names(data), value = TRUE)
jazz_columns <- jazz_columns[!jazz_columns %in% c("big_jazz")]

data <- data[, !names(data) %in% jazz_columns]

punk_columns <- grep("punk", names(data), value = TRUE)
punk_columns <- punk_columns[!punk_columns %in% c("big_punk")]

data <- data[, !names(data) %in% punk_columns]

blues_columns <- grep("blues", names(data), value = TRUE)
blues_columns <- blues_columns[!blues_columns %in% c("big_blues")]

data <- data[, !names(data) %in% blues_columns]

ska_columns <- grep("ska", names(data), value = TRUE)
ska_columns <- ska_columns[!ska_columns %in% c("big_ska")]

data <- data[, !names(data) %in% ska_columns]

folk_columns <- grep("folk", names(data), value = TRUE)
folk_columns <- folk_columns[!folk_columns %in% c("big_folk")]

data <- data[, !names(data) %in% folk_columns]

soul_columns <- grep("soul", names(data), value = TRUE)
soul_columns <- soul_columns[!soul_columns %in% c("big_soul")]

data <- data[, !names(data) %in% soul_columns]

metal_columns <- grep("metal", names(data), value = TRUE)
metal_columns <- metal_columns[!metal_columns %in% c("big_metal")]

data <- data[, !names(data) %in% metal_columns]

str(data)


# colonna "altro" per i brani con NA nei generi ( [""])

data$altro <- ifelse(data$id %in% c("2wCPMWR3y4xclijuCcLJv7", "12YAgUbl6Uk9E7fzopF4Ji", "0di0zKKJ7ZdYcsjeYfAVyR", "7GNsHkiYPcQQjvhTiILFUL",
                                    "7IiWdCebKD9AhpHhqzqhBN", "46pF1zFimM582ss1PrMy68", "49FE3lHyo4yboBYHOeGG98", "7a8FwfEPX4fPZjB8JytSWG", "28AvvdmV8bFzpYVBHbrNZE",
                                    "6oIpL9XkEhyo0bDrCagQQY", "4y69lx0SFFZZrcbyy9sV6n", "213fqUTLbPk2zYxqjlMWZh", "4iZUPqpbdr4cI8OOzlKKqR"), 1, 0)


ggplot(data) +
  geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) +
  geom_density(aes(energy, fill ="energy", alpha = 0.1)) +
  geom_density(aes(speechiness, fill ="speechiness", alpha = 0.1)) +
  scale_x_continuous(name = "Danceability, Energy, Speechiness") +
  scale_y_continuous(name = "Density") +
  ggtitle("Danceability, Energy and Speechiness of christmas songs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        text = element_text(size = 10)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

ggplot(data) +
  geom_density(aes(acousticness, fill ="acousticness", alpha = 0.1)) +
  geom_density(aes(valence, fill ="valence", alpha = 0.1)) +
  geom_density(aes(liveness, fill ="liveness", alpha = 0.1)) +
  scale_x_continuous(name = "Acousticness, Liveness, Valence") +
  scale_y_continuous(name = "Density") +
  ggtitle("Acousticness, Valence and Liveness of christmas songs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        text = element_text(size = 10)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

loudness_density <- ggplot(data) +
  geom_density(aes(loudness, fill ="loudness")) +
  scale_x_continuous(name = "Loudness") +
  scale_y_continuous(name = "Density") +
  ggtitle ("Loudness of christmas songs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        text = element_text(size = 10), legend.position = 'none') +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2")

duration_density <- ggplot(data) +
  geom_density(aes(track_duration_sec, fill ="duration")) +
  scale_x_continuous(name = "Duration") +
  scale_y_continuous(name = "Density") +
  ggtitle ("Duration of christmas songs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        text = element_text(size = 10), legend.position = 'none') +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Paired")

time_signature_density <- ggplot(data) +
  geom_density(aes(time_signature, fill ="time signature")) +
  scale_x_continuous(name = "Time signature") +
  scale_y_continuous(name = "Density") +
  ggtitle ("Time Signature of christmas songs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        text = element_text(size = 10), legend.position = 'none') +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "RdBu")

tempo_density <- ggplot(data) +
  geom_density(aes(tempo, fill ="tempo")) +
  scale_x_continuous(name = "Tempo") +
  scale_y_continuous(name = "Density") +
  ggtitle ("Tempo of christmas songs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        text = element_text(size = 10), legend.position = 'none') +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "viridis")
grid.arrange(loudness_density, duration_density,time_signature_density, tempo_density, nrow = 4)

ggplot(data, aes(x=track_popularity)) +
  geom_histogram(bins = 30, fill="#00AFBB", alpha = 0.4, color = "#00AFBB")+
  geom_vline(xintercept = mean(data$track_popularity), color = "red", linetype = "dashed")+
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  labs(title = "Distribution of Spotify Track Popularity", x="Spotify track popularity",y="Count")


average_popularity<- mean(data$track_popularity)
standard_deviation_popularity<- sd(data$track_popularity)
data$standardized_popularity<- (data$track_popularity-average_popularity) / standard_deviation_popularity


ggplot(data, aes(x=standardized_popularity)) +
  geom_histogram(bins = 30, fill="#00AFBB", alpha = 0.4, color = "#00AFBB")+
  geom_vline(xintercept = mean(data$standardized_popularity), color = "red", linetype = "dashed")+
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  labs(title = "Distribution of Spotify Track Popularity", x="Spotify track popularity",y="Count")


key_counts <- data %>%
  count(key_signature, sort = TRUE)

top_three_keys <- key_counts$key_signature[1:3]

data <- data %>%
  mutate(top_keys = ifelse(key_signature %in% top_three_keys, "Top 3", "Others"))

data %>%
  ggplot(aes(key_signature)) +
  geom_bar(aes(fill = top_keys)) +
  scale_fill_manual(values = c('dark blue', 'dark red', 'dark red', 'dark red')) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = 'none',
    panel.grid = element_line(size = 0.25, linetype = 'solid', colour = "light grey"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(title = "Distribution of Key and Mode among christmas songs", x = "Key", y = "Count")
data <- data[, -c(86)]


ggplot(data, aes(x=artist_popularity)) +
  geom_histogram(bins = 30, fill="#00AFBB", alpha = 0.4, color = "#00AFBB")+
  geom_vline(xintercept = mean(data$track_popularity), color = "red", linetype = "dashed")+
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  labs(title = "Distribution of Spotify Artist Popularity", x="Spotify Artist popularity",y="Count")

str(data)
cor <- cor(data[,-c(1, 2, 4, 5, 8, 16, 20:84)]) #tolgo colonne non numeriche (e track_popularity)

corrplot(cor, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

data <- data[, -c(7)] #tolgo la colonna energy

cor_bin <- cor(data[,-c(1:22, 84)])

std_dev <- apply(data[,-c(1:22, 84)], 2, sd)
zero_variance_columns <- names(std_dev[std_dev == 0])

if (length(zero_variance_columns) > 0) {
  cat("Le seguenti colonne hanno deviazione standard zero o valori costanti:\n")
  cat(zero_variance_columns, "\n")
}

data <- data[, -c(34)] #tolgo children music

cor_bin <- cor(data[,-c(1:21, 83)])
corrplot(cor_bin, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

calcola_correlazioni <- function(dati) {
  colonne_selezionate <- dati[, 22:82]
  matrice_correlazione <- cor(colonne_selezionate, method = "pearson")
  coppie_correlate <- which(abs(matrice_correlazione) > 0.75 | abs(matrice_correlazione) < -0.75, arr.ind = TRUE)
  coppie_correlate <- coppie_correlate[coppie_correlate[,1] != coppie_correlate[,2], ]
  return(coppie_correlate)
}


risultato_correlazioni <- calcola_correlazioni(data)
print(risultato_correlazioni)


data$classical <- rowSums(data[, c("classical", "classical tenor", "italian tenor")])
data$classical <- ifelse(data$classical > 0, 1, 0)
data <- data[, !(colnames(data) %in% c("classical tenor", "italian tenor"))]

data$lounge <- rowSums(data[, c("lounge", "adult standards")])
data$lounge <- ifelse(data$lounge > 0, 1, 0)
data <- data[, !(colnames(data) %in% c("lounge", "adult standards"))]

data$dixieland <- rowSums(data[, c("dixieland", "harlem renaissance")])
data$dixieland <- ifelse(data$dixieland > 0, 1, 0)
data <- data[, !(colnames(data) %in% c("dixieland", "harlem renaissance"))]

data[["early music"]] <- rowSums(data[, c("early music", "english baroque", "german baroque")])
data[["early music"]] <- ifelse(data[["early music"]] > 0, 1, 0)
data <- data[, !(colnames(data) %in% c("english baroque", "german baroque"))]

data$western <- rowSums(data[, c("western swing", "yodeling", "cowboy western")])
data$western <- ifelse(data$western > 0, 1, 0)
data <- data[, !(colnames(data) %in% c("western swing", "yodeling", "cowboy western"))]

data <- data[, c(names(data)[-c(72, 73)], "western", "standardized_popularity")]


## raggruppo in colonna "altro" i generi con meno di 10 canzoni associate ad essi

conteggi <- colSums(data[, 22:72] == 1)
colonne_da_eliminare <- names(conteggi[conteggi < 10])
data$altro <- ifelse(rowSums(data[colonne_da_eliminare]) > 0 | data$altro == 1, 1, 0)
data <- data[, !(names(data) %in% colonne_da_eliminare)]

str(data)

data <- data[, c(names(data)[-c(31, 32)], "altro", "standardized_popularity")]

cor_bin_new <- cor(data[,-c(1:21, 32)])
corrplot(cor_bin_new, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

anyNA(data)


col_with_space <- c('easy listening', 'mellow gold')
col <- colnames(data)
index <- which(col %in% col_with_space)
normal_names <- gsub("[[:space:]-]", "_", col_with_space)
colnames(data)[index] <- normal_names

# tabella di contingenza tra artist_name e label_group
contingency_table <- table(data$artist_name, data$label_group)
#print(contingency_table)
heatmap(contingency_table, Rowv=NA, Colv=NA)

# grafico frequenza genere-artista
genres_subset <- data[, c("easy_listening", "swing", "mellow_gold", "motown", "big_pop", "big_rock", "big_jazz", "big_folk", "big_soul", "altro")]

data_subset <- data.frame(artist_name = data$artist_name, genres_subset)
genre_freq <- aggregate(. ~ artist_name, data = data_subset, FUN = sum) # frequenza dei generi per ogni artista
genre_freq_melted <- melt(genre_freq, id.vars = "artist_name")

ggplot(genre_freq_melted, aes(x = artist_name, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Artisti", y = "Frequenza dei generi") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## MODELLI

set.seed(123)
samp <- sample(1:nrow(data), ceiling(0.80*nrow(data)))
training.data<-data[samp,]
test.data<-data[-samp,]

# training e test set senza standardized_popularity
training.nostand <- training.data[, -c(32)]
test.nostand <- test.data[, -c(32)]

training.new <- training.nostand[, !(names(training.nostand) %in% c("id", "artist_name", "track_name", "label", "album"))]
test.new <- training.nostand[, !(names(training.nostand) %in% c("id", "artist_name", "track_name", "label", "album"))]

x <- model.matrix(~., data=training.new[,-c(2)])
x.vvv <- model.matrix(~., data=test.new[,-c(2)])

# training e test set standardizzati
training.standardized <- training.new
test.standardized <- test.new

training.standardized[, c(1:3, 5:14, 17:26)] <- scale(training.standardized[, c(1:3, 5:14, 17:26)])
test.standardized[, c(1:3, 5:14, 17:26)] <- scale(test.standardized[, c(1:3, 5:14, 17:26)])

x.sd <- model.matrix(~., data=training.standardized[,-c(2)]) # SENZA TRACK POPULARITY
x.sd.vvv <- model.matrix(~., data=test.standardized[,-c(2)])


## SUBSET SELECTION ##

#forward regression

nomi <- names(training.standardized)
scope <- as.formula(paste("~ ", paste(nomi[-c(2)], collapse=" + ")))

fit1 <- step(lm(training.standardized$track_popularity~1, data=training.standardized), scope=scope, direction="forward")
summary(fit1)

p1.forward <- predict(fit1, newdata=test.standardized)
mse.forward <- mean((p1.forward - test.standardized$track_popularity)^2)
mse.forward
#
mae.forward <- mean(abs(p1.forward - test.standardized$track_popularity))
mae.forward

#backward selection

fit2 <- step(lm(training.standardized$track_popularity~., data=training.standardized),  direction="backward")
summary(fit2)
p1.backward <- predict(fit2, newdata=test.standardized)
mse.backward <- mean((p1.backward - test.standardized$track_popularity)^2)
mse.backward

mae.backward <- mean(abs(p1.backward - test.standardized$track_popularity))
mae.backward

# stepwise hybrid

fit3 <- step(lm(training.standardized$track_popularity~., data=test.standardized),  direction="both")
summary(fit3)
p1.both <- predict(fit3, newdata=test.standardized)
mse.both <- mean((p1.both - test.standardized$track_popularity)^2)
mse.both

mae.both <- mean(abs(p1.both - test.standardized$track_popularity))
mae.both

# regression on the principal components

pc <- princomp(x.sd[,-1])

summary(pc)
screeplot(pc, npcs=NCOL(x)-1)

x.pc<-pc$scores[,1:12]

f2 <- as.formula(paste("training.data$track_popularity~", paste(colnames(x.pc),collapse="+"), collapse=NULL))

pc1 <- lm(f2, data=as.data.frame(x.pc))
summary(pc1)

pc.test <- predict(pc, newdata=x.sd.vvv)

ppc1<- predict(pc1, newdata=as.data.frame(pc.test))
mse.pc <- mean((ppc1-test.standardized$track_popularity)^2)
mse.pc

mae.pc <- mean(abs(ppc1-test.standardized$track_popularity))
mae.pc

data.frame(names=c("forward","backward", "both", "pc"),
           mse=c(mse.forward, mse.backward, mse.both, mse.pc),
           mae=c(mae.forward, mae.backward, mae.both, mae.pc))


## LOESS ##

#m.loess <- loess(track_popularity ~ artist_popularity +easy_listening + big_soul, data = training.standardized, span = 0.3, degree = 1)
#fit.loess <- predict(m.loess, newdata = test.standardized)
#theta.loess <- (fit.loess[3]-fit.loess[2])/10 - (fit.loess[2]-fit.loess[1])/10
#theta.loess

calculate_best_span <- function(predictors, response, training_data, test_data, span_values) {
  best_spans <- numeric(length(predictors))
  
  for (i in seq_along(predictors)) {
    min_mse <- Inf
    best_span <- NULL
    for (span in span_values) {
      m.loess_temp <- loess(as.formula(paste(response, "~", paste(predictors[i], collapse = "+"))),
                            data = training_data, span = span, degree = 1)
      fit <- predict(m.loess_temp, newdata = test_data)
      mse <- mean((fit - test_data[[response]])^2)
      if (mse < min_mse) {
        min_mse <- mse
        best_span <- span
      }
    }
    best_spans[i] <- best_span
  }
  names(best_spans) <- predictors
  return(best_spans)
}

predictor_names <- c("artist_popularity", "liveness")
response_variable <- "track_popularity"
span_values_to_test <- seq(0.1, 5, by = 0.3)
best_spans <- calculate_best_span(predictor_names, response_variable, training.standardized, test.standardized, span_values_to_test)

best_spans

m.loess.best <- loess(track_popularity ~ artist_popularity + big_soul,
                      data = training.standardized, span = mean(c(0.1, 0.1)), degree = 1)

fit.loess.best <- predict(m.loess.best, newdata = test.standardized)

mse.loess.best <- mean((fit.loess.best - test.standardized$track_popularity)^2)
mse.loess.best

mae.loess.best <- mean(abs(fit.loess.best - test.standardized$track_popularity))
mae.loess.best


## LEAPS ##

#m1.leaps <- leaps(x, training.new$track_popularity, int=FALSE)

# MA mi accorgo della scritta "warning: n linear dependencies", quindi procedo con VIF
vif_result <- vif(lm(track_popularity ~ ., data = training.standardized))
print(vif_result)

x.vif <- model.matrix(~., data=training.standardized[,-c(2, 4, 15, 25)])
x.vvv.vif <- model.matrix(~., data=test.standardized[,-c(2, 4, 15, 25)])

# ora ho meno di 31 variabili, quindi posso procedere con leaps()
m1.leaps <- leaps(x.vif[, -1], training.standardized$track_popularity, int=FALSE)
str(m1.leaps)
m1.leaps$which[1:3,]

plot(m1.leaps$size, m1.leaps$Cp, xlab="Complexity", ylab = expression(C[p]))

m2.leaps <- leaps(x.vif, training.standardized$track_popularity, int=FALSE, nbest=40)
plot(m2.leaps$size, m2.leaps$Cp, xlab="Complexity", ylab = "Cp")

m3.leaps <- leaps(x.vif, training.standardized$track_popularity, int=FALSE, nbest=1)
m3.leaps$which[1:3,]
points(m3.leaps$size, m3.leaps$Cp, xlab="Complexity", ylab = "Cp", col=3, pch=16)
plot(m3.leaps$size, m3.leaps$Cp, xlab="Complexity", ylab = "Cp")


var.chosen <- m1.leaps$which[m1.leaps$Cp==min(m1.leaps$Cp),] #sceglie modello con valore minimo del criterio cp
nomi <- colnames(x.vif)[var.chosen]
nomi

m1 <- lm(paste("training.standardized$track_popularity ~", paste(nomi[-1], collapse=" + ")), data=data.frame(x.vif)[, -1]) #modello lineare con variabili selezionate
summary(m1)

p1.leaps <- predict(m1, newdata=data.frame(x.vvv.vif)[, -1])

mse.leaps <- mean((p1.leaps - test.standardized$track_popularity)^2)
mse.leaps

mae.leaps <- mean(abs(p1.leaps-test.standardized$track_popularity))
mae.leaps

# Leave-one-out cross-validation

cval <- NULL
for(i in 1:NROW(m1.leaps$which)) {
  m11 <- lm(training.standardized$track_popularity~as.matrix(x.vif[, m1.leaps$which[i,]])-1)
  cval[i] <- sum(((training.standardized$track_popularity-predict(m11))/(1-hatvalues(m11)))^2)/NROW(training.standardized$track_popularity)
  cat(i,"")
}

cval
plot(m1.leaps$size, cval, xlab="Complexity", ylab = "CV error")

chosen <-  cval==min(cval)
dati.cv <- data.frame(x.vif[, m1.leaps$which[chosen,]])
m11 <- lm(training.standardized$track_popularity~., data=dati.cv)
summary (m11)

p11<-predict(m11, newdata=data.frame(x.vvv.vif[, m1.leaps$which[chosen,]]))
mse.cv <- mean((p11-test.standardized$track_popularity)^2)
mse.cv
mae.cv <- mean(abs(p11-test.standardized$track_popularity))
mae.cv


## LASSO ##

# per ridge e lasso utilizziamo variabili standardizzate
#x.sd <- model.matrix(~., data=training.standardized[,-c(2)])
#x.sd.vvv <- model.matrix(~., data=test.standardized[,-c(2)])

m.lasso=glmnet(x.sd[,-1], training.standardized$track_popularity, alpha = 1)
plot(m.lasso)
plot(m.lasso, xvar="lambda", main="model coefficient paths")
plot(m.lasso, xvar="dev", main="model coefficient paths")

m.lasso
cv.lasso <- cv.glmnet(x.sd[,-1], training.standardized$track_popularity)

plot(cv.lasso)

cv.lasso$lambda.min
cv.lasso$lambda.1se
abline(h=cv.lasso$cvm[cv.lasso$lambda==cv.lasso$lambda.min]+cv.lasso$cvsd[cv.lasso$lambda==cv.lasso$lambda.min])

plot(cv.lasso)

p.lasso <- predict(m.lasso, newx=x.sd.vvv[,-1])
str(p.lasso)

mse.lasso <- apply((p.lasso - test.standardized$track_popularity)^2, 2, mean)
mse.lasso

mae.lasso <- apply(abs(p.lasso - test.standardized$track_popularity), 2, mean)
mae.lasso

#cv
coef(m.lasso, s=cv.lasso$lambda.min)[coef(m.lasso, cv.lasso$lambda.min)!=0]
lasso.best.cv <- glmnet(x.sd[,-1], training.standardized$track_popularity, alpha = 1, lambda = cv.lasso$lambda.min)
lasso.best.cv

mse.las <- mse.lasso[cv.lasso$lambda==cv.lasso$lambda.min]
mae.las <- mae.lasso[cv.lasso$lambda==cv.lasso$lambda.min]


## RIDGE ##

# per ridge e lasso utilizzo variabili standardizzate
#x.sd <- model.matrix(~., data=training.standardized[,-c(2)])
#x.sd.vvv <- model.matrix(~., data=test.standardized[,-c(2)])

m.ridge=glmnet(x.sd[,-1], training.standardized$track_popularity, alpha=0)
plot(m.ridge)
plot(m.ridge, xvar="lambda", main="model coefficient paths")
plot(m.ridge, xvar="dev", main="model coefficient paths")

m.ridge
cv.ridge <- cv.glmnet(x.sd[,-1], training.standardized$track_popularity, alpha=0)

plot(cv.ridge)

cv.ridge$lambda.min
cv.ridge$lambda.1se

p.ridge <- predict(m.ridge, newx=x.sd.vvv[,-1])
str(p.ridge)
mse.ridge <- apply((p.ridge - test.standardized$track_popularity)^2, 2, mean)
mse.ridge

mae.ridge <- apply(abs(p.ridge - test.standardized$track_popularity), 2, mean)
mae.ridge

#cv
coef(m.ridge, s=cv.ridge$lambda.min)[coef(m.ridge, cv.ridge$lambda.min)!=0]
ridge.best.cv <- glmnet(x.sd[,-1], training.standardized$track_popularity, alpha = 0, lambda = cv.ridge$lambda.min)
ridge.best.cv

mse.rid <- mse.ridge[cv.ridge$lambda==cv.ridge$lambda.min]
mae.rid <- mae.ridge[cv.ridge$lambda==cv.ridge$lambda.min]


## BEST GLM ##

# m1.best <-
#   bestglm(Xy = data.frame(x[,-c(1,4,14,19)], y= training.data$track_popularity),
#           IC = "AIC")
# summary(m1.best)

# Warning message in leaps.setup(x, y, wt = weights, nbest = nbest, nvmax = nvmax, :
#“7  linear dependencies found”

# Il codice gira a vuoto. Questo potrebbe essere dovuto a un problema di ram o a dipendenze lineari troppo forti
# che non permettono al modello di funzionare correttamente. A questo punto passiamo direttamente a RIDGE e LASSO per gestire le dipendenze
# e le eventuali multicollinearità tra le variabili.

# Quindi bestglm con le variabili selezionate da LASSO.

selected_coef <- coef(lasso.best.cv) # estraggo i coefficienti non nulli per il valore di lambda ottimale selezionato dalla c-v
selected_variables <- which(selected_coef != 0)

new.x.sd <- x.sd[, selected_variables]

m1.best <- bestglm(Xy = data.frame(new.x.sd[, -1], y = training.standardized$track_popularity), IC = "AIC")
summary(m1.best)

m1.best$BestModels

m2 <- m1.best$BestModel
summary(m2)
par(mfrow=c(2,2))
plot(m2, cex=0.2)
par(mfrow=c(1,1))

#colnames(x.sd.vvv)
#colnames(m1.best$BestModels)

selected_columns <- colnames(x.sd.vvv)[unlist(m1.best$BestModels[1, --ncol(m1.best$BestModels)])]
predict_data <- data.frame(x.sd.vvv[, selected_columns], y = test.standardized$track_popularity)

p2.best <- predict(m2, newdata = predict_data)

mse.best <- mean((p2.best-test.standardized$track_popularity)^2)
mse.best

mae.best <- mean(abs(p2.best-test.standardized$track_popularity))
mae.best


## LARS: Least Angle Regression ##

m.lars <- lars(x.sd[, -1], training.standardized$track_popularity, type = "lasso", trace = FALSE, normalize = TRUE, max.steps = 1000)
plot(m.lars, xvar="norm", main="model coefficient paths", breaks = FALSE) # in funzione della norma dei coefficienti

cv_lars <- cv.lars(x.sd[,-1], training.standardized$track_popularity, type = "lasso")

p.lars <- predict.lars(m.lars, newx=x.sd.vvv[,-1])
str(p.lars)

mse.lars <- apply((p.lars$fit - test.standardized$track_popularity)^2, 2, mean)
mse.lars

mae.lars <- apply(abs(p.lars$fit - test.standardized$track_popularity), 2, mean)
mae.lars

#cv
num_vars <- which.min(cv_lars$cv.error)

m.lars.best <- lars(x.sd[, -1], training.standardized$track_popularity, type = "lasso", trace = FALSE, normalize = TRUE, max.steps = num_vars)

p.lars.best <- predict.lars(m.lars.best, newx = x.sd.vvv[, -1])

mse.lars.best <- mean((p.lars.best$fit - test.standardized$track_popularity)^2)
mse.lars.best

mae.lars.best <- mean(abs(p.lars.best$fit - test.standardized$track_popularity))
mae.lars.best

data.frame(names=c("forward","backward", "both", "pc","loess", "leaps", "bestglm", "lasso", "ridge", "lars"),
           mse=c(mse.forward, mse.backward, mse.both, mse.pc, mse.loess.best, mse.leaps, mse.best, mse.las, mse.rid, mse.lars.best),
           mae=c(mae.forward, mae.backward, mae.both, mae.pc, mae.loess.best, mae.leaps, mae.best, mae.las, mae.rid, mae.lars.best))


## SVM ##

install.packages("gcdnet")
library(gcdnet)

str(training.standardized)

x.svm <- model.matrix(~., data=training.standardized[,-c(16)])
x.svm.vvv <- model.matrix(~., data=test.standardized[,-c(16)])

fit1 <- gcdnet(x.svm,training.standardized$film, method = "sqsvm")
plot(fit1)

cvfit1 <- cv.gcdnet(x.svm,training.standardized$film, method = "sqsvm", lambda.factor = 0.0000000001)
plot(cvfit1)

cvfit1$lambda.min
cvfit1$lambda.1se

plot(fit1, xvar = "lambda")

abline(v = log(cvfit1$lambda.min), col =1) #we have the minimum
abline(v = log(cvfit1$lambda.1se), col =4) #we obtain the minimum number of variable

coef(fit1, s = cvfit1$lambda.min)[coef(fit1, cvfit1$lambda.min) != 0]

coef.est1 =as.matrix(coef(fit1, s = cvfit1$lambda.min))
str(coef,est1)
coef.vec1 <- subset(coef.est1, coef.est1!=0)
coef.vec1

# previsioni sul set di test
predictions <- predict(fit1, newx = x.svm.vvv, s = cvfit1$lambda.min)

confusion_matrix <- table(predictions, test.standardized$film)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

print(confusion_matrix)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")








