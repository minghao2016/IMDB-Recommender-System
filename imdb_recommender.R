library(data.table)
library(ggplot2)
library(lme4)
options(scipen = 8)

#load dataset
movies <- fread("data/movies.csv")
genres <- fread("data/genres.csv")
people <- fread("data/people.csv")
roles <- fread("data/roles.csv")

setnames(movies, paste0("V", 1:4), c("movie_id", "title", "num_votes", "avg_rating"))
setnames(genres, paste0("V", 1:2), c("movie_id", "genre"))
setnames(people, paste0("V", 1:2), c("person_id", "name"))
setnames(roles, paste0("V", 1:3), c("movie_id", "person_id", "role"))

#average movie rating and its variability for each genre
#note that a movie can have multiple genres
setkey(movies, movie_id)
setkey(genres, movie_id)
mg_merge <- merge(movies, genres)
mg_merge[, sum_rating := num_votes*avg_rating]
avg_rating_by_genre <- mg_merge[, .(avg_rating = sum(sum_rating)/sum(num_votes),
                                    sd_avg_rating = sd(avg_rating)), by = genre][order(-avg_rating)]
avg_rating_by_genre$genre <- as.factor(avg_rating_by_genre$genre)
avg_rating_by_genre$genre <- reorder(avg_rating_by_genre$genre, avg_rating_by_genre$avg_rating)

qplot(genre, avg_rating, data = avg_rating_by_genre) + geom_bar(stat = "identity") + coord_flip() +
  xlab("Average Rating") + ylab("Movie Genre") + ggtitle("Average Rating by Movie Genre")
p <- ggplot(data = avg_rating_by_genre, aes(x = avg_rating, y = sd_avg_rating)) +
  geom_point(aes(colour = "red"), na.rm = T) + xlim(6.5, 8.5) +
  geom_text(aes(label = genre), fontface = "bold", size = 4, na.rm = T, hjust = 1, vjust = 1) +
  xlab("Average Rating") + ylab("Rating Variability") + ggtitle("Rating Mean vs Standard Deviation")
plot(p)

#top 10 popularity ranking of actors, directors, and writers for each genre
#by the total number of ratings for the movies they were involved in
setkey(roles, person_id)
setkey(people, person_id)
rp_merge <- merge(people, roles)

setkey(rp_merge, movie_id)
setkey(mg_merge, movie_id)
all <- merge(mg_merge, rp_merge, allow.cartesian = T)
all[, , by = genre]

#predict average rating of a movie from genre, actor, director, and writer
#note that multiple inputs should be allowed for each input
data <- all[, .(movie_id, title, avg_rating, genre, person_id, name, role)]
data$genre <- as.factor(data$genre)
data$person_id <- as.factor(data$person_id)
data$role <- as.factor(data$role)
str(data)

#fit a bayesian linear mixed model
#set genre and role as fixed effect and person_id as random effect
fit <- lmer(avg_rating ~ genre + role + (role|person_id), data = data) #this may take a lot of time
fit1 <- lmer(avg_rating ~ genre + role + (1|person_id), data = data, verbose = T)
print(summary(fit), cor = F)
print(summary(fit1), cor = F)

#compare the two nested models using a likelihood ratio test
fit.mle <- update(fit, REML = F)
fit1.mle <- update(fit1, REML = F)
anova(fit.mle, fit1.mle)

#fitted values
fitted_values <- fitted(fit1)
resid <- data$avg_rating - as.numeric(fitted_values)
plot(resid)
