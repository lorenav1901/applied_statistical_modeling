imdb_movies <- read.csv("imdb_top_1000.csv", stringsAsFactors = FALSE)

str(imdb_movies)
summary(imdb_movies)
sapply(imdb_movies, class)

bad_year_rows <- imdb %>%
  filter(!str_detect(Released_Year, "^[0-9]{4}$")) %>%
  select(Series_Title, Released_Year, Certificate)

bad_year_rows

head(imdb_movies$Runtime)
head(imdb_movies$Gross)

imdb_movies$Released_Year <- as.integer(imdb_movies$Released_Year)
imdb_movies$Runtime <- as.integer(str_remove(imdb_movies$Runtime, " min"))
imdb_movies$Gross <- as.numeric(str_remove_all(imdb_movies$Gross, ","))

colSums(is.na(imdb_movies) | imdb_movies == "")

imdb_movies$Certificate[imdb_movies$Certificate == ""] <- "Not Rated"

imdb_movies <- imdb_movies %>%
  mutate(Certificate_Group = case_when(
    Certificate %in% c("U", "G", "UA", "U/A", "PG", "TV-PG", "Approved", "Passed", "GP") ~ "General Audience",
    Certificate %in% c("A", "R", "PG-13", "TV-14", "TV-MA", "16", "Unrated") ~ "Mature",
    Certificate == "Not Rated" ~ "Not Rated",
    TRUE ~ "Other"
  ))

imdb_movies$Primary_Genre <- str_trim(str_split_fixed(imdb_movies$Genre, ",", 2)[, 1])

imdb_movies$Decade <- floor(imdb_movies$Released_Year / 10) * 10

sum(duplicated(imdb_movies$Series_Title))
imdb_movies <- imdb_movies %>% distinct(Series_Title, Released_Year, .keep_all = TRUE)

imdb_clean <- imdb_movies %>% select(-Poster_Link, -Overview)

boxplot(imdb_clean$Released_Year, main = "Released Year - Outlier Check")

boxplot(imdb_clean$Runtime, main = "Runtime - Outlier Check")

boxplot(imdb_clean$IMDB_Rating, main = "IMDB_Rating - Outlier Check")

boxplot(imdb_clean$Meta_score, main = "Meta_Score - Outlier Check")

boxplot(imdb_clean$No_of_Votes, main = "No_of_Votes - Outlier Check")

boxplot(imdb_clean$Gross, main = "Gross Revenue - Outlier Check")

summary(imdb_clean)

describe(imdb_clean %>% select(IMDB_Rating, Meta_score, Runtime,
                               Gross, No_of_Votes, Released_Year))

mean(imdb_clean$IMDB_Rating, na.rm = TRUE)
sd(imdb_clean$IMDB_Rating, na.rm = TRUE)
median(imdb_clean$Gross, na.rm = TRUE)
sd(imdb_clean$Gross, na.rm = TRUE)

table(imdb_clean$Primary_Genre)
table(imdb_clean$Certificate_Group)
table(imdb_clean$Decade)

prop.table(table(imdb_clean$Certificate_Group)) * 100

ggplot(imdb_clean, aes(x = IMDB_Rating)) +
  geom_histogram(binwidth = 0.1, fill = "#AF719D") +
  labs(title = "Distribution of IMDB Rating")

ggplot(imdb_clean, aes(x = Gross)) +
  geom_histogram(bins= 30, fill = "#AF719D") +
  labs(title = "Distribution of Gross Revenue")

ggplot(imdb_clean, aes(x = Runtime)) +
  geom_histogram(bins= 30, fill = "#AF719D") +
  labs(title = "Distribution of Runtime")

ggplot(imdb_clean, aes(x = No_of_Votes)) +
  geom_histogram(bins= 30, fill = "#AF719D") +
  labs(title = "Distribution of No. of votes")

ggplot(imdb_clean, aes(x = Released_Year)) +
  geom_histogram(binwidth = 5, fill = "#AF719D") +
  labs(title = "Distribution of Released Year")

ggplot(imdb_clean, aes(x = Meta_score)) +
  geom_histogram(bins= 30, fill = "#AF719D") +
  labs(title = "Distribution of Meta Score")

ggplot(imdb_clean, aes(x = Certificate_Group)) +
  geom_bar(fill = "#AF719D") +
  labs(title = "Count by Certificate Group")

ggplot(imdb_clean, aes(x = Primary_Genre)) +
  geom_bar(fill = "#AF719D") +
  coord_flip() +
  labs(title = "Count by Primary Genre")

#relationships
ggplot(imdb_clean, aes(x = IMDB_Rating, y = Gross)) +
  geom_point(alpha = 0.4) +
  labs(title = "IMDB Rating vs Gross Revenue (H1 preview)")

ggplot(imdb_clean, aes(x = Meta_score, y = Gross)) +
  geom_point(alpha = 0.4) +
  labs(title = "Meta Score vs Gross Revenue (H2 preview)")

ggplot(imdb_clean, aes(x = No_of_Votes, y = Gross)) +
  geom_point(alpha = 0.4) +
  labs(title = "No. of Votes vs Gross Revenue (H6 preview)")

# group comparisons
ggplot(imdb_clean, aes(x = Primary_Genre, y = IMDB_Rating)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "IMDB Rating by Genre (H3 preview)")

ggplot(imdb_clean, aes(x = Certificate_Group, y = Gross)) +
  geom_boxplot() +
  labs(title = "Gross Revenue by Certificate Group (H5 preview)")

ggplot(imdb_clean, aes(x = factor(Decade), y = IMDB_Rating)) +
  geom_boxplot() +
  labs(title = "IMDB Rating by Decade (H7 preview)")

num_vars <- imdb_clean %>%
  select(IMDB_Rating, Meta_score, Runtime, Gross, No_of_Votes) %>%
  cor(use = "pairwise.complete.obs")

corrplot(num_vars, method = "number", type = "upper")

#H1
model_h1 <- lm(Gross ~ IMDB_Rating, data = imdb_clean)
summary(model_h1)

#assumption checks h1
plot(model_h1, which = 1)
plot(model_h1, which = 2)
shapiro.test(residuals(model_h1))
plot(model_h1, which = 4)

#log transform
model_h1_log <- lm(log(Gross) ~ IMDB_Rating, data = imdb_clean)
summary(model_h1_log)
shapiro.test(residuals(model_h1_log))
par(mfrow = c(2, 2))
plot(model_h1_log)
par(mfrow = c(1, 1))

#h2
model_h2 <- lm(Gross ~ Meta_score, data = imdb_clean)
summary(model_h2)

#assumption checks h2
plot(model_h2, which = 1)
plot(model_h2, which = 2)
shapiro.test(residuals(model_h2))
plot(model_h2, which = 4)

#log transform
model_h2_log <- lm(log(Gross) ~ Meta_score, data = imdb_clean)
summary(model_h2_log)
shapiro.test(residuals(model_h2_log))
par(mfrow = c(2, 2))
plot(model_h2_log)
par(mfrow = c(1, 1))

#h4
model_h4 <- lm(IMDB_Rating ~ Runtime, data = imdb_clean)
summary(model_h4)

#assumption checks h4
plot(model_h4, which = 1)
plot(model_h4, which = 2)
shapiro.test(residuals(model_h4))
plot(model_h4, which = 4)

#h6
model_h6 <- lm(Gross ~ No_of_Votes, data = imdb_clean)
summary(model_h6)

#assumption checks h6
plot(model_h6, which = 1)
plot(model_h6, which = 2)
shapiro.test(residuals(model_h6))
plot(model_h6, which = 4)

# log transform
model_h6_log <- lm(log(Gross) ~ No_of_Votes, data = imdb_clean)
summary(model_h6_log)
shapiro.test(residuals(model_h6_log))
par(mfrow = c(2, 2))
plot(model_h6_log)
par(mfrow = c(1, 1))

#h3
model_h3 <- aov(IMDB_Rating ~ Primary_Genre, data = imdb_clean)
summary(model_h3)
shapiro.test(residuals(model_h3))
leveneTest(IMDB_Rating ~ Primary_Genre, data = imdb_clean) 
kruskal.test(IMDB_Rating ~ Primary_Genre, data = imdb_clean)

plot(model_h3, which = 1)
plot(model_h3, which = 2)

#h5
model_h5 <- aov(Gross ~ Certificate_Group, data = imdb_clean)
summary(model_h5)
shapiro.test(residuals(model_h5))
leveneTest(Gross ~ Certificate_Group, data = imdb_clean)
kruskal.test(Gross ~ Certificate_Group, data = imdb_clean)

plot(model_h5, which = 1)
plot(model_h5, which = 2)

#h7
imdb_clean$Decade <- as.factor(imdb_clean$Decade) 
model_h7 <- aov(IMDB_Rating ~ Decade, data = imdb_clean)
summary(model_h7)
shapiro.test(residuals(model_h7))
leveneTest(IMDB_Rating ~ Decade, data = imdb_clean)
kruskal.test(IMDB_Rating ~ Decade, data = imdb_clean)
# decades had to be changed to factor bc it was being read as a continuous variable

plot(model_h7, which = 1)
plot(model_h7, which = 2)
