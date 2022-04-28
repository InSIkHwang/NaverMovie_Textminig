##라이브러리 설치 및 부착

install.packages("stringr")
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("curl")
install.packages("multilinguer")
install.packages("readr")
install_jdk()
install.packages('rJava', type = 'binary');library(rJava);.jinit();rstudioapi::restartSession()
install.packages(c("stringr","hash","tau","Sejong","RSQLite","devtools"),
                 type = "binary")
install.packages("remotes")
install.packages("ggwordcloud")
install.packages("patchwork")
remotes::install_github("haven-jeon/KoNLP", upgrade = "never", INSTALL_opts =
                          c("--no-multiarch"), force = TRUE)

library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(showtext)
library(curl)
library(ggwordcloud)
library(patchwork)
library(multilinguer)
library(KoNLP)
library(showtext)
library(readr)
useNIADic()


##데이터 불러오기
movie <- read_csv("movie.csv")

#데이터 구조 살펴보기
head(movie)
str(movie)
#공감순 정렬
movie %>% arrange(desc(sympathy))


#전처리
#한글 제외 문자 삭제
movie <- movie %>%
    mutate(review = str_replace_all(review, "[^가-힣]", " "), review = str_squish(review))
movie

#review열 필요 없는 단어 삭제
movie <- movie %>%
  mutate(review = str_replace_all(review, "관람객", "")) %>%
  mutate(review = str_replace_all(review, "스포일러가 포함된 감상평입니다 감상평 보기", "")
         , review = str_squish(review))

movie

#토큰화(단어)
movie_review_word <- movie %>%
  unnest_tokens(input = review, output = word, token = "words", drop = F)
movie_review_word

#토큰화(명사)
movie_review_ext <- movie %>%
  unnest_tokens(input = review, output = word, token = extractNoun, drop = F)
movie_review_ext


#빈도(2글자 이상)
word_freq <- movie_review_word %>%
  filter(str_count(word)>1) %>% count(word, sort = T)
word_freq

ext_freq <- movie_review_ext %>%
  filter(str_count(word)>1) %>% count(word, sort = T)
ext_freq

#상위 20개 항목 뽑기
word_top20 <- word_freq %>% head(20)
word_top20

ext_top20 <- ext_freq %>% head(20)
ext_top20


#빈도 그래프
gg_word <- ggplot(word_top20, aes(x = reorder(word, n), y = n)) + geom_col() +
  coord_flip() + geom_text(aes(label = n), hjust = -0.3) +
  labs(title = "단어 빈도 수(단어)", x="단어", y="빈도") +
  theme(title = element_text(size = 10))

gg_ext <- ggplot(ext_top20, aes(x = reorder(word, n), y = n)) + geom_col() +
  coord_flip() + geom_text(aes(label = n), hjust = -0.3) +
  labs(title = "단어 빈도 수(명사)", x="단어", y="빈도") +
  theme(title = element_text(size = 10))

#기준에 따른 빈도 그래프 차이(patchwork라이브러리 사용)
gg_word + gg_ext

##TF-IDF 별점별
ext_freq <- movie_review_ext %>%
  filter(str_count(word)>1) %>% count(stars, word, sort = T)

ext_freq


movie_freq <- ext_freq %>% bind_tf_idf(term = word, document = stars, n=n) %>% 
  arrange(-tf_idf)
movie_freq

#별점별 필터링
movie_freq %>% filter(stars =="10")
movie_freq %>% filter(stars =="5")
movie_freq %>% filter(stars =="1")

#막대 그래프 비교
top10 <- movie_freq %>% group_by(stars) %>% slice_max(tf_idf, n=10, with_ties = F)
#별점 1, 5, 10점 비교
top10$stars <- factor(top10$stars, levels = c("1","5","10"))
top10 <- na.omit(top10) #결측치 제거
ggplot(top10, aes(x=reorder_within(word, tf_idf, stars), 
                  y=tf_idf, fill=stars)) +geom_col(show.legend = F) +
                  coord_flip() + facet_wrap(~stars, scales = "free",ncol = 2)+
                  scale_x_reordered() + labs(x=NULL)


##감정사전을 적용하여, 텍스트의 감정 경향을 분석하기
#감정사전 불러오기
dic <- read_csv("knu_sentiment_lexicon.csv")

#점수 부여
movie_review_word_em <- movie_review_word %>% left_join(dic, by="word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
movie_review_word_em %>% select(word, polarity)

#감정 분류
movie_review_word_em <- movie_review_word_em %>% mutate(sentiment = ifelse(
  polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
movie_review_word_em %>% count(sentiment)

#상위 단어 추출
top10_sen <- movie_review_word_em %>% filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
top10_sen


#막대 그래프
ggplot(top10_sen, aes(x=reorder(word, n), y=n, fill=sentiment)) +
  geom_col() + coord_flip() + geom_text(aes(label=n), hjust=-0.3) +
  facet_wrap(~sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05,0.15))) + 
  labs(x=NULL)

#리뷰별 감정 점수
score_review <- movie_review_word_em %>% group_by(id, review) %>%
  summarise(score=sum(polarity)) %>% ungroup()

#내림차순
score_review %>% select(score,review) %>% arrange(desc(score))

#감정 경향 살피기
score_review <- score_review %>% mutate(sentiment=ifelse(
  score>=1,"pos",ifelse(score<=-1,"neg","neu")))

freq_score <- score_review %>% count(sentiment) %>% mutate(ratio=n/sum(n)*100)
freq_score

#막대 그래프
ggplot(freq_score, aes(x=sentiment, y=n, fill=sentiment)) +
  geom_col() + geom_text(aes(label=n),vjust=-0.3) + scale_x_discrete(
    limits=c("pos","neu","neg"))


##감정사전 수정하여 적용하고, 수정전과 비교분석하기
#감정 사전 추가
newword <- tibble(word = c("재밌게", "재밌음", "좋았습니다","지루하고", "아쉬운"),
                  polarity = c(2,2,2,-2,-1))
new_dic <- bind_rows(dic,newword)
tail(new_dic)

#수정된 사전 감정 점수 부여
new_movie_review_word <- movie_review_word %>% left_join(new_dic, by="word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

new_movie_review_word <- new_movie_review_word %>% mutate(sentiment = ifelse(
  polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))

movie_review_word_em %>% count(sentiment) #수정 전
new_movie_review_word %>% count(sentiment) #수정 후


#리뷰별 감정 점수
new_score_review <- new_movie_review_word %>% group_by(id, review) %>%
  summarise(score=sum(polarity)) %>% ungroup()

#내림차순
score_review %>% select(score,review) %>% arrange(desc(score)) #수정 전
new_score_review %>% select(score,review) %>% arrange(desc(score)) #수정 후

#감정 경향 살피기
new_score_review <- new_score_review %>% mutate(sentiment=ifelse(
  score>=1,"pos",ifelse(score<=-1,"neg","neu")))

new_freq_score <- new_score_review %>%
  count(sentiment) %>%
  mutate(ratio=n/sum(n)*100)

freq_score #수정 전
new_freq_score #수정 후


#수정 전 막대 그래프
ggplot(freq_score, aes(x=sentiment, y=n, fill=sentiment)) +
  geom_col() + geom_text(aes(label=n),vjust=-0.3) + scale_x_discrete(
    limits=c("pos","neu","neg"))

#수정 후 막대 그래프
ggplot(new_freq_score, aes(x=sentiment, y=n, fill=sentiment)) +
  geom_col() + geom_text(aes(label=n),vjust=-0.3) + scale_x_discrete(
    limits=c("pos","neu","neg"))
