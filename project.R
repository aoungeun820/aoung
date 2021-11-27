#packge
library(stringr)
library(dplyr)
library(rJava)
library(readr)
library(reshape2)
library(tidyverse)
library(multilinguer)
library(tidytext)
library(textclean)
library(ggplot2)
library(ggwordcloud)
library(tidygraph)
library(widyr)
library(ggraph)
library(crayon)
library(showtext)
library(KoNLP)
useSejongDic()

#--------종합리뷰분석----------

#데이터불러오기
reviews<-read_csv("utf_naver_review.csv") 
str(reviews)

#전처리
clean_review<-reviews %>% 
  filter(str_count(review, " ")>=1) %>%
  mutate(id=row_number(),
         review_raw=str_squish(replace_html(review)), #원문 보유
         review=str_replace_all(review, "[^가-힣]", " "),
         review=str_squish(review))

#토큰화
review_token<-clean_review %>% 
  unnest_tokens(input=review,
                output=word,
                token="words",
                drop=F)

#감정어사전 불러오기
dic<-read.csv('SentiWord_Dict.csv') 

#점수부여

review_token<-review_token %>% 
  left_join(dic, by="word") %>% 
  mutate(polarity=ifelse(is.na(polarity),0,polarity))

review_token %>%
  select(word, polarity) %>%
  head(20)

#긍정/중립/부정 단어 분류
review_token<-review_token %>%
  mutate(sentiment=ifelse(polarity>=1, "pos",  #점수가 1점 이상이면 긍정
                          ifelse(polarity<=-1, "neg", "neu")))  #점수가 -1점 이하면 부정, 아니면 중립

#백분율
frequency_word_polarity<-review_token %>%
  count(sentiment)%>%
  mutate(ratio=n/sum(n)*100)  
frequency_word_polarity

#긍정/중립/부정 단어 비율 막대그래프
frequency_word_polarity$dummy <- 0
frequency_word_polarity

ggplot(frequency_word_polarity, aes(x=dummy, y=ratio, fill=sentiment))+
  geom_col()+
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#긍정 단어
pos_top10_sentiment<-review_token %>%
  filter(sentiment == "pos") %>%  
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n=10)
pos_top10_sentiment 

#부정 단어
neg_top10_sentiment<-review_token %>%
  filter(sentiment == "neg") %>%  
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n=10)
neg_top10_sentiment  

#중립 단어 100개->불용어로 분류
neu_top100_sentiment<-review_token %>%
  filter(sentiment == "neu") %>%  
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n=100)
neu_top100_sentiment  

#중립단어 워드 클라우드 그리기
ggplot(neu_top100_sentiment, aes(label=word, size=n, col = n))+ 
  geom_text_wordcloud(seed=1107)+
  scale_radius(limits=c(3, NA),
               range=c(3, 35))+
  scale_color_gradient(low = "#ffdd61", high = "#ffc800")+
  theme_minimal()

#리뷰(원문) 감성 파악

review_score<-review_token %>%
  group_by(id, review) %>%
  summarise(score=sum(polarity)) %>%
  ungroup()

review_score %>% 
  select(score, review)

#긍정/중립/부정 리뷰 분류
review_score<-review_score %>%
  mutate(sentiment=ifelse(score>=1, "pos",  #1점 이상이면 긍정
                          ifelse(score<=-1, "neg", "neu")))  #-1점 이하면 부정, 0점은 중립

#백분율
frequency_polarity_before<-review_score %>%
  count(sentiment) %>%
  mutate(ratio=n/sum(n)*100)  
frequency_polarity_before

#긍정/중립/부정 리뷰 비율 막대그래프
frequency_polarity_before$dummy <- 0
frequency_polarity_before

ggplot(frequency_polarity_before, aes(x=dummy, y=ratio, fill=sentiment))+
  geom_col()+
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#평점 데이터를 통한 감정 점수 가산점 부여

if(reviews$score >= 8){
  review_score$score<-review_score$score+1  #평점 8점 이상이면 +1
} else if(reviews$score <= 2){
  review_score$score<-review_score$score-1  #평점 2점 이하면 -1
} else {}

review_score %>%
  select(score, review)

#평점 데이터 반영 후 백분율
review_score<-review_score %>%
  mutate(sentiment=ifelse(score>=1, "pos",  #1점 이상이면 긍정
                          ifelse(score<=-1, "neg", "neu")))  #-1점 이하면 부정, 0점은 중립

frequency_polarity_after<-review_score %>%
  count(sentiment) %>%
  mutate(ratio=n/sum(n)*100)  
frequency_polarity_after

#긍정/중립/부정 리뷰 비율 막대그래프
frequency_polarity_after$dummy <- 0
frequency_polarity_after

ggplot(frequency_polarity_after, aes(x=dummy, y=ratio, fill=sentiment))+
  geom_col()+
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#불용어목록
neu_dic<-neu_top100_sentiment$word  

#빈도 분석(불용어 제거 전)
frequency<-review_token %>%
  count(word, sort=T) %>%
  filter(str_count(word)>1) #두글자 이상만
top20<-frequency %>%
  head(20) #주요단어(상위20개)
top20

#주요단어 막대그래프 빈도수 정렬
ggplot(top20, aes(x=reorder(word,n),y=n))+ 
  geom_col()+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)+
  
  labs(title = "영화 리뷰 속 주요 단어",
       subtitle="언급 빈도 Top 20",
       x=NULL, y=NULL)+
  
  theme_minimal()+
  theme(text=element_text(size=10),
        plot.title = element_text(size=12, face="bold"),
        plot.subtitle = element_text(size=12))

#불용어 제거 후 Top20
top_word_20<-frequency %>%
  filter(!word %in% neu_dic) %>%  #불용어 제거
  head(20)
top_word_20 

ggplot(top_word_20, aes(x=reorder(word,n),y=n))+ 
  geom_col()+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.3)+
  
  labs(title = "영화 리뷰 속 주요 단어(불용어 제거)",
       subtitle="언급 빈도 Top 20",
       x=NULL, y=NULL)+
  
  theme_minimal()+
  theme(text=element_text(size=12),
        plot.title = element_text(size=14, face="bold"),
        plot.subtitle = element_text(size=13))

#불용어 제거 후 워드클라우드
top100<-frequency %>%
  filter(!word %in% neu_dic) %>%  #불용어 제거
  head(100) 

ggplot(top100, aes(label=word, size=n, col = n))+ 
  geom_text_wordcloud(seed=1107)+
  scale_radius(limits=c(3, NA),
               range=c(1, 20))+
  scale_color_gradient(low = "#ffdd61", high = "#ffc800")+
  theme_minimal()

#네트워크 그래프
#토큰화
review_pos<-clean_review %>% 
  unnest_tokens(input=review,
                output=word,
                token=SimplePos22,
                drop=F)

#명사, 동사, 형용사 추출
review_npvpa<-review_pos %>%
  separate_rows(word, sep="[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%  
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word)>=2) %>%
  arrange(id)

#파이계수
word_cors<-review_npvpa %>%
  add_count(word) %>%
  filter(n>=20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors

set.seed(1107)

#파이계수 그래프용 데이터 

graph_cors<-word_cors %>%
  filter(correlation >= 0.135) %>%   #파이계수가 0.135이상인 단어쌍
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

#파이계수 그래프
ggraph(graph_cors, layout = "fr")+
  geom_edge_link(color="gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = F)+
  scale_edge_width(range=c(1,4))+
  
  geom_node_point(aes(size = centrality,
                      color=group),
                  show.legend=F)+
  scale_size(range=c(5,10))+
  
  geom_node_text(aes(label=name),
                 repel=T,
                 size=5,)+
  theme_graph()

#결합
review_line<-review_npvpa %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))
review_line

#토큰화
bigram_review<-review_line %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n=2)

#단어쌍 분리
bigram_sepreated<-bigram_review %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#단어쌍 빈도
pair_bigram<-bigram_sepreated %>%
  count(word1, word2, sort = T) %>%
  na.omit(0)
pair_bigram  

#바이그램 그래프용 데이터 

graph_bigram <- pair_bigram %>%
  filter(n>=8) %>%             #8번이상 등장한 단어쌍        
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

#바이그램 그래프
ggraph(graph_bigram, layout = "fr")+
  geom_edge_link(color="gray50",
                 alpha=0.5)+
  
  geom_node_point(aes(size=centrality,
                      color=group),
                  show.legend = F)+
  scale_size(range = c(4,8))+
  
  geom_node_text(aes(label=name),
                 repel=T,
                 size=5,)+
  theme_graph()

#--------저평점리뷰분석----------

#데이터불러오기
u_reviews<-read_csv("under_comment.csv") 
str(u_reviews)

#평점이 3점 이하인 리뷰만 추출: 2056개
u_reviews<-u_reviews %>%
  filter(u_reviews$score<=3) 

#전처리
u_clean_review<-u_reviews %>% 
  filter(str_count(review, " ")>=1) %>%
  mutate(id=row_number(), 
         review_raw=str_squish(replace_html(review)), 
         review=str_replace_all(review, "[^가-힣]", " "),
         review=str_squish(review)) 

#토큰화
u_review_token<-u_clean_review %>% 
  unnest_tokens(input=review,
                output=word,
                token="words",
                drop=F)

u_review_token %>%
  select(word, review)

#점수부여
u_review_token<-u_review_token %>% 
  left_join(dic, by="word") %>% 
  mutate(polarity=ifelse(is.na(polarity),0,polarity)) 

u_review_token %>%
  select(word, polarity) %>%
  head(20)

#긍정/중립/부정 단어 분류
u_review_token<-u_review_token %>%
  mutate(sentiment=ifelse(polarity>=1, "pos", 
                          ifelse(polarity<=-1, "neg", "neu")))

#백분율
u_frequency_word_polarity<-u_review_token %>%
  count(sentiment)%>%
  mutate(ratio=n/sum(n)*100)  
u_frequency_word_polarity

#긍정/중립/부정 단어 비율 막대그래프
u_frequency_word_polarity$dummy <- 0
u_frequency_word_polarity

ggplot(u_frequency_word_polarity, aes(x=dummy, y=ratio, fill=sentiment))+
  geom_col()+
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#긍정 단어
u_pos_top10_sentiment<-u_review_token %>%
  filter(sentiment == "pos") %>%  
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n=10)
u_pos_top10_sentiment  

#특정 단어(좋은, 감동, 기대 등)가 포함된 리뷰 출력 함수
find_word <- function(df, x, keyword, n = 6) {
  
  font <- combine_styles(make_style("ivory"),
                         make_style("yellow", bg = TRUE),
                         make_style("bold"))
  
  df %>%
    filter(str_detect({{x}}, keyword)) %>%                  
    head(n) %>%                                             
    mutate(x = paste0("[", row_number(), "] ", {{x}}),      
           x = paste0(str_replace_all(x,
                                      keyword,
                                      font(keyword)))) %>%  
    pull(x) %>%                                             
    cat(sep = "\n\n")                                      
}

#함수 적용
u_reviews %>% find_word(x=review, keyword = "기대") #긍정단어지만 좋은 의미의 맥락에서 사용된 것이 아님

#부정 단어
u_neg_top10_sentiment<-u_review_token %>%
  filter(sentiment == "neg") %>%  
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n=10)
u_neg_top10_sentiment 

#중립 단어
u_neu_top10_sentiment<-u_review_token %>%
  filter(sentiment == "neu") %>% 
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n=10)
u_neu_top10_sentiment  

#리뷰(원문) 감성 파악
u_review_score<-u_review_token %>%
  group_by(id, review) %>%
  summarise(score=sum(polarity)) %>%
  ungroup()

u_review_score %>% 
  select(score, review)

#긍정/중립/부정 리뷰 분류
u_review_score<-u_review_score %>%
  mutate(sentiment=ifelse(score>=1, "pos",  
                          ifelse(score<=-1, "neg", "neu"))) 

#백분율
u_frequency_polarity<-u_review_score %>%
  count(sentiment) %>%
  mutate(ratio=n/sum(n)*100)
u_frequency_polarity

#긍정/중립/부정 리뷰 비율 막대그래프
u_frequency_polarity$dummy <- 0
u_frequency_polarity

ggplot(u_frequency_polarity, aes(x=dummy, y=ratio, fill=sentiment))+
  geom_col()+
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#네트워크 그래프
#토큰화
u_review_pos<-u_clean_review %>% 
  unnest_tokens(input=review,
                output=word,
                token=SimplePos22,
                drop=F)

#명사, 동사, 형용사 추출
u_review_npvpa<-u_review_pos %>%
  separate_rows(word, sep="[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%  
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word)>=2) %>%
  arrange(id)

#파이계수
u_word_cors<-u_review_npvpa %>%
  add_count(word) %>%
  filter(n>=20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
u_word_cors

#파이계수 그래프용 데이터
set.seed(1107)
u_graph_cors<-u_word_cors %>%
  filter(correlation >= 0.16) %>%   #파이계수가 0.16이상인 단어쌍
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

#파이계수 그래프
ggraph(u_graph_cors, layout = "fr")+
  geom_edge_link(color="gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = F)+
  scale_edge_width(range=c(1,4))+
  
  geom_node_point(aes(size = centrality,
                      color=group),
                  show.legend=F)+
  scale_size(range=c(5,10))+
  
  geom_node_text(aes(label=name),
                 repel=T,
                 size=5,)+
  theme_graph() 

#결합
u_review_line<-u_review_npvpa %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))
u_review_line

#토큰화
u_bigram_review<-u_review_line %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n=2)       

#분리
u_bigram_sepreated<-u_bigram_review %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#단어쌍 빈도
u_pair_bigram<-u_bigram_sepreated %>%
  count(word1, word2, sort = T) %>%
  na.omit(0)
u_pair_bigram 

#바이그램 그래프용 데이터
set.seed(1107)
u_graph_bigram <- u_pair_bigram %>%
  filter(n>=6) %>%                       #6번이상 등장한 단어쌍
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

#바이그램 그래프
ggraph(u_graph_bigram, layout = "fr")+
  geom_edge_link(color="gray50",
                 alpha=0.5)+
  
  geom_node_point(aes(size=centrality,
                      color=group),
                  show.legend = F)+
  scale_size(range = c(4,8))+
  
  geom_node_text(aes(label=name),
                 repel=T,
                 size=5,)+
  theme_graph() 

