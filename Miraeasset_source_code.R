#load library
library(data.table)
library(tidyverse)  
library(caret)
library(randomForest)
library(magrittr)
library(xts)
library(quantmod)
library(pROC)
library(nnet)
library(MLmetrics)
library(e1071)
library(TTR)
library(dispRity)

# Ⅰ. Data Collection
## 1. Technical Indicator

###kopsi 지수 데이터를 불러옵니다

kospi <- fread('KOSPI.csv') 

###사용할 변수만 남겨줍니다

kospi2 <- kospi %>% select(날짜, Y, 지수종가, 지수시가, 지수고가, 지수저가)
x <- as.data.frame(kospi2[,3]) 
x <- x[,1]

### 5일후 종가와 비교하여 상승 폭을 계산합니다. 이의 절댓값이 1.5보다 작으면 보합으로 간주하여 재범주화합니다.

y <- c(x[6:4449],rep(NA,5))
kospi2 %<>% mutate(change = y-x)
kospi3 <- kospi2 %>% mutate(changerate = change / 지수종가 * 100) %>% 
  mutate(Y= ifelse(changerate>1.5, '상승',
                   ifelse(changerate<=1.5&changerate>0,'상승보합',
                          ifelse(changerate<=0&changerate>-1.5,'하락보합','하락'))))

#상승/하락 별 최대 변화량과 최소 변화량이 매우 많이 차이나는 것을 알 수 있습니다.

kospi3 %>% filter(Y %in% c('상승','상승보합')) %>% select(날짜,changerate) %>% arrange(changerate) %>%top_n(5)
kospi3 %>% filter(Y %in% c('상승','상승보합')) %>% select(날짜,changerate) %>% arrange(changerate) %>%top_n(-5)
kospi3 %>% filter(Y %in% c('하락','하락보합')) %>% select(날짜,changerate) %>% arrange(changerate) %>%top_n(-5)
kospi3 %>% filter(Y %in% c('하락','하락보합')) %>% select(날짜,changerate) %>% arrange(changerate) %>%top_n(5)

### Table을 보면 각 class의 비율이 유사하다는 것을 알 수 있습니다.

table(kospi3$Y)

### 기술적 변수를 계산하기 위하여 고가,저가,종가를 모은 데이터를 따로 만듭니다.

kospiHLC <- kospi %>% select(High=지수고가,Low=지수저가,Close=지수종가)

### Stochastic Fast %K, Fast %D, slow %D
#ROC (rate of change)
#moment (momentum)
#cci (Commodity channel index)
#disparity : 이격도
#pctB : 볼린저 밴드의 표준편차

stc <- stoch(kospiHLC,nFastK = 5, nFastD = 3, nSlowD = 3)
roc <- kospiHLC %>% select(Close) %>% ROC(1) %>% as.data.frame()
moment <- kospiHLC %>% select(Close) %>% momentum() %>% as.data.frame()
cci <- CCI(kospiHLC) %>% as.data.frame()
colnames(roc) <- 'roc'
colnames(moment) <- 'momentum'
colnames(cci) <- 'cci'

###사용할 변수를 train set과 test set으로 나눠서 저장합니다.

kospidata <- kospi3 %>% cbind(stc,roc,moment,cci) %>%
  cbind(BBands(kospi[,c('지수시가','지수저가','지수종가')],n=5)) %>%
  mutate(disparity=지수종가/mavg*100,Y=as.factor(Y)) %>%
  filter(날짜>=2011&날짜<2017) %>% 
  select(날짜,Y,fastK:cci,pctB,disparity)

kospitest <- kospi2 %>% cbind(stc,roc,moment,cci) %>%
  cbind(BBands(kospi[,c('지수시가','지수저가','지수종가')],n=5)) %>%
  mutate(disparity=지수종가/mavg*100,Y=as.factor(Y)) %>%
  filter(날짜>=2017) %>% 
  select(날짜,Y,fastK:cci,pctB,disparity)


## 2. News Text

### 뉴스 링크와 주소 가져오기

library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=japan&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
japan_news <- tibble(date=news_date,content=news_content)

### 뉴스 링크와 주소 가져오기(중국)
library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=china&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
china_news <- tibble(date=news_date,content=news_content)

### 뉴스 링크와 주소 가져오기(북한)
library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=north_korea&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
northkorea_news <- tibble(date=news_date,content=news_content)

### 뉴스 링크와 주소 가져오기(미국)
library(rvest)
library(dplyr) ; library(magrittr)
url <- 'http://www.koreaherald.com/search/index.php?q=us&sort=1&mode=list&np='
page <- 1:3000
news_url <- c(); news_date <- c()
for(i in page){
  herald_url <- paste0(url,i)
  html <- read_html(herald_url)
  temp1 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.contentsArea')%>%
    html_nodes('a')%>%
    html_attr('href')%>%unique()
  news_url <- c(news_url,temp1)
  temp2 <- html_nodes(html,css='.wrapDiv')%>%
    html_nodes(css='.contentsWrap')%>%
    html_nodes(css='.contents.sub')%>%
    html_nodes(css='.overHidden')%>%
    html_nodes(css='.subContents.w776')%>%
    html_nodes(css= '.listDiv.l1.mt58.bb1.pb17')%>%
    html_nodes(css='.mb24.pl55.w721')%>%
    html_nodes(css='.dateDiv')%>%
    html_nodes('p')%>%
    html_text()
  news_date <- c(news_date,temp2)
}

### 뉴스 내용 가져오기
news_content <- c()
for(i in 1:length(news_url)){
  html2 <- read_html(paste0('http://www.koreaherald.com',news_url[i]))
  temp3 <- html_text(html_nodes(html2,'#articleText.content_view'))
  news_content <- c(news_content,temp3)
}

### 뉴스 날짜, 내용 합치기
us_news <- tibble(date=news_date,content=news_content)


# Ⅱ. Data Preprocessing

## Text mining, Sentimental Analysis

### text처리

library(tidytext)
library(tidyr)

### 날짜 정리(일본)

t1 <- japan_news%>%unnest_tokens(word,content)
t1%<>%anti_join(stop_words) # 관사 제거
t1%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t1%>%count(word, sort=T)

### 감성분석
emotion <- t1 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_japan<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

### 날짜 정리(중국)
t2 <- china_news%>%unnest_tokens(word,content)
t2%<>%anti_join(stop_words) # 관사 제거
t2%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t2%>%count(word, sort=T)

### 감성분석
emotion <- t2 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_china<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

### 날짜 정리(북한)
t3 <- northkorea_news%>%unnest_tokens(word,content)
t3%<>%anti_join(stop_words) # 관사 제거
t3%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t3%>%count(word, sort=T)

### 감성분석
emotion <- t3 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_northkorea<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

### 날짜 정리(미국)
t4 <- us_news%>%unnest_tokens(word,content)
t4%<>%anti_join(stop_words) # 관사 제거
t4%<>%mutate(word = gsub("'s", "", word)) # 's 없애기
c<- t4%>%count(word, sort=T)

### 감성분석
emotion <- t4 %>% group_by(date)%>%
  left_join(get_sentiments("bing")) %>% 
  count(word, sentiment)
emotion%<>%na.omit
emotion%<>%mutate(posneg = ifelse(sentiment=='positive', 1, -1))

emotion%<>%separate(date,into=c('month','day','year'),sep=' ')
emotion%<>%na.omit
emotion%<>%mutate(day = gsub(',', '',day))
emotion%<>%mutate(month = recode(month, 'Jan'=1,'Feb'=2,'Mar'=3,'Apr'=4,'May'=5,'Jun'=6,'Jul'=7,'Aug'=8,'Sep'=9,'Oct'=10,'Nov'=11,'Dec'=12))
emotion%<>%unite(col=date,year,month,day,sep='-')
emotion%<>%arrange(date)

emotion_us<- emotion%>%
  group_by(date)%>%
  summarise(sentiment_score=sum(posneg*n))

### 이후 data.table::fwrite(data,'japan_news.csv') 와 같은 방식으로 감성분석 지수를 저장하였습니다.

#################################################################################

###사용할 감성점수 csv 파일을 불러옵니다

emotion_jp <- fread('emotion_japan.csv')
emotion_nk <- fread('emotion_nk.csv')
emotion_usa <- fread('emotion_usa.csv')
emotion_ch <- fread('emotion_china.csv')

###4개의 emotion 파일을 하나로 합칩니다

emotion <- emotion_jp %>%
  mutate(jp_score = sentiment_score) %>%
  select(-sentiment_score) %>% 
  full_join(emotion_nk) %>% 
  mutate(nk_score = sentiment_score) %>% 
  select(-sentiment_score) %>% 
  full_join(emotion_usa) %>%
  mutate(na_score = sentiment_score) %>%
  select(-sentiment_score) %>% 
  full_join(emotion_ch) %>% 
  mutate(ch_score = sentiment_score) %>% 
  select(-sentiment_score)


emotion %<>% mutate(date=as.character(as.Date(date)))

###감성점수의 이전 5일간 평균을 만드는 함수를 통해, emotion 데이터를 전처리해줍니다.

avgcal <- function(data){
  result <- matrix(NA,nrow(data),ncol(data))
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      if(i-4<=0){
        result[i,j] <- NA}
      else{
        result[i,j] <- mean(data[(i-4):i,j],na.rm=T)
      } 
    }
  }
  name <- colnames(data)
  colnames(result) <- paste(name,'avg',sep='')
  cbind(data,result)
}


emotion <- cbind(emotion[1],avgcal(emotion[-1]))
emotion %<>% select(date,jp_scoreavg:ch_scoreavg)


###원래 데이터에 감성점수를 합친 kospiemotion, kospiemotiontest 데이터를 만듭니다.


kospiemotion <- kospidata %>% left_join(emotion, by=c('날짜'='date')) %>% select(-날짜)
kospiemotiontest <- kospitest %>% left_join(emotion,by=c('날짜'='date')) %>% select(-날짜)


###결측치 제거 = 각각 나라별 감성지수의 평균을 이용하여 결측치를 제거해줍니다.

attach(kospiemotion)
kospiemotion %<>% replace_na(list(jp_scoreavg=mean(jp_scoreavg,na.rm=T),
                                  nk_scoreavg=mean(nk_scoreavg,na.rm =T),
                                  na_scoreavg=mean(na_scoreavg,na.rm=T),
                                  ch_scoreavg=mean(ch_scoreavg,na.rm=T)))
detach(kospiemotion)
attach(kospiemotiontest)
kospiemotiontest %<>% replace_na(list(jp_scoreavg=mean(jp_scoreavg,na.rm=T),
                                      nk_scoreavg=mean(nk_scoreavg,na.rm =T),
                                      na_scoreavg=mean(na_scoreavg,na.rm=T),
                                      ch_scoreavg=mean(ch_scoreavg,na.rm=T)))
detach(kospiemotiontest)

###날짜는 앞으로 분석에 사용하지 않을 것이기 때문에 제거합니다.

kospidata %<>% select(-날짜)
kospitest %<>% select(-날짜)

#############################################################################

###데이터 column이 numeric인 column들을 scaling 해주는 함수

datascaling <- function(data){
  for( i in 1:ncol(data)){
    if(class(data[,i])=='numeric'){
      data[,i] <- scale(data[,i])}
    
  }
  return(data)
}

#모든 데이터를 scaling해줍니다.

kospidata <- datascaling(kospidata)
kospitest <- datascaling(kospitest)
kospiemotion <- datascaling(kospiemotion)
kospiemotiontest <- datascaling(kospiemotiontest)

###이미지데이터 준비

#전체 데이터셋에 대한 기술적 분석 지표를 넣은 데이터를 만듭니다

kospiplot <- kospi2 %>% cbind(stc,roc,moment,cci) %>%
  cbind(BBands(kospi[,c('지수시가','지수저가','지수종가')],n=5)) %>%
  mutate(disparity=지수종가/mavg*100,Y=as.factor(Y)) %>% 
  datascaling()

#plotting 함수는 ggplot 패키지를 통해 k번째 날부터 이전 5일간 기술적분석 지표의 움직임을 담은 그래프를 출력하는 함수입니다.

plotting <- function(k){
  ggplot(kospiplot[(k-4):k,],aes(날짜,group=1)) + 
    geom_line(aes(y=cci),col='green', size=1.5) +
    geom_line(aes(y=roc),col='pink',size=1.5) +
    geom_line(aes(y=fastD),col='red',size=1.5) +
    geom_line(aes(y=fastK),col='yellow',size=1.5) +
    geom_line(aes(y=slowD),col='white',size=1.5) +
    geom_line(aes(y=disparity),col='blue',size=1.5) +
    geom_line(aes(y=momentum),col='purple',size=1.5) +
    geom_line(aes(y=pctB),col='deepskyblue',size=1.5) +
    theme(panel.background = element_rect(fill='black', color='black'),panel.border=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
    ggsave(paste('image',k,".png",sep=''))
}

#1977 행은 2011년 데이터가 시작되는 행으로, 이 행부터 시작해서 이미지를 출력해냈습니다

for(k in 1977:nrow(kospi)){
  plotting(k)
}

``

# Ⅲ. Feature Selection & Validation

logdata <- kospidata
logemotion <- kospiemotion
control <- trainControl(method = 'cv', number = 5,summaryFunction = multiClassSummary, classProbs = TRUE, search = 'random')
test_y <- kospiemotiontest$Y

## Multinomial logistic regression model

set.seed(2019)
logmodel <- train(Y~., data=logdata, trControl= control, method = 'multinom', metric = 'Mean_F1', tuneLength = 10, verbose=F)
summary(logmodel)
y_pred <- predict(logmodel,kospitest[-1], type='raw')
y_pred_log <- as.factor(ifelse(y_pred %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_log, test_y)

##  Randomforest model

grid1 = expand.grid(.mtry = 2:8)
set.seed(2019)
rfmodel <- train(Y~., data=logdata, trControl= control, method = 'rf', metric = 'Mean_F1', tuneGrid = grid1, verbose=F)
rfmodel
imp<- varImp(rfmodel,scale = F)
plot(imp)
y_pred <- predict(rfmodel,kospitest[-1], type='raw')
y_pred_rf <- as.factor(ifelse(y_pred %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_rf, test_y)

##  Support vector machine model

set.seed(2019)
svmmodel <- train(Y~., data=logdata, trControl= trainControl('cv',5,summaryFunction = multiClassSummary,classProbs = T, sampling = 'down'), method = 'svmRadial', metric = 'Mean_F1', verbose=F, tuneGrid = expand.grid(.C=0.125,.sigma=0.125))
svmmodel
y_pred <- predict(svmmodel,kospitest[-1], type='raw')
y_pred_svm <- as.factor(ifelse(y_pred %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_svm, test_y)

######################## with emotion ########################

logemotion <- kospiemotion
control <- trainControl(method = 'cv', number = 5,summaryFunction = multiClassSummary, classProbs = TRUE, search = 'random')

##  Multinomial logistic regression model with emotion

set.seed(2019)
logmodel2 <- train(Y~., data=logemotion, trControl= control, method = 'multinom', metric = 'Mean_F1', tuneLength = 10, verbose=F)
summary(logmodel2)
y_pred2 <- predict(logmodel2,kospiemotiontest[-1], type='raw')
y_pred_log2 <- as.factor(ifelse(y_pred2 %in% c('상승','상승보합'), '상승','하락'))

confusionMatrix(y_pred_log2, test_y)

##  Randomforest model with emotion

grid1 = expand.grid(.mtry = 2:12)
set.seed(2019)
rfmodel2 <- train(Y~., data=logemotion, trControl= control, method = 'rf', metric = 'Mean_F1', tuneGrid = grid1, verbose=F)
rfmodel2
imp<- varImp(rfmodel2,scale = F)
plot(imp)
y_pred2 <- predict(rfmodel2,kospiemotiontest[-1], type='raw')
y_pred_rf2 <- as.factor(ifelse(y_pred2 %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_rf2, test_y)

##  Support vector mahcine model with emotion

grid = expand.grid(.sigma = 1/2, .C = 2^7)
set.seed(2019)
svmmodel2 <- train(Y~., data=logemotion, trControl= control, method = 'svmRadial', metric = 'Mean_F1', tuneLength = 3, verbose=F, tuneGrid = grid)
svmmodel2
y_pred2 <- predict(svmmodel2,kospiemotiontest[-1], type='raw')
y_pred_svm2 <- as.factor(ifelse(y_pred2 %in% c('상승','상승보합'), '상승','하락'))
confusionMatrix(y_pred_svm2, test_y)

# Ⅳ. Best Model Selection

Accuracy(y_pred_log, test_y); F1_Score(y_pred_log, test_y) # accuracy :0.5503 , f1-score : 0.6812
Accuracy(y_pred_rf, test_y); F1_Score(y_pred_rf, test_y) # accuracy : 0.5318, f1-score : 0.625
Accuracy(y_pred_svm, test_y); F1_Score(y_pred_svm, test_y) # accuracy : 0.5503, f1-score : 0.7076
Accuracy(y_pred_log2, test_y); F1_Score(y_pred_log2, test_y) # accuracy : 0.5646, f1-score : 0.6778
Accuracy(y_pred_rf2, test_y); F1_Score(y_pred_rf2, test_y) # accuracy : 0.5441, f1-score : 0.6287
Accuracy(y_pred_svm2, test_y); F1_Score(y_pred_svm2, test_y) # accuracy : 0.54, f1-score : 0.6753

#이와 비교할 CNN, CNN-MLP 모델은 파이썬 코드에 첨부합니다.