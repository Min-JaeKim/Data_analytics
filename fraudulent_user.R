library(dplyr)
library(ggplot2)

#데이터 불러오기
day_time <- read.csv("내국인(블록)_일자별시간대별.csv")
sex_age <- read.csv("내국인(집계구)_성별연령대별.csv")
inflow <- read.csv("내국인(집계구)_유입지별.csv")
business <- read.csv("신한카드_내국인_63업종_코드.csv") 
sh_address <- read.csv("신한카드_행정동코드.csv") 
sh_sex_age <- read.csv("신한카드_내국인_성별연령별.csv")
sh_inflow <- read.csv("신한카드_내국인_유입지별.csv")


#필요한 정보만 담긴 데이터 병합
teenagers <- subset(sex_age,AGE_GB.연령대별.=='10대')
write.csv(teenagers, "teenagers.csv")


#dummy data 담긴 데이터(주로 이용할 데이터)
teen_inflow <- read.csv("teenagers_변수이름변경.csv")


#1.일주일이상 카드를 미사용한 아동
teen_inflow$TS_YMD일별 = as.character(teen_inflow$TS_YMD일별)

days <- teen_inflow %>%
  mutate(TS_YMD일별 = as.Date(TS_YMD일별, format = "%Y%m%d")) %>% 
  arrange(ID, TS_YMD일별) %>% 
  group_by(ID) %>% 
  mutate(pre_date = lag(TS_YMD일별)) %>% 
  mutate(range = difftime(TS_YMD일별, pre_date, units = "days")) %>% 
  ungroup()


seven_days_more <- subset(days, sibal$range >= 7)
seven_days_more[c(1,10)] #id랑 range만 

seven_days_more <- seven_days_more[!duplicated(seven_days_more[,1]),] #중복제거 
seven_days_more[c(1,10)]



#1-2.일주일이상 카드를 미사용한 아동 그래프
ggplot(data = seven_days_more, aes(x=ID, y=range))+
  geom_bar(stat = 'identity')+
  labs(x="아동ID",y="미사용 기간", 
       title="<일주일이상 카드를 미사용한 아동>",
       caption = "단위 : 일(day)")+
  geom_text(aes(label=range),
            position = position_dodge(width = 1.8),
            vjust=1.5,
            size=4,color='#FFFFFF')
  


#2. 전체 사용량의 90%가 편의점인 아동의 ID와 확률 

teen_conv <- subset(teen_inflow, SB_UPJONG_CD내국인업종코드=="SB016") 
conv_wrong <- subset((table(teen_conv$ID)/table(teen_inflow$ID)), (table(teen_conv$ID)/table(teen_inflow$ID)) > 0.9) 
conv_wrong

#####소수 둘째짜리에서 반올림했으면 좋겠다


#2-1. 업종별 사용 비율 그래프 (민재)
cw <- data.frame((table(teen_conv$ID)/table(teen_inflow$ID)))
colnames(cw) = c("child_ID","child_conv_percent")

ggplot(cw, aes(child_ID ,child_conv_percent)) +
  geom_bar(stat='identity') + coord_flip()+
  geom_text(aes(label=child_conv_percent),size=3,hjust=1.25,color='#FFFFFF')


#2-2. 업종 별 사용 그래프/전체 항목 그래프(가로)
ggplot(data = teen_inflow, aes(x=SB_UPJONG_CD내국인업종코드))+
  geom_bar(stat='count')+coord_flip()+
  labs(x="업종",y="결제 횟수", title="<전체 업종 별 결제 횟수>")
  #geom_text(aes(label=..count..))
 
  
   
#2-3.상위 5개 업종 별 결제 횟수
top_upjong <- data.frame(sort(table(teen_inflow$SB_UPJONG_CD내국인업종코드),decreasing=TRUE))
top_upjong <- data.frame(top_upjong[c(1:5),])
top_upjong

ggplot(data = top_upjong, aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity')+coord_flip()+
  labs(x="업종",y="결제 횟수", title="<상위 5개 업종 별 결제 횟수>")+
  geom_bar(data = top_upjong[top_upjong$Var1=='SB016',],aes(x=Var1, y=Freq),fill='#5CBED2',stat = 'identity')+
  geom_text(aes(label=Freq),size=5,hjust=1.25,color='#FFFFFF')




#3. 새벽 사용량이 50%이 이상인 아동의 ID와 확률
teen_dawn <- subset(teen_inflow, TM시간대<=5)
dawn_wrong <- subset((table(teen_dawn$ID)/table(teen_inflow$ID)), (table(teen_dawn$ID)/table(teen_inflow$ID)) > 0.5) 
dawn_wrong

#####소수 둘째짜리에서 반올림했으면 좋겠다


#3-1. 시간 별 사용 비율 그래프(민재)
teen_dawn
td <- data.frame((table(teen_dawn$ID)/table(teen_inflow$ID)))

head(td)
colnames(td) = c("child_ID","child_dawn_percent")

ggplot(td, aes(child_ID ,child_dawn_percent)) +
  geom_bar(stat='identity') + coord_flip() 

#3-2. 시간 별 사용자 명수
by_time <- data.frame(table(teen_inflow$TM시간대))
by_time

ggplot(data = by_time, aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity')+coord_flip()+
  labs(x="시간 (단위:시)",y="결제 횟수", title="<시간별 이용자 수>")+
  geom_text(aes(label=Freq),size=5,hjust=1.25,color='#FFFFFF')







