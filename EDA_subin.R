getwd()
setwd("c:/data/주차")
library(data.table)
library(GGally)
library(dplyr)

train <- data.frame(fread('train.csv', encoding = 'UTF-8'))
age <-  data.frame(fread('age_gender_info.csv', encoding = 'UTF-8'))

head(train)
head(age)

str(train)
dim(distinct(train))

############## 1. 데이터 중복 행 ##############
train <- distinct(train)
dim(train)
str(train)

############## 2. 데이터 정제 ##############
attach(train)

#### 총세대수(int) ####
summary(총세대수)
hist(총세대수)
boxplot(총세대수)
# 이상치 존재 가능성 有
# 지역, 보증금, 임대료 등 다른 변수들과 비교해야 할 필요 존재

#### 임대건물구분(chr) ####
table(임대건물구분)
# 상가    285
# 아파트  2347

#### 지역(chr) ####
table(지역)
# 강원도  경기도  경상남도  경상북도  광주광역시  대구광역시  대전광역시  부산광역시  서울특별시 
# 165     609     314       118       139         99          203         232         63 
# 세종특별자치시  울산광역시  전라남도  전라북도  제주특별자치도  충청남도  충청북도 
# 47              32          149       107       69              125       161 

ggplot(train, aes(x=지역, y=..count.., fill=지역))+
  geom_bar()+geom_text(stat="count",aes(label=..count..),
                       position=position_dodge(width=1.8),
                       vjust=-0.5)+theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(face="bold", size=20, vjust=2, hjust=0.5))+
  ggtitle("지역 빈도")
# 경기도 > 경상남도 > 부산 > 대전 > ...

#### 공급유형(chr) ####
table(공급유형)
# 공공분양  공공임대(10년)  공공임대(50년)  공공임대(5년) 공공임대(분납)  
# 7         203             31              3             12             
# 국민임대  영구임대  임대상가  장기전세  행복주택 
# 1730      149       285       9         203
ggplot(train, aes(x=공급유형, y=..count.., fill=공급유형))+
  geom_bar()+geom_text(stat="count",aes(label=..count..),
                       position=position_dodge(width=1.8),
                       vjust=-0.5)+theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(face="bold", size=20, vjust=2, hjust=0.5))+
  ggtitle("공급유형 빈도")
# 국민임대 >>>>>>>>>>> 공공임대(10년), 임대상가, 행복주택 > 영구임대 >>> 나머지

#### 전용면적(num) ####
summary(전용면적)
hist(전용면적)
boxplot(전용면적)
# 이상치가 존재 가능성有
# 지역, 보증금, 임대료 등 다른 변수들과 비교해야 할 필요 존재 

#### 전용면적별세대수(int) ####
summary(전용면적별세대수)
hist(전용면적별세대수)
boxplot(전용면적별세대수)
# 이상치가 존재 가능성有
# 지역, 보증금, 임대료 등 다른 변수들과 비교해야 할 필요 존재 

#### 공가수(num) ####
summary(공가수)
hist(공가수)
boxplot(공가수)
# 이상치가 존재 가능성有
# 지역, 보증금, 임대료 등 다른 변수들과 비교해야 할 필요 존재 

#### 자격유형(chr) ####
table(자격유형)
# A       B    C    D    E    F    G    H    I    J    K    L    M    N    O 
# 1775   18   92  292   37    3    9  154   49  105   33   33    2   29    1
ggplot(train, aes(x=자격유형, y=..count.., fill=자격유형))+
  geom_bar()+geom_text(stat="count",aes(label=..count..),
                       position=position_dodge(width=1.8),
                       vjust=-0.5)+theme_bw()+
  theme(axis.text.x=element_text(hjust=1),
        plot.title=element_text(face="bold", size=20, vjust=2, hjust=0.5))+
  ggtitle("자격유형 빈도")

#### 임대보증금(chr) ####
임대보증금 <- as.numeric(임대보증금)
colSums(is.na(train))
summary(임대보증금)

#### 임대료(chr) ####


#### 도보 10분거리 내 지하철역 수(환승노선 수 반영) (num) ####


#### 도보 10분거리 내 버스정류장 수 (num) ####


#### 단지내주차면수 (num) ####


#### 등록차량수 (num) ####








