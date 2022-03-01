
source("./src/00_libs.R", encoding = "UTF-8")

library(dplyr)
train = read.csv("./data/raw/airline_dataset/train.csv")
test = read.csv("./data/raw/airline_dataset/test.csv")
submission = read.csv("./data/raw/airline_dataset/sample_submission.csv")

# id : 샘플 아이디
# Gender : 성별
# Customer Type : Disloyal 또는 Loyal 고객
# Age : 나이
# Type of Travel : Business 또는 Personal Travel
# Class : 등급
# Flight Distance : 비행 거리
# Seat comfort : 좌석 만족도
# Departure/Arrival time convenient : 출발/도착 시간 편의성 만족도
# Food and drink : 식음료 만족도
# Gate location : 게이트 위치 만족도
# Inflight wifi service : 기내 와이파이 서비스 만족도
# Inflight entertainment : 기내 엔터테인먼트 만족도
# Online support : 온라인 지원 만족도
# Ease of Online booking : 온라인 예매 편리성 만족도
# On-board service : 탑승 서비스 만족도
# Leg room service : Leg room 서비스 만족도
# Baggage handling : 수하물 처리 만족도
# Checkin service : 체크인 서비스 만족도
# Cleanliness : 청결도 만족도
# Online boarding : 온라인보딩 만족도
# Departure Delay in Minutes : 출발 지연 시간
# Arrival Delay in Minutes : 도착 지연 시간
# target : 만족 여부

train %>% head
test %>% head
submission %>% head

train %>% dim
test %>% dim


