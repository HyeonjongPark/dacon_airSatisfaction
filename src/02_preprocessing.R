#rm(list = ls())
source("./src/01_dataLoad.R", encoding = "UTF-8")

sub_ver = "sub1"

test$target = NA

train = cbind(division = "train" ,train)
test = cbind(division = "test" ,test)

data = rbind(train, test)

data %>% head
data$Gender %>% unique
data$Type.of.Travel %>% unique

data$Customer.Type %>% unique
data$Class  %>% unique

data1 = data %>%
  mutate(Class = dplyr::recode(Class , 'Eco' = 1, 'Eco Plus' = 2, 'Business' = 3)) %>%
  mutate(Customer.Type = dplyr::recode(Customer.Type , 'disloyal Customer' = 1, 'Loyal Customer' = 2))


data1 %>% str

dmy <- dummyVars(~., data = data1[,3:ncol(data1)])
data2 <- data.frame(predict(dmy, newdata = data1[,3:ncol(data1)]))

data1 = cbind(data1[,1:2], data2)
data1 %>% str

colSums(is.na(data1)) # 결측치 미존재

data1 %>% summary

data1 %>% arrange(desc(Flight.Distance)) %>% filter(!is.na(target))
table(data1$target)
# 이상치 검출
# data1 %>% filter(Departure.Delay.in.Minutes == max(Departure.Delay.in.Minutes)) # 
# data1$Departure.Delay.in.Minutes[data1$Departure.Delay.in.Minutes == 2207] = 2007
# data1 %>% head


# 파생변수 추가

# id : 샘플 아이디
# Gender : 성별
# Customer Type : Disloyal 또는 Loyal 고객
# Age : 나이
# Type of Travel : Business 또는 Personal Travel
# Class : 등급
# Flight Distance : 비행 거리

# 탑승 전
# Departure/Arrival time convenient : 출발/도착 시간 편의성 만족도
# Gate location : 게이트 위치 만족도
# Ease of Online booking : 온라인 예매 편리성 만족도
# Baggage handling : 수하물 처리 만족도
# Checkin service : 체크인 서비스 만족도
# Online boarding : 온라인보딩 만족도
# On-board service : 탑승 서비스 만족도

# 탑승 후 - 몸
# Seat comfort : 좌석 만족도
# Food and drink : 식음료 만족도

# 탑승 후 - 정신
# Inflight wifi service : 기내 와이파이 서비스 만족도
# Inflight entertainment : 기내 엔터테인먼트 만족도
# Online support : 온라인 지원 만족도
# Leg room service : Leg room 서비스 만족도
# Cleanliness : 청결도 만족도


# Departure Delay in Minutes : 출발 지연 시간
# Arrival Delay in Minutes : 도착 지연 시간
# target : 만족 여부

# data1 = data1 %>% mutate(total.satisfaction = (Seat.comfort + Food.and.drink) *
#                            (Inflight.entertainment + Inflight.wifi.service + Online.support + Leg.room.service + Cleanliness) *
#                            (Departure.Arrival.time.convenient + Gate.location + Ease.of.Online.booking + On.board.service +
#                             Baggage.handling + Checkin.service + Online.boarding))



# 탑승 전 - 몸
# Departure/Arrival time convenient : 출발/도착 시간 편의성 만족도
# Gate location : 게이트 위치 만족도
# Baggage handling : 수하물 처리 만족도
# Checkin service : 체크인 서비스 만족도
# On-board service : 탑승 서비스 만족도

# 탑승 전 - 정신
# Ease of Online booking : 온라인 예매 편리성 만족도
# Online boarding : 온라인보딩 만족도

# 탑승 후 - 몸
# Seat comfort : 좌석 만족도
# Food and drink : 식음료 만족도
# Leg room service : Leg room 서비스 만족도
# Cleanliness : 청결도 만족도

# 탑승 후 - 정신
# Inflight wifi service : 기내 와이파이 서비스 만족도
# Inflight entertainment : 기내 엔터테인먼트 만족도
# Online support : 온라인 지원 만족도


# Departure Delay in Minutes : 출발 지연 시간
# Arrival Delay in Minutes : 도착 지연 시간
# target : 만족 여부

data1 = data1 %>% mutate(total.satisfaction = (Seat.comfort + Food.and.drink + Leg.room.service + Cleanliness + Inflight.entertainment + Inflight.wifi.service + Online.support) *
                           (Ease.of.Online.booking + On.board.service + Departure.Arrival.time.convenient + Gate.location + Baggage.handling + Checkin.service + Online.boarding))

# 
# data1 = data1 %>% mutate(Garage.Yr.Blt_cal = 2011 - Garage.Yr.Blt,
#                          Year.Remod.Add_cal = 2011 - Year.Remod.Add,
#                          Year.Built_cal = 2011 - Year.Built)
# 
# data1 = data1 %>% mutate(comb.Built = Year.Built_cal + Year.Remod.Add_cal)
# data1 = data1 %>% mutate(comb.Area = Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + X1st.Flr.SF)
# data1 = data1 %>% mutate(total.Price.Index = Overall.Qual * comb.Qual * comb.Area)
# 
# 
# 
# data1$Year.Remod.Add = NULL
# data1$Garage.Yr.Blt = NULL
# 
# 
# 
# 
# # 시각화
# ggplot(data = data1[!is.na(data1$target),], aes(x = total.Price.Index, y = target)) +
#   geom_point(col = 'blue') + 
#   geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1))





# 정규화 - log
data1 %>% head

data1 = cbind(as.data.frame(data1[,!(names(data1) %in% c("Age", "Flight.Distance", "Gender.Female", "Gender.Male", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes"))]),
              as.data.frame(log(data1[,names(data1) %in% c("Age", "Flight.Distance", "Gender.Female", "Gender.Male", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")]+1)))

data1 = data1 %>% relocate(target, .after = last_col())




colnames(data1)[1] = "division"
colnames(data1)[2] = "id"
colnames(data1)[ncol(data1)] = "target"

# 이상치 제거
outlierTest_lm = lm(target ~ ., data=data1[,3:ncol(data1)])
outlierTest(outlierTest_lm, cutoff = 0.5)
olt = outlierTest(outlierTest_lm, 0.5) 
olt = as.numeric(row.names(as.data.frame(olt$rstudent)))

data1 = data1[!(as.numeric(rownames(data1)) %in% olt),]





ggplot(data = data1[!is.na(data1$target),], aes(x = Departure.Delay.in.Minutes, y = target)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1))






data1 = data1 %>% relocate(target, .after = last_col())

# 상관 계수 확인 후 - 상관성 높은 변수 drop
cor(data1[!is.na(data1$target) ,3:ncol(data1)]) %>% tail
DataExplorer::plot_correlation(data1[!is.na(data1$target) ,3:ncol(data1)])


write.csv(data1, paste0("./data/prep/",sub_ver,".csv"), row.names = FALSE)
