
#############

source("./src/02_preprocessing.R", encoding = "UTF-8")

data2 = read.csv(paste0("./data/prep/",sub_ver,".csv"))

data2$id = as.character(data2$id)

train2 = data2 %>% filter(division == "train")
test2 = data2 %>% filter(division == "test")

train2$division = NULL
test2$division = NULL

# python run set
train3 = train2
test3 = test2
test3$target = NULL
fwrite(train3, "./data/prep/train3.csv")
fwrite(test3, "./data/prep/test3.csv")

############
