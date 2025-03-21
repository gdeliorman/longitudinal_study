##libraies
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

##full original data
lopaplus<- read_excel("/Users/gokcedeliorman/Downloads/Lopaplus_excel.xlsx", col_names=TRUE)

##change type
lopaplus$BPRS<-  as.numeric(lopaplus$BPRS)
lopaplus$TREAT<- as.factor(lopaplus$TREAT)
lopaplus$XTREAT<-as.factor(lopaplus$XTREAT)
lopaplus$CRFID<- as.factor(lopaplus$CRFID)
lopaplus$TRIAL<- as.factor(lopaplus$TRIAL)
lopaplus$XTIME<- as.factor(lopaplus$XTIME)
lopaplus$PANSS<- as.numeric(lopaplus$PANSS)
lopaplus$INVEST<-as.factor(lopaplus$INVEST)
lopaplus$CGI_SEV<-as.numeric(lopaplus$CGI_SEV)

##remove na and unnecessary columns
lopaplus<-lopaplus[,-c(8)]

lopaplus<- lopaplus[-(which(is.na(lopaplus$CGI_SEV)==TRUE)),]
lopaplus<- lopaplus[-(which(is.na(lopaplus$BPRS)==TRUE)),]
summary(lopaplus)
which(is.na(lopaplus)==TRUE)

##obtain longitudinal data
##case_1: trial 1 - surrogate BPRS true enpoint is PANSS

case_1_fin1<- lopaplus[lopaplus$TRIAL=="FIN-1",]## 284 HUTTUNEN
case_1_fin1_bprs <- case_1_fin1 %>%
  pivot_longer(cols = c(PANSS, BPRS), names_to = "resp2", values_to = "resp") %>%
  select(CRFID, INVEST, XTREAT, XTIME, TREAT, resp, resp2)

case_1_fin1_bprs$resp3<- ifelse(case_1_fin1_bprs$resp2=="PANSS", 1,0)

##add id 
case_1_fin1_bprs <- case_1_fin1_bprs %>%
  mutate(unique_id = dense_rank(CRFID)) %>%  # Assign unique ID to each CRFID
  select(unique_id, everything()) 

##change column names
colnames(case_1_fin1_bprs)[colnames(case_1_fin1_bprs) == "unique_id"] <- "id"
colnames(case_1_fin1_bprs)[colnames(case_1_fin1_bprs) == "resp3"] <- "type"
colnames(case_1_fin1_bprs)[colnames(case_1_fin1_bprs) == "resp2"] <- "type2"

##change type
case_1_fin1_bprs$id<-as.factor(case_1_fin1_bprs$id)
case_1_fin1_bprs$type<- as.factor(case_1_fin1_bprs$type)
case_1_fin1_bprs <- case_1_fin1_bprs %>% arrange(id, TREAT, XTIME,type)
summary(case_1_fin1_bprs)

##seperate as control and treatment
placebo_case_1_fin1_bprs<- case_1_fin1_bprs[case_1_fin1_bprs$TREAT == "0",]
experimental_case_1_fin1_bprs<- case_1_fin1_bprs[case_1_fin1_bprs$TREAT == "1",]

cat(length(unique(case_1_fin1_bprs$id)), "total patients")
cat(length(unique(placebo_case_1_fin1_bprs$id)), "patients in placebo")
cat(length(unique(experimental_case_1_fin1_bprs$id)), "patients in experimental")

# ##they can be removed if yes add new id
# aa<-case_1_fin1_bprs %>% count(id)
# which(aa$n<3)
# case_1_fin1_bprs <- case_1_fin1_bprs %>%
#   filter(!id %in% which(aa$n<3)) ## 562 patients
# case_1_fin1_bprs <- case_1_fin1_bprs %>%
#   mutate(unique_id2 = dense_rank(id)) %>%  # Assign unique ID to each CRFID
#   select(unique_id2, everything())  # Move unique_id to the first column
# case_1_fin1_bprs<- case_1_fin1_bprs[,-2]
# colnames(case_1_fin1_bprs)[colnames(case_1_fin1_bprs) == "unique_id2"] <- "id"
# aa_subset <- aa[which(aa$n == 10), ]
# subset_case_1_fin1_bprs <- case_1_fin1_bprs[case_1_fin1_bprs$id %in% aa_subset$id, ]



##case_2: trial 1 - surrogate endpoint is PANSS true enpoint is CGI
case_1_fin1_cgi <- case_1_fin1 %>%
  pivot_longer(cols = c(CGI_SEV, PANSS), names_to = "resp2", values_to = "resp") %>%
  select(CRFID, INVEST, XTREAT, XTIME, TREAT, resp, resp2)

case_1_fin1_cgi$resp3<- ifelse(case_1_fin1_cgi$resp2=="PANSS", 0,1)

case_1_fin1_cgi <- case_1_fin1_cgi %>%
  mutate(unique_id = dense_rank(CRFID)) %>%  # Assign unique ID to each CRFID
  select(unique_id, everything()) 

colnames(case_1_fin1_cgi)[colnames(case_1_fin1_cgi) == "unique_id"] <- "id"
colnames(case_1_fin1_cgi)[colnames(case_1_fin1_cgi) == "resp3"] <- "type"
colnames(case_1_fin1_cgi)[colnames(case_1_fin1_cgi) == "resp2"] <- "type2"

##order
case_1_fin1_cgi <- case_1_fin1_cgi %>% arrange(id, TREAT, XTIME,type)

##seperate as control and treatment
placebo_case_1_fin1_cgi<- case_1_fin1_cgi[case_1_fin1_cgi$TREAT == "0",]
experimental_case_1_fin1_cgi<- case_1_fin1_cgi[case_1_fin1_cgi$TREAT == "1",]

cat(length(unique(case_1_fin1_cgi$id)), "total patients")
cat(length(unique(placebo_case_1_fin1_cgi$id)), "patients in placebo")
cat(length(unique(experimental_case_1_fin1_cgi$id)), "patients in experimental")

# ##they can be removed
# bb<-case_1_fin1_cgi %>% count(id)
# which(bb$n<3)
# case_1_fin1_cgi <- case_1_fin1_cgi %>%
#   filter(!id %in% which(bb$n<3))

##write all data as csv
write.csv(placebo_case_1_fin1_bprs,"~/Downloads/placebo_case_1_fin1_bprs.csv", row.names = FALSE)
write.csv(experimental_case_1_fin1_bprs,"~/Downloads/experimental_case_1_fin1_bprs.csv", row.names = FALSE)
write.csv(placebo_case_1_fin1_cgi,"~/Downloads/placebo_case_1_fin1_cgi.csv", row.names = FALSE)
write.csv(experimental_case_1_fin1_cgi,"~/Downloads/experimental_case_1_fin1_cgi.csv", row.names = FALSE)

write.csv(case_1_fin1_cgi,"~/Downloads/case_1_fin1_cgi.csv", row.names = FALSE)
write.csv(case_1_fin1_bprs,"~/Downloads/case_1_fin1_bprs.csv", row.names = FALSE)


