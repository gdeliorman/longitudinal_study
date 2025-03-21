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

lopaplus_3_trial<- lopaplus[lopaplus$TRIAL=="FRA-3" | lopaplus$TRIAL=="INT-2" | lopaplus$TRIAL=="INT-3" ,]## 284 HUTTUNEN
lopaplus_3_trial<- lopaplus_3_trial[-(which(lopaplus_3_trial$XTREAT=="LEVOMEPROMAZINE")),]

lopaplus_3_trial %>% 
  count(XTIME, XTIME)

##remove these times
lopaplus_3_trial<- lopaplus_3_trial[-(which(lopaplus_3_trial$XTIME==3)),]
lopaplus_3_trial<- lopaplus_3_trial[-(which(lopaplus_3_trial$XTIME==5)),]
lopaplus_3_trial<- lopaplus_3_trial[-(which(lopaplus_3_trial$XTIME==10)),]
lopaplus_3_trial<- lopaplus_3_trial[-(which(lopaplus_3_trial$XTIME==12)),]

case_2_bprs <- lopaplus_3_trial %>%
  pivot_longer(cols = c(PANSS, BPRS), names_to = "resp2", values_to = "resp") %>%
  select(CRFID, INVEST, XTREAT, XTIME, TREAT, resp, resp2)

case_2_bprs$resp3<- ifelse(case_2_bprs$resp2=="PANSS", 1,0)

##add id 
case_2_bprs$id<- as.numeric(as.factor(with(case_2_bprs,paste(CRFID,INVEST,sep="_"))))

##change column names
colnames(case_2_bprs)[colnames(case_2_bprs) == "resp3"] <- "type"
colnames(case_2_bprs)[colnames(case_2_bprs) == "resp2"] <- "type2"

##change type
case_2_bprs$id<-as.factor(case_2_bprs$id)
case_2_bprs$type<- as.factor(case_2_bprs$type)
case_2_bprs <- case_2_bprs %>% arrange(id, TREAT, XTIME,type)
summary(case_2_bprs)

##seperate as control and treatment
placebo_case_2_bprs<- case_2_bprs[case_2_bprs$TREAT == "0",]
experimental_case_2_bprs<- case_2_bprs[case_2_bprs$TREAT == "1",]

cat(length(unique(case_2_bprs$id)), "total patients")
cat(length(unique(placebo_case_2_bprs$id)), "patients in placebo")
cat(length(unique(experimental_case_2_bprs$id)), "patients in experimental")

# ##they can be removed if yes add new id
# aa<-case_2_bprs %>% count(id)
# which(aa$n<3)
# case_2_bprs <- case_2_bprs %>%
#   filter(!id %in% which(aa$n<3)) ## 562 patients
# case_2_bprs <- case_2_bprs %>%
#   mutate(unique_id2 = dense_rank(id)) %>%  # Assign unique ID to each CRFID
#   select(unique_id2, everything())  # Move unique_id to the first column
# case_2_bprs<- case_2_bprs[,-2]
# colnames(case_2_bprs)[colnames(case_2_bprs) == "unique_id2"] <- "id"
# aa_subset <- aa[which(aa$n == 10), ]
# subset_case_2_bprs <- case_2_bprs[case_2_bprs$id %in% aa_subset$id, ]



##case_2: trial 1 - surrogate endpoint is PANSS true enpoint is CGI
case_2_cgi <- lopaplus_3_trial %>%
  pivot_longer(cols = c(CGI_SEV, PANSS), names_to = "resp2", values_to = "resp") %>%
  select(CRFID, INVEST, XTREAT, XTIME, TREAT, resp, resp2)

case_2_cgi$resp3<- ifelse(case_2_cgi$resp2=="PANSS", 0,1)

case_2_cgi <- case_2_cgi %>%
  mutate(unique_id = dense_rank(CRFID)) %>%  # Assign unique ID to each CRFID
  select(unique_id, everything()) 

colnames(case_2_cgi)[colnames(case_2_cgi) == "unique_id"] <- "id"
colnames(case_2_cgi)[colnames(case_2_cgi) == "resp3"] <- "type"
colnames(case_2_cgi)[colnames(case_2_cgi) == "resp2"] <- "type2"

##order
case_2_cgi <- case_2_cgi %>% arrange(id, TREAT, XTIME,type)

##seperate as control and treatment
placebo_case_2_cgi<- case_2_cgi[case_2_cgi$TREAT == "0",]
experimental_case_2_cgi<- case_2_cgi[case_2_cgi$TREAT == "1",]

cat(length(unique(case_2_cgi$id)), "total patients")
cat(length(unique(placebo_case_2_cgi$id)), "patients in placebo")
cat(length(unique(experimental_case_2_cgi$id)), "patients in experimental")

# ##they can be removed
# bb<-case_2_cgi %>% count(id)
# which(bb$n<3)
# case_2_cgi <- case_2_cgi %>%
#   filter(!id %in% which(bb$n<3))

##write all data as csv
write.csv(placebo_case_2_cgi,"~/Downloads/placebo_case_2_cgi.csv", row.names = FALSE)
write.csv(experimental_case_2_cgi,"~/Downloads/experimental_case_2_cgi.csv", row.names = FALSE)
write.csv(placebo_case_2_bprs,"~/Downloads/placebo_case_2_bprs.csv", row.names = FALSE)
write.csv(experimental_case_2_bprs,"~/Downloads/experimental_case_2_bprs.csv", row.names = FALSE)

write.csv(case_2_cgi,"~/Downloads/case_2_cgi.csv", row.names = FALSE)
write.csv(case_2_bprs,"~/Downloads/case_2_bprs.csv", row.names = FALSE)


