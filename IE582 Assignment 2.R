ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}

packages <- c( "tidyverse","data.table")
ipak(packages)
uspop = fread("http://personal.psu.edu/mzm6664/uspop_assgn.csv",header = T)
metapop = fread("http://personal.psu.edu/mzm6664/meta_assgn.csv",header = T)
dtset_quck_view = function (data_name){
  print("Dimension of the dataset is")
  print(dim(data_name))
  print("The number of rows")
  print(dim(data_name)[1])
  print("The number of columns")
  print(dim(data_name)[2])
  print("The Columns names are:")
  print(colnames(data_name))
  print("The first few rows of the data")
  print(head(data_name))
  print("The last few rows of the data")
  print(tail(data_name))
}
dtset_quck_view(uspop)
dtset_quck_view(metapop)

library(ggplot2)
# Q2
# subset older
older <- subset(uspop, AGEGRP>13)
# State
ST <- unique(uspop[,STNAME])
OLD_POP <- rep(NA, 51)
i=1
for (x in ST) {
  state <- subset(older, STNAME == x)
  OLD_POP[i] <- sum(state$TOT_POP)
  i<-i+1
}
OLD_ST <- data.frame(ST,OLD_POP)
new_OLD_ST <- OLD_ST[order(-OLD_POP),]
ggplot(new_OLD_ST[1:10,],aes(y=OLD_POP,x=factor(ST,level =ST))) + geom_bar(stat="identity")+
  xlab('State') + ylab('Population (in millions)') +
  theme(axis.text.x = element_text(angle = 45,hjust=1))
# County
FIPS <- unique(uspop[,FIPS])
OLD_POP_C <- rep(NA, 3142)
i=1
for (x in FIPS) {
  county <- subset(older, FIPS == x)
  OLD_POP_C[i] <- sum(county$TOT_POP)
  i<-i+1
}
FIPSCT <- unique.matrix(older[,c('FIPS','STNAME','CTYNAME')])
OLD_CT <- data.frame(FIPSCT,OLD_POP_C)
new_OLD_CT <- OLD_CT[order(-OLD_POP_C),]
new_OLD_CT$SC <- paste(new_OLD_CT$CTYNAME,'\n(',new_OLD_CT$STNAME,')')
ggplot(new_OLD_CT[1:10,],aes(y=OLD_POP_C,x=factor(SC,level =SC))) + geom_bar(stat="identity")+
  xlab('County') + ylab('Population (in millions)') +
  theme(axis.text.x = element_text(angle = 45,hjust=1))
#Q3
TOTAL_MALE <- rep(NA, 51)
TOTAL_FEMALE <- rep(NA, 51)
i=1
for (x in ST) {
  state <- subset(uspop, AGEGRP == 0 & STNAME == x)
  TOTAL_MALE[i] <- sum(state$TOT_MALE)
  TOTAL_FEMALE[i] <- sum(state$TOT_FEMALE)
  if (TOTAL_MALE[i] < TOTAL_FEMALE[i]){
    print(x)
  }
  i<-i+1
}
#3.2
uspop_tot <- subset(uspop, AGEGRP == 0)
uspop_tot$high_number <- ifelse(uspop_tot$TOT_MALE < uspop_tot$TOT_FEMALE, 1, 0)
high_number_count <- rep(NA, 51)
i=1
for (x in ST) {
  uspop_state <- subset(uspop, AGEGRP == 0 & STNAME == x)
  high_number_count[i] <- sum(uspop_state$high_number)
  i <- i+1
}
gender_comp <- data.frame(ST,high_number_count)
#3.3
library(plyr)
gender_comp$total_county <- ddply(FIPSCT,.(STNAME),nrow)[,2]
gender_comp$percent <- gender_comp$high_number_count/gender_comp$total_county[,2]
#4.1
Penn <- subset(uspop, AGEGRP == 0 & STNAME == 'Pennsylvania')
Penn$WA_MH <- Penn$WA_MALE/Penn$TOT_MALE*10000
write.csv(Penn,
          "~/OneDrive - The Pennsylvania State University/Study/FA21/CE582/Assignments/Assignment2_4_1.csv", 
          row.names = FALSE)
#4.2
AR <- subset(uspop, AGEGRP == 0 & STNAME == 'Arkansas')
AR$BA_FV <- AR$BA_FEMALE/sum(AR$BA_FEMALE)*10000
write.csv(AR,
          "~/OneDrive - The Pennsylvania State University/Study/FA21/CE582/Assignments/Assignment2_4_2.csv", 
          row.names = FALSE)
#4.3
data_normalization <- function(stname, standardization_direction,value){
  state_dataset <- subset(uspop, AGEGRP == 0 & STNAME == stname)
  output_column = value+'_'+standardization_direction
  if(grepl("FE",value)){
    tot_gender = TOT_FEMALE
  } else{
    tot_gender = TOT_MALE
}
  if(standardization_direction = 'horizontal'){
  state_dataset$output_column <- state_dataset$value/state_dataset$tot_gender*10000
  return(toupper(spacer_icef))
} else if (standardization_direction = 'vertical'){
  state_dataset$output_column <- state_dataset$value/sum(state_dataset$value)*10000
  return(toupper(spacer_icef))
}
}
#Bonus Question
data_normalization <- function(stname, standardization_direction,value,standardization_control){
  state_dataset <- subset(uspop, AGEGRP == 0 & STNAME == stname)
  if(standardization_direction = 'horizontal'){
    state_dataset$output_column <- state_dataset$value/state_dataset$standardization_control*10000
    return(toupper(spacer_icef))
  } else if (standardization_direction = 'vertical'){
    state_dataset$output_column <- state_dataset$value/sum(state_dataset$value)*10000
    return(toupper(spacer_icef))
  }
}