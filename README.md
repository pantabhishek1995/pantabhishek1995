###Package Installation###

install.packages("dplyr")
install.packages("haven")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("broom")
install.packages("modelsummary")
install.packages("parsnip")
install.packages("janitor")
install.packages("jtools")
install.packages("vip")
install.packages("gridExtra")


library(dplyr)
library(haven)
library(ggplot2)
library(tidyverse)
library(broom)
library(modelsummary)
library(parsnip)
library(janitor)
library(jtools)
library(vip)
library(gridExtra)
###########################

#######Loading data#######

setwd("/Users/Abhishek/Desktop/TUM/Quantitative Methods/Research Paper/Working Dir/")
file1<-read_dta(file = "ZA7505_v2-0-0.dta")
testfile1<-read.csv(file = "WV6_Data_R_v20201117.csv")

###########################

#######Data cleaning#######

#Usable data
file2<-file1[,c(10,11,41:231)]

#Testing sets for model<>Sets with same mathematical characteristics<>Based on WVS questionnaire
set1<-file2[,c(3:10)]
set2<-file2[,c(3:10,160)]
set3<-file2[,c(3:10,176)]
set5<-file2[,c(81:95)]
set6<-file2[,c(81:95,160)]
set7<-file2[,c(134:146)]

#Column Names
colnames(set1) <- c("Imp of Family", "Imp of Friends", "Imp of Leisure", "Imp of Politics", "Imp of Work", "Imp of Religion","Feeling of Happiness", "State of Health")
colnames(set2) <- c("Imp of Family", "Imp of Friends", "Imp of Leisure", "Imp of Politics", "Imp of Work", "Imp of Religion","Feeling of Happiness", "State of Health","Age")
colnames(set3) <- c("Imp of Family", "Imp of Friends", "Imp of Leisure", "Imp of Politics", "Imp of Work", "Imp of Religion","Feeling of Happiness", "State of Health","Education")
colnames(set5) <-c("Confidence in Churches","Confidence in Armed Forces","Confidence in Press","Confidence in Labour Unions","Confidence in Police","Confidence in Parliament","Confidence in Civil Services","Confidence in Regional organisations","Confidence in EU","Confidence in Government","Confidence in Political Parties","Confidence in Major Companies","Confidence in Environment protection movement","Confidence in Justice System","Confidence in UN")
colnames(set6) <-c("Confidence in Churches","Confidence in Armed Forces","Confidence in Press","Confidence in Labour Unions","Confidence in Police","Confidence in Parliament","Confidence in Civil Services","Confidence in Regional organisations","Confidence in EU","Confidence in Government","Confidence in Political Parties","Confidence in Major Companies","Confidence in Environment protection movement","Confidence in Justice System","Confidence in UN", "Age")
colnames(set7) <-c("Taking unentitled govt benefits is justifiable","Not buying tickets is justifiable","Tax fraud is justifiable","Bribery is justifiable", "Homosexuality is justifiable","Prostitution is justifiable","Abortion is justifiable","Divorce is justifiable","Euthanasia is justifiable","Suicide is justifiable","Casual Sex is justifiable","Political Voilence is justifiable","Death Penalty is justifiable")

#Removing values corresponding to wrong/no answer filled by correspondent
set1[set1 < 0] <- NA 
set1 <- na.omit(set1)
set2[set2 < 0] <- NA 
set2 <- na.omit(set2)
set3[set3 < 0] <- NA 
set3 <- na.omit(set3)
set5[set5 < 0] <- NA 
set5 <- na.omit(set5)
set6[set6 < 0] <- NA 
set6 <- na.omit(set6)
set7[set7 < 0] <- NA 
set7 <- na.omit(set7)

#######Function for regression#######

#Automation function using LM

quantmodel<-function(a){
  #Initial setup and regression type selection  
  i<-1
  a <- janitor::clean_names(a)
  colnames1 <- colnames(a)
  lm_model <- linear_reg() %>% 
    set_engine('lm') %>%
    set_mode('regression')
  
  #New working directory for saving results
  setwd("/Users/Abhishek/Desktop/TUM/Quantitative Methods/Research Paper/Working Dir/Test results/")
  out_lst <- list()
  #Scale function
  for (i in seq_along(a)) {

    a[,i]<-scale(a[,i])    
  }
  
  #Regression loop
  i=1
  for (i in seq_along(a)) {
    lm_fit <- lm_model %>% 
      fit(as.formula(paste(colnames1[i], "~ .")), data = a)
    
    out_lst[[i]] <- lm_fit
    #Saving relevance plot of each parameter
    temp_plot = vip(lm_fit ,geom = "col", aesthetics = list(color = "black", fill = "black"))
    ggsave(temp_plot, file=paste0("relevanceplot_", colnames1[i],".png"), width = 14, height = 10, units = "cm")
    #Saving pdf of individual summaries
    pdf(paste0("summary_", colnames1[i],".pdf"), width = 15,height = 5) 
    grid.table(coef(summary(lm_fit$fit)))
    dev.off()
  
  }
  #Switching back to working directory and returning model results from function
  setwd("/Users/Abhishek/Desktop/TUM/Quantitative Methods/Research Paper/Working Dir/")
  return(out_lst)
}
set5<-set5[c(1:6)]
#Running model for datasets
model_results <- quantmodel(set7)   #Change input set as and when required
set7<-set7[c(7:13)]
#Creating summary table
modelsummary(dvnames(model_results),estimate = "{estimate}{stars}")
tab_model(model_results,auto.label = FALSE, show.ci = FALSE, show.p = FALSE, p.style = "stars", CSS = css_theme("cells"))

#Manual deep diving into prominent relations highlighted by automation

#######Country wise analysis into trends highlighted##########

#Data cleaning
set11<-file2[,c(2,3:10)]
set21<-file2[,c(2,3:10,160)]
set31<-file2[,c(2,3:10,176)]
set51<-file2[,c(2,81:95)]
set61<-file2[,c(2,81:95,160)]
set71<-file2[,c(2,134:146)]

#Column Names
colnames(set11) <- c("Cntry_AN","Imp of Family", "Imp of Friends", "Imp of Leisure", "Imp of Politics", "Imp of Work", "Imp of Religion","Feeling of Happiness", "State of Health")
colnames(set21) <- c("Cntry_AN","Imp of Family", "Imp of Friends", "Imp of Leisure", "Imp of Politics", "Imp of Work", "Imp of Religion","Feeling of Happiness", "State of Health", "Age")
colnames(set31) <- c("Cntry_AN","Imp of Family", "Imp of Friends", "Imp of Leisure", "Imp of Politics", "Imp of Work", "Imp of Religion","Feeling of Happiness", "State of Health","Education")
colnames(set51) <-c("Cntry_AN","Confidence in Churches","Confidence in Armed Forces","Confidence in Press","Confidence in Labour Unions","Confidence in Police","Confidence in Parliament","Confidence in Civil Services","Confidence in Regional organisations","Confidence in EU","Confidence in Government","Confidence in Political Parties","Confidence in Major Companies","Confidence in Environment protection movement","Confidence in Justice System","Confidence in UN")
colnames(set61) <-c("Cntry_AN","Confidence in Churches","Confidence in Armed Forces","Confidence in Press","Confidence in Labour Unions","Confidence in Police","Confidence in Parliament","Confidence in Civil Services","Confidence in Regional organisations","Confidence in EU","Confidence in Government","Confidence in Political Parties","Confidence in Major Companies","Confidence in Environment protection movement","Confidence in Justice System","Confidence in UN","Age")
colnames(set71) <-c("Cntry_AN","Taking unentitled govt benefits is justifiable","Not buying tickets is justifiable","Tax fraud is justifiable","Bribery is justifiable", "Homosexuality is justifiable","Prostitution is justifiable","Abortion is justifiable","Divorce is justifiable","Euthanasia is justifiable","Suicide is justifiable","Casual Sex is justifiable","Political Voilence is justifiable","Death Penalty is justifiable")

#Removing values corresponding to wrong/no answer filled by correspondent
set11[set11 < 0] <- NA 
set11 <- na.omit(set11)
set21[set21 < 0] <- NA 
set21 <- na.omit(set21)
set31[set31 < 0] <- NA 
set31 <- na.omit(set31)
set51[set51 < 0] <- NA 
set51 <- na.omit(set51)
set61[set61 < 0] <- NA 
set61 <- na.omit(set61)
set71[set71 < 0] <- NA 
set71 <- na.omit(set71)

######Country Summary Rollup######

country_summary1 <- set11 %>% 
  group_by(Cntry_AN)  %>% 
  summarise_each(funs(mean))

country_summary2 <- set21 %>% 
  group_by(Cntry_AN)  %>% 
  summarise_each(funs(mean))

country_summary3 <- set31 %>% 
  group_by(Cntry_AN)  %>% 
  summarise_each(funs(mean))

country_summary5 <- set51 %>% 
  group_by(Cntry_AN)  %>% 
  summarise_each(funs(mean))

country_summary6 <- set61 %>% 
  group_by(Cntry_AN)  %>% 
  summarise_each(funs(mean))

country_summary7 <- set71 %>% 
  group_by(Cntry_AN)  %>% 
  summarise_each(funs(mean))

######SUMMARY########

#Country plot
#Change value as per dataset and columns highlighted in automation framework
plot_country<-ggplot(country_summary7, aes(x=`Abortion is justifiable`, y=`Divorce is justifiable`, label = Cntry_AN))+ 
  geom_point(aes())+
  geom_smooth(method=lm)+
  geom_text(vjust = -0.5)+
  xlab("Abortion is justifiable") +
  ylab("Divorce is justifiable") +
  ggtitle("Relation between Divorce and Abortion justifiability")

plot_country

###Testing automation of country summary####
#############TEST START#############
countrymodel<-function(a,b,c){
  
  country_summary1 <- a %>% 
    group_by(Cntry_AN)  %>% 
    summarise_each(funs(mean))
  
  set_sample<-sample_n(a,1300)
  
  plot_country<-ggplot(country_summary1, aes(x=b, y=c, label = Cntry_AN))+ 
    geom_point(aes())+
    geom_smooth(method=lm)+
    geom_text(vjust = -0.5)+
    xlab("b") +
    ylab("c") +
    ggtitle("Country level analysis")
  
  return(plot_country)
  
  #Coplot with 1% data
  coplot_country<-coplot(a~ b|Cntry_AN, type="b", show.given = FALSE,data = set_sample)
  
  return(coplot_country)
}

#Summary
countrymodel(set31,`Education`,`Imp of Religion`)
coplot_country
plot_country

##########TEST END#########
