rm(list=ls())
library(stargazer)
library(lmtest)
library(sandwich)
library(wooldridge)
library(tidyverse)
library(car)
library(plm)
library(AER)
library(dplyr)
library(stats)
library(pwt10)
data("pwt10.0")
setwd("~/Desktop/tesi")


data.p <- pdata.frame(pwt10.0,index=c("country","year"))
data.p

financial_dev <- read.csv("financial_dev.csv", header=T, sep=",", na.strings = "")
fin.p <- pdata.frame(financial_dev,index=c("country","year"))

debt<-read.csv("debt.csv", header=T, sep=",", na.strings = "")
debt.p <- pdata.frame(debt,index=c("country","year"))
colnames(debt)[c(1,3,4)] <- c("country","year","debt.to.GDP")

urban<-read.csv("WDI_urban.csv", header=T, sep=",", na.strings = "")
colnames(urban)[c(1,2,5,7)] <- c("country","isocode","year", "urb.to.GDP")

database<-pwt10.0 %>% left_join(financial_dev)
data<- database%>%left_join(debt)
data1<-data%>%left_join(urban)
database.p <- pdata.frame(data1,index=c("country","year"))



dat<-database.p %>% filter( year==1970 | year==1980| year==1990 | year==2000| year==2010 |year==2020)

dMat = data.frame(dat)

dMat1<-dMat%>% filter(year==1970)
summary(rgdpo, na.omit())
attach(dMat1)
summary(dMat)


dr<-dMat %>% 
  group_by(country) %>%
  mutate(lagged_y= dplyr::lag(rgdpo,n=1)) %>% 
  mutate(lagged_N= dplyr::lag(emp, n=1)) %>%
  mutate(lagged_Hc=dplyr::lag(hc, n=1)) %>%
  mutate(lagged_K=dplyr::lag(cn, n=1)) %>%
  mutate(credit_GDP= dplyr::lag(di01,n=1)) %>%
  mutate(assets= dplyr::lag(di04,n=1)) %>%
  mutate(M3= dplyr::lag(di05,n=1)) %>%
  mutate(credit_other_GDP= dplyr::lag(di12,n=1)) %>%
  mutate(stock_market= dplyr::lag(dm02,n=1)) %>%
  mutate(lagged_pl_c= dplyr::lag(pl_c,n=1)) %>%
  mutate(lagged_debt= dplyr::lag(debt.to.GDP,n=1)) %>% 
  mutate(lagged_urb= dplyr::lag(urb.to.GDP,n=1))%>%
  mutate(growth_rate = log(rgdpo/emp)-log(lagged_y/lagged_N)) %>%
  mutate(n = log(emp)-log(lagged_N)) %>%
  mutate(sh = log(hc)-log(lagged_Hc)) %>%
  mutate(sk = log(cn)-log(lagged_K)) 

dr$lagged_urb <- as.numeric(dr$lagged_urb)

dr.p <- pdata.frame(dr,index=c("country","year"))
is.pbalanced(dr.p)

summary(dr$rgdpo)


dd<-dr%>%
  mutate(Low_Income= ifelse(country == 'Afghanistan'|country == 'Burkina Faso'|country == 'Burundi'|country == 'Central African Republic'|country == 'Chad'|country == 'Congo, Democratic Republic'|country == 'Ethipia'|country == "Gambia"|country == 'Guinea'|country == 'Guinea-Bissau'|country == 'Liberia'|country == 'Madagascar'|country == 'Malawi'|country == 'Mali'|country == "Mozambique"|country == 'Niger'|country == 'Rwanda'|country == 'Sierra Leone'|country == 'Sudan'|country == 'Togo'|country == "Syrian Arab Republic"|country == 'Uganda'|country == 'Yemen'|country == "Zambia", 1, 0))

dd<-dd%>%
  mutate(LowerMiddle_Income= ifelse(country == 'Angola'|country == 'Algeria'|country == 'Bangladesh'|country == 'Benin'|country == 'Bhutan'|country == 'Bolivia'|country == 'Cabo Verde'|country == "Cambodia"|country == 'Cameroon'|country == 'Comoros'|country == 'Congo'|country == "Cote d'Ivoire"|country == 'Djibouti'|country == 'Egypt'|country == "El Salvador"|country == 'Eswatini'|country == 'Ghana'|country == 'Haiti'|country == 'Honduras'|country == 'India'|country == "Indonesia"|country == 'Iran (Islamic Republic of)'|country == 'Kyrgyzstan'|country == "Kenya"|country == "Lao People's DR"|country == 'Lebanon'|country == 'Lesotho'|country == 'Mauritania'|country == 'Mongolia'|country == 'Morocco'|country == 'Myanmar'|country == "Nepal"|country == 'Nicaragua'|country == 'Nigeria'|country == 'Pakistan'|country == "Philippines"|country == 'Sao Tome and Principe'|country == 'Senegal'|country == "Sri Lanka"|country == 'Tajikistan'|country == 'Zimbabwe'|country == 'Viet Nam', 1, 0))

dd<-dd%>%
  mutate(UpperMiddle_Income= ifelse(country == 'Albania'|country == 'Argentina'|country == 'Armenia'|country == 'Belarus'|country == 'Belize'|country == 'Bosnia and Herzegovina'|country == 'Botswana'|country == "Brazil"|country == 'Bulgaria'|country == 'China'|country == 'Colombia'|country == "Costa Rica"|country == 'Dominica'|country == 'Dominican Republic'|country == "Ecuador"|country == 'Fiji'|country == 'Gabon'|country == 'Georgia'|country == 'Grenada'|country == 'Guatemala'|country == "Guyana"|country == 'Iraq'|country == 'Jamaica'|country == "Jordan"|country == "Kazakhstan"|country == 'Libya'|country == 'Malaysia'|country == 'Maldives'|country == 'Montenegro'|country == 'Namibia'|country == 'North Macedonia'|country == "South Africa"|country == 'Paraguay'|country == 'Peru'|country == 'Russian Federation'|country == "Serbia"|country == 'St. Vincent & Grenadines'|country == 'Turkey'|country == "Turkmenistan", 1, 0))

dd<-dd%>%
  mutate(High_Income= ifelse(country=='China, Macao SAR'|country == 'Antigua and Barbuda'|country == 'Aruba'|country == 'Australia'|country == 'Austria'|country == 'Bahamas'|country == 'Bahrain'|country == 'Barbados'|country == "Belgium"|country == 'Bermuda'|country == 'British Virgin Islands'|country == 'Brunei Darussalam'|country == "Canada"|country == 'Cayman Islands'|country == 'Chile'|country == "Croatia"|country == 'Curacao'|country == 'Cyprus'|country == 'Czech Republic'|country == 'Denmark'|country == 'Estonia'|country == "Finland"|country == 'France'|country == 'Germany'|country == "Greece"|country == "China, Hong Kong SAR"|country == 'Hungary'|country == 'Iceland'|country == 'Ireland'|country == 'Israel'|country == 'Italy'|country == 'Japan'|country == "Kuwait"|country == 'Latvia'|country == 'Luxembourg'|country == 'Lithuania'|country == "Malta"|country == 'Netherlands'|country == 'New Zealand'|country == "Norway"|country == 'Oman'|country == "Panama"|country == 'Portugal'|country == 'Qatar'|country == 'Romania'|country == "Saudi Arabia"|country == 'Seychelles'|country == 'Sint Maarten (Dutch part)'|country == "Singapore"|country == 'Slovakia'|country == 'Slovenia'|country == 'Spain'|country == 'Sweden'|country == 'Switzerland'|country == "Trinidad and Tobago"|country == 'Taiwan'|country == 'Turks and Caicos Islands'|country == "United Arab Emirates"|country == "United Kingdom"|country == 'United States'|country == 'Uruguay', 1, 0))



w<-cor(dr.p[, c('growth_rate', 'lagged_y','sk','n', 'sh','credit_GDP', 'assets', 'M3', 'credit_other_GDP', 'stock_market', 'lagged_pl_c', 'lagged_debt', 'lagged_urb')],use="complete.obs")

stargazer(w,
          header=FALSE, type="text")

Solow_OLS<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh), data = dr)
r1<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh), data = dr)
coef1<-coeftest(Solow_OLS, vcov. = vcovHC)


#Solow_OLS_Dummy<- lm(growth_rate ~ log(lagged_y)+ log(n+0,05)+ log(sk)+log(sh)+LowerMiddle_Income+UpperMiddle_Income+High_Income, data = dd)
#coef2<-coeftest(Solow_OLS_Dummy, vcov. = vcovHC)

Solow_OLS_Dummy_dip<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh)+LowerMiddle_Income+UpperMiddle_Income+High_Income+log(lagged_y)*LowerMiddle_Income+log(lagged_y)*UpperMiddle_Income+log(lagged_y)*High_Income +log(n+0.05)*LowerMiddle_Income+ log(n+0.05)*UpperMiddle_Income+ log(n+0.05)*High_Income+ log(sk)*LowerMiddle_Income+ log(sk)*UpperMiddle_Income+ log(sk)*High_Income+ log(sh)*LowerMiddle_Income+ log(sh)*UpperMiddle_Income+ log(sh)*High_Income, data = dd)
r2<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh)+LowerMiddle_Income+UpperMiddle_Income+High_Income+log(lagged_y)*LowerMiddle_Income+log(lagged_y)*UpperMiddle_Income+log(lagged_y)*High_Income +log(n+0.05)*LowerMiddle_Income+ log(n+0.05)*UpperMiddle_Income+ log(n+0.05)*High_Income+ log(sk)*LowerMiddle_Income+ log(sk)*UpperMiddle_Income+ log(sk)*High_Income+ log(sh)*LowerMiddle_Income+ log(sh)*UpperMiddle_Income+ log(sh)*High_Income, data = dd)
coefind<-coeftest(Solow_OLS_Dummy_dip, vcov. = vcovHC)

stargazer(coef1,coef2, coefind, 
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          column.labels=c("OLS","IV"), type="text")



stargazer(coef1, coef2, 
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          column.labels=c("OLS","IV"), type="text")

Solow_OLS_controlli<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets + M3 + credit_other_GDP + stock_market +lagged_pl_c + lagged_debt + lagged_urb   , data = dr)
coef3<-coeftest(Solow_OLS_controlli, vcov. = vcovHC)


Solow_OLS_controlli1<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb   , data = dr)
r3<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb  , data = dr)
pippo1<-coeftest(Solow_OLS_controlli1, vcov. = vcovHC)

Solow_OLS_controlli2<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + M3 +  stock_market +lagged_pl_c + lagged_debt + lagged_urb   , data = dr)
pippo2<-coeftest(Solow_OLS_controlli2, vcov. = vcovHC)

Solow_OLS_controlli3<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh)+ credit_other_GDP + stock_market +lagged_pl_c + lagged_debt + lagged_urb   , data = dr)
pippo3<-coeftest(Solow_OLS_controlli3, vcov. = vcovHC)

Solow_OLS_controlli4<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh)+ credit_GDP + stock_market +lagged_pl_c + lagged_debt + lagged_urb   , data = dr)
pippo4<-coeftest(Solow_OLS_controlli4, vcov. = vcovHC)

stargazer(pippo1, pippo2, pippo3, pippo4 , 
          header=FALSE, 
          title = "OLS with different confrols for financial system",
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=TRUE)


stare<-linearHypothesis(Solow_OLS_controlli1, c("assets=0","stock_market=0","lagged_pl_c=0","lagged_debt=0","lagged_urb=0"), vcov. = vcovHC)

stargazer(stare,
          header=FALSE, type="text")


linearHypothesis(Solow_OLS_controlli1, c("assets=0","stock_market=0"), vcov. = vcovHC)
linearHypothesis(Solow_OLS_controlli1, c("lagged_pl_c=0","lagged_debt.to.GDP=0"), vcov. = vcovHC)



stargazer(pippo1, pippo2,pippo3,pippo4, 
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          column.labels=c("OLS","IV"), type="text")


Solow_OLS_controlli_Dummy<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets+ stock_market +lagged_pl_c + lagged_debt + lagged_urb +LowerMiddle_Income+UpperMiddle_Income+High_Income +log(lagged_y)*LowerMiddle_Income+log(lagged_y)*UpperMiddle_Income+log(lagged_y)*High_Income +log(n+0.05)*LowerMiddle_Income+ log(n+0.05)*UpperMiddle_Income+ log(n+0.05)*High_Income+ log(sk)*LowerMiddle_Income+ log(sk)*UpperMiddle_Income+ log(sk)*High_Income+ log(sh)*LowerMiddle_Income+ log(sh)*UpperMiddle_Income+ log(sh)*High_Income  , data = dd)
r4<- lm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets+ stock_market +lagged_pl_c + lagged_debt + lagged_urb +LowerMiddle_Income+UpperMiddle_Income+High_Income +log(lagged_y)*LowerMiddle_Income+log(lagged_y)*UpperMiddle_Income+log(lagged_y)*High_Income +log(n+0.05)*LowerMiddle_Income+ log(n+0.05)*UpperMiddle_Income+ log(n+0.05)*High_Income+ log(sk)*LowerMiddle_Income+ log(sk)*UpperMiddle_Income+ log(sk)*High_Income+ log(sh)*LowerMiddle_Income+ log(sh)*UpperMiddle_Income+ log(sh)*High_Income  , data = dd)
coef4<-coeftest(Solow_OLS_controlli_Dummy, vcov. = vcovHC)

stargazer(coef1, coefind, pippo1,coef4,
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          column.labels=c("OLS","IV"), type="text")





Solow_Panel <- plm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh), data=dr.p, model="within", effect="twoways")
r5<-plm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh), data=dr.p, model="within", effect="twoways")
coefp<-coeftest(Solow_Panel, vcov=vcovHC(Solow_Panel,cluster = "group"))

Solow_Panel_controlli <- plm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb   , data=dr.p, model="within", effect="twoways")
r6<-plm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb  , data=dr.p, model="within", effect="twoways")
coefpc<-coeftest(Solow_Panel_controlli, vcov=vcovHC(Solow_Panel_controlli,cluster = "group"))

stargazer(coef1, coefp, coefpc, 
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          type="text")


IV_Solow <- ivreg(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh)|lagged_y+lagged_N+lagged_Hc+lagged_K, data=dr)
r7<-ivreg(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh)|lagged_y+lagged_N+lagged_Hc+lagged_K, data=dr)
a<-coeftest(IV_Solow, vcov. = vcovHC)

IV_Solow_controlli<- ivreg(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb  |lagged_y+lagged_N+lagged_Hc+lagged_K + assets + M3 + credit_other_GDP + stock_market +lagged_pl_c + lagged_debt + lagged_urb, data=dr)
r8<-ivreg(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb  |lagged_y+lagged_N+lagged_Hc+lagged_K + assets + M3 + credit_other_GDP + stock_market +lagged_pl_c + lagged_debt + lagged_urb, data=dr)
b<-coeftest(IV_Solow_controlli, vcov. = vcovHC)

IV_panel<- plm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh)|lagged_y+lagged_N+lagged_Hc+lagged_K,index = c("country", "year"), model = "within", effect = "twoways", data=dr.p)
c<-coeftest(IV_panel, vcov=vcovHC(IV_panel,cluster = "group"))

#c in appendice

Final_Model<- plm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets  + stock_market +lagged_pl_c + lagged_debt+ lagged_urb   |lagged_y+lagged_N+lagged_Hc+lagged_K + assets + M3 + credit_other_GDP + stock_market +lagged_pl_c + lagged_debt + lagged_urb ,index = c("country", "year"), model = "within", effect = "twoways", data=dr.p)
r9<-plm(growth_rate ~ log(lagged_y)+ log(n+0.05)+ log(sk)+log(sh) + assets  + stock_market +lagged_pl_c + lagged_debt + lagged_urb   |lagged_y+lagged_N+lagged_Hc+lagged_K + assets + M3 + credit_other_GDP + stock_market +lagged_pl_c + lagged_debt + lagged_urb ,index = c("country", "year"), model = "within", effect = "twoways", data=dr.p)
d<-coeftest(Final_Model, vcov=vcovHC(Final_Model,cluster = "group"))


stargazer(a,b,c,d, 
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          type="text")

#without controls
stargazer(coef1, coefpc,a, b,d, 
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          type="text")





stargazer(coef1, coefind, pippo1,coef4, coefp, coefpc,a, b,d, 
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=TRUE,
          model.names=TRUE,
          column.labels=c("BR","BRDummy","CBR","CBRDummy","PBR","CPBR","IVBR","CIVBR","Final Model"),
          type="text")

rob_se <- list(sqrt(diag(vcovHC(r1.0, type = "HC1"))),
               sqrt(diag(vcovHC(r2.0, type = "HC1"))),
               sqrt(diag(vcovHC(r3.0 , type = "HC1"))),
               sqrt(diag(vcovHC(r6.0, type = "HC1"))),
               sqrt(diag(vcovHC(r4.0 , type = "HC1"))),
               sqrt(diag(vcovHC(r5.0, type = "HC1"))))
stargazer(r1.0 , r2.0 , r3.0 ,r6.0 , r4.0 , r5.0, 
          type="text",
          se=rob_se,
          model.names=FALSE,
          column.labels=c("OLS","FE", "FE - IV", "IV - no FE", "First stage Lethrate","First stage GDP"))



rob_se <- list(sqrt(diag(vcovHC(Solow_OLS))),
               sqrt(diag(vcovHC(Solow_OLS_Dummy_dip))),
               sqrt(diag(vcovHC(Solow_OLS_controlli1 ))),
               sqrt(diag(vcovHC(Solow_OLS_controlli_Dummy))),
               sqrt(diag(vcovHC(Solow_Panel ))),
               sqrt(diag(vcovHC(Solow_Panel_controlli))),
               sqrt(diag(vcovHC(IV_Solow))),
               sqrt(diag(vcovHC(IV_Solow_controlli))),
               sqrt(diag(vcovHC(Final_Model))))

prov<-list(sqrt(diag(vcovHC(Solow_OLS))),
           sqrt(diag(vcovHC(Solow_OLS_Dummy_dip))),
           sqrt(diag(vcovHC(Solow_OLS_controlli1 ))))

prova<-list(sqrt(diag(vcovHC(Solow_OLS_controlli_Dummy))),
            sqrt(diag(vcovHC(Solow_Panel ))),
            sqrt(diag(vcovHC(Solow_Panel_controlli))))

prova2<-list(sqrt(diag(vcovHC(IV_Solow))),
             sqrt(diag(vcovHC(IV_Solow_controlli))),
             sqrt(diag(vcovHC(Final_Model))))

               
stargazer(r1,r2,r3,r4,r5,r6,r7,r8,r9, 
          type="text", 
          se=rob_se,
          model.names=TRUE,
          column.labels=c("OLS","FE", "FE - IV", "IV - no FE", "First stage Lethrate","First stage GDP")
)

stargazer(r1,r2,r3,r4,r5,r6,r7,r8,r9, 
          se=rob_se, type="text",
          model.names=TRUE,
          column.labels=c("BR","BRDummy", "CBR", "CBRDummy","PBR", "CPBR","IVBR","CIVBR", "Final Model")
)

stargazer(r1,r2,r3,r4,r5,r6,r7,r8,r9, 
          se=rob_se, 
          model.names=TRUE,
          column.labels=c("BR","BRDummy", "CBR", "CBRDummy","PBR", "CPBR","IVBR","CIVBR", "Final Model")
)



stargazer(r1,r2,r3, 
          se=prov,
          model.names=TRUE,
          column.labels=c("BR","BRDummy", "CBR"))

stargazer(r4,r5,r6, 
          se=prova, 
          model.names=FALSE,
          column.labels=c("CBRDummy","PBR", "CPBR")
          )


stargazer(r7,r8,r9, 
          se=prova2,
          model.names=TRUE,
          column.labels=c("IVBR","CIVBR", "Final Model"))


stargazer(coef1, coefind, pippo1,coef4,
          header=FALSE, 
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,
          column.labels=c("OLS","IV"), type="text")






linearHypothesis(Final_Model, c("log(lagged_y)=0"," log(n + 0, 5)=0","log(sk)=0","log(sh)=0","credit_GDP=0","assets=0","M3=0","credit_other_GDP=0","stock_market=0","lagged_pl_c=0","lagged_debt.to.GDP=0","lagged_urb.to.GDP=0"), vcov. = vcovHC)


firstStage1 <- lm(sk ~ lagged_y+lagged_N+lagged_Hc+lagged_K+  assets  + stock_market +lagged_pl_c + lagged_debt + lagged_urb , data = dr)
fs1<-coeftest(firstStage1, vcov.  = vcovHC)

star1<-linearHypothesis(firstStage1, c("lagged_N=0","lagged_Hc=0","lagged_K=0"), vcov. = vcovHC)



firstStage2 <- lm(sh ~ lagged_y+lagged_N+lagged_Hc+lagged_K+  assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb , data = dr)
fs2<-coeftest(firstStage2, vcov.  = vcovHC)

star2<-linearHypothesis(firstStage2, c("lagged_N=0","lagged_Hc=0","lagged_K=0"), vcov. = vcovHC)

firstStage3 <- lm(n ~ lagged_y+lagged_N+lagged_Hc+lagged_K+ assets + stock_market +lagged_pl_c + lagged_debt + lagged_urb , data = dr)
fs3<-coeftest(firstStage3, vcov.  = vcovHC)

star3<-linearHypothesis(firstStage3, c("lagged_N=0","lagged_Hc=0","lagged_K=0"), vcov. = vcovHC)

stargazer(fs1, fs2, fs3,
          header=FALSE, title="First Stages",
          keep.stat="n",digits=4, single.row=FALSE,
          intercept.bottom=FALSE,
          model.names=FALSE,column.labels=c("sk","sh","n")
          )

stargazer(star1, star2, star3,
          header=FALSE, type="text",column.labels=c("sk","sh","n"))


regression<- plm(growth_rate ~ log(lagged_y)+ log(n+0,05)+ log(sk)+log(sh) + credit_GDP  +lagged_pl_c + lagged_debt.to.GDP   |lagged_y+lagged_N+lagged_Hc+lagged_K+  M3  + stock_market +lagged_pl_c + lagged_debt.to.GDP  ,index = c("country", "year"), model = "within", effect = "twoways", data=dr.p)

coeftest(regression, vcov. = vcovHC)
summary(regression)
