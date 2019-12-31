#--------
#----Afrodatae wave 1-7を読み込んだデータを使って各種処理
#----2019/12/10 hamaoka@fbc.keio.ac.jp
#--------
#Rについては例えば　濱岡の下記ページ参照
#　http://news.fbc.keio.ac.jp/~hamaoka/cgi-bin/fswiki/wiki.cgi?page=R　


#install.packages(c("dplyr","haven","memisc","biglm"))	#としてインストールしておく
options(width=150)
library(dplyr)	#国別集計用
library(haven)	#SPSSデータ読み込み
library(memisc)	#lmの出力整形
library(biglm)	#bigdata lm,glm

#ファイルのあるディレクトリ指定　　自分のAfroデータのあるところに変更  下記を書き換えるか､Rのメニューで指定
setwd("/Users/yh/Dropbox/Files2019/研究2019/AfroData")
#save.image("0Afrodat.img")

load("0Afrodat.img");ls()

#Afrodat1-Afrodat7　やそれらの共通の変数をまとめたAfrodatAllなどが入っている

#データ
#http://afrobarometer.org/data/merged-data
#http://afrobarometer.org/data/merged-round-6-data-36-countries-2016
#Merged Round 6 data (36 countries) (2016) (last updated: 2016)

# Round7
#https://www.afrobarometer.org/data/merged-round-7-data-34-countries-2019
#　code bookは未公開



repNA01<-function(x){
	x[x<0|x>1]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA02<-function(x){
	x[x<0|x>2]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA03<-function(x){
	x[x<0|x>3]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
#0-4　以外をNAに　
repNA04<-function(x){
	x[x<0|x>4]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}
repNA05<-function(x){
	x[x<0|x>5]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}

#0-1　以外をNAに　
repNA01<-function(x){
	x[x<0|x>1]<-NA
	print(table(x,exclude=NULL))
	return(x)
	}

#NAを除いて平均計算
mean2<-function(x){
	m<-mean(x,na.rm=T)
	return(m)
	}

max2<-function(x){
	m<-max(x,na.rm=T)
	return(m)
	}

min2<-function(x){
	m<-min(x,na.rm=T)
	return(m)
	}

#-------------------------------
dwaw.line2<-function(dat){		#datの中にあるtを横軸､yを縦軸にとってプロット｡左端に国記号　dat$t:year, dat$y, dat$cnam: country name
	d<-dim(dat)[1]
	lines(dat$t,dat$y)
		text(dat$t[d],dat$y[d],dat$cnam[d],pos=4,cex=0.5)		#最後のtのところにcountry nameを
	}

#datの中にあるcnam毎にトレンドをプロット
group_trend_plot<-function(dat,lab){
par(mfrow=c(1,1))
	ymin<-min2(dat$y)
	ymax<-max2(dat$y)
	tmax<-max2(dat$t)
	tmin<-min2(dat$t)
	plot(dat$t,dat$y,xlim=c(tmin,tmax),ylim=c(ymin,ymax),main=lab,xlab="Year",ylab="Mean")
		by(dat,dat$cnam,dwaw.line2)
	}

head(m)
#Radio
#dat<-m[,c(1:3)];names(dat)<-c("cnam","t","y")
#	group_trend_plot(dat,lab="Radio")


#-------Round 6で遊ぶ
#国別に集計してみる
Afrodat6g<-group_by(Afrodat6,Afrodat6$COUNTRY2)

m<-summarise(Afrodat6g,mNews_Radio=mean2(News_Radio),mNews_Television=mean2(News_Television),mNews_Newspaper=mean2(News_Newspaper),mNews_Internet=mean2(News_Internet),mNews_Social_media=mean2(News_Social_media),
mOwn_Radio=mean2(Own_Radio), mOwn_TV=mean2(Own_TV), mOwn_Auto=mean2(Own_Auto), mOwn_Mbphone=mean2(Own_Mbphone))
as.data.frame(m)
#edit(as.data.frame(m))


#"dlang_English"                   "dlang_French"                    "dlang_Portuguese"               "dlang_Swahili"                   "dlang_Arabic"                    "dlang_Afrikaans"                 "dlang_Chichewa"                 "dlang_Akan"                      "dlang_Other"                     "dlang_Egyptian_Arabic"           "dlang_Crioulo"                   "dlang_Kirund"                    "dlang_Sesotho"                   "dlang_Sudanese_Arabic"           "dlang_Creole"                   "dlang_siSwati"                   "dlang_Shona"                     "dlang_Algerian_Arabic" 

#"dOccupation_Never"               "dOccupation_Student"            "dOccupation_Housewife_homemaker" "dOccupation_primary"             "dOccupation_Trader"              "dOccupation_Retail"              "dOccupation_Unskilled"           "dOccupation_skilled"             "dOccupation_Clerical"            "dOccupation_Supervisor"         "dOccupation_police"              "dOccupation_Mid_level"           "dOccupation_Upper_level"         "dOccupation_Other" 

#"dRace_BAf"                       "dRace_Wh"                        "dRace_Col"                       "dRace_Arab"                      "dRace_SAs"                       "dRace_EAs"                       "dRace_Oth"                      


#big
res<-glm(Own_TV~Age+Gender_f+Education+as.factor(Employment_status)+as.factor(Race)+as.factor(Occupation)+Mem_religious+Mem_voluntary+gone_food+gone_cash+as.factor(Language)+COUNTRY2,family="binomial",data=Afrodat6)
	summary(res)


res<-glm(Own_TV~Age+Gender_f+Education+as.factor(Employment_status)+as.factor(Race)+as.factor(Occupation)+Mem_religious+Mem_voluntary+gone_food+gone_cash+as.factor(Language)+COUNTRY2,family="binomial",data=Afrodat6)
	summary(res)




#---------------
#			7
#---------------
Afrodat7 <- read_sav("r7_merged_data_34ctry.release.sav")
#View(Afrodat7)
summary(Afrodat7)
names(Afrodat7)	
dim(Afrodat7)	#[1] 53935   364
	Afrodat7$wave<-7
	Afrodat7$year<-2018

table(Afrodat7$COUNTRY)
Afrodat7$COUNTRY2<-substr(Afrodat7$RESPNO,1,3)	#はじめの3文字が国名
	table(Afrodat7$COUNTRY2)
# BEN  BFO  BOT  CAM  CDI  CVE  GAB  GAM  GHA  GUI  KEN  LES  LIB  MAD  MAU  MLI  MLW  MOR  MOZ  NAM  NGR  NIG  SAF  SEN  SRL  STP  SUD  SWZ  TAN  TOG  TUN  UGA  ZAM  ZIM 
#1200 1200 1198 1202 1200 1200 1199 1200 2400 1194 1599 1200 1200 1200 1200 1200 1200 1200 2392 1200 1200 1600 1840 1200 1200 1200 1200 1200 2400 1200 1199 1200 1200 1200 


#2019/11/現在　codebookがないので､ボツワナの調査票を参照　https://www.afrobarometer.org/sites/default/files/questionnaires/Round%207/bot_r7_questionnaire_062018.pdf
#これを使って欠損値処理
#12. How often do you get news from the following sources? 
#A.Radio
#B. Television
#C. Newspapers
#D.Internet
#E. Social media such as Facebook or Twitter

Afrodat7$News_Radio<-repNA04(Afrodat7$Q12A)
Afrodat7$News_Television<-repNA04(Afrodat7$Q12B)
Afrodat7$News_Newspaper<-repNA04(Afrodat7$Q12C)
Afrodat7$News_Internet<-repNA04(Afrodat7$Q12D)
Afrodat7$News_Social_media<-repNA04(Afrodat7$Q12E)

#保存
save(Afrodat7,file=“0Afrodat7.rda”)







#------全ラウンド必要部分をまとめる　　とりあえずニュースの利用
v<-c("wave","year","COUNTRY2","News_Radio","News_Television","News_Newspaper","News_Internet","News_Social_media")
AfrodatAll<-rbind(Afrodat1[,v],Afrodat2[,v],Afrodat3[,v],Afrodat4[,v],Afrodat5[,v],Afrodat6[,v],Afrodat7[,v])
	dim(AfrodatAll)	#[1] 204464      8

table(AfrodatAll$COUNTRY2,AfrodatAll$wave,exclude=NULL)
#AfrodatAllg<-group_by(AfrodatAll,c("year","COUNTRY2"))
AfrodatAllg<-group_by(AfrodatAll, AfrodatAll$COUNTRY2,AfrodatAll$year)	#国､年別に集計することを指定

(m<-summarise(AfrodatAllg,mNews_Radio<-mean2(News_Radio),mNews_Television<-mean2(News_Television),mNews_Newspaper<-mean2(News_Newspaper),mNews_Internet<-mean2(News_Internet),mNews_Social_media<-mean2(News_Social_media)))

edit(m)


#-------------------------------
dwaw.line2<-function(dat){		#datの中にあるtを横軸､yを縦軸にとってプロット｡左端に国記号　dat$t:year, dat$y, dat$cnam: country name
	d<-dim(dat)[1]
	lines(dat$t,dat$y)
		text(dat$t[d],dat$y[d],dat$cnam[d],pos=4,cex=0.5)		#最後のtのところにcountry nameを
	}

#datの中にあるcnam毎にトレンドをプロット
group_trend_plot<-function(dat,lab){
par(mfrow=c(1,1))
	ymin<-min2(dat$y)
	ymax<-max2(dat$y)
	tmax<-max2(dat$t)
	tmin<-min2(dat$t)
	plot(dat$t,dat$y,xlim=c(tmin,tmax),ylim=c(ymin,ymax),main=lab,xlab="Year",ylab="Mean")
		by(dat,dat$cnam,dwaw.line2)
	}

head(m)
#Radio
dat<-m[,c(1:3)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Radio")
dat<-m[,c(1:2,4)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="TV")

dat<-m[,c(1:2,5)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Newspaper")

dat<-m[,c(1:2,6)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Internet")

dat<-m[,c(1:2,7)];names(dat)<-c("cnam","t","y")
	group_trend_plot(dat,lab="Social_media ")



save.image("0Afrodat.img")



#---------------
# データ・コーディング_go 2019.12.31
#---------------

#----国
#----Afrodat5
table(Afrodat5$COUNTRY)
Afrodat5$COUNTRY2<-substr(Afrodat5$RESPNO,1,3)    #はじめの3文字が国名
table(Afrodat5$COUNTRY2)

Afrodat5$dCOUNTRY_ALG<-ifelse(Afrodat5$COUNTRY2=="ALG",1,0)    #すべてについてダミーを定義したので使うときはどれかを除く
Afrodat5$dCOUNTRY_BDI<-ifelse(Afrodat5$COUNTRY2=="BDI",1,0)
Afrodat5$dCOUNTRY_BEN<-ifelse(Afrodat5$COUNTRY2=="BEN",1,0) 　　
Afrodat5$dCOUNTRY_BFO<-ifelse(Afrodat5$COUNTRY2=="BFO",1,0)
Afrodat5$dCOUNTRY_CAM<-ifelse(Afrodat5$COUNTRY2=="CAM",1,0)
Afrodat5$dCOUNTRY_CDI<-ifelse(Afrodat5$COUNTRY2=="CDI",1,0)
Afrodat5$dCOUNTRY_CVE<-ifelse(Afrodat5$COUNTRY2=="CVE",1,0)　　
Afrodat5$dCOUNTRY_EGY<-ifelse(Afrodat5$COUNTRY2=="EGY",1,0)
Afrodat5$dCOUNTRY_GHA<-ifelse(Afrodat5$COUNTRY2=="GHA",1,0)
Afrodat5$dCOUNTRY_GUI<-ifelse(Afrodat5$COUNTRY2=="GUI",1,0)
Afrodat5$dCOUNTRY_KEN<-ifelse(Afrodat5$COUNTRY2=="KEN",1,0)
Afrodat5$dCOUNTRY_LES<-ifelse(Afrodat5$COUNTRY2=="LES",1,0)
Afrodat5$dCOUNTRY_LIB<-ifelse(Afrodat5$COUNTRY2=="LIB",1,0)
Afrodat5$dCOUNTRY_MAD<-ifelse(Afrodat5$COUNTRY2=="MAD",1,0)
Afrodat5$dCOUNTRY_MAU<-ifelse(Afrodat5$COUNTRY2=="MAU",1,0)
Afrodat5$dCOUNTRY_MLI<-ifelse(Afrodat5$COUNTRY2=="MLI",1,0)
Afrodat5$dCOUNTRY_MLW<-ifelse(Afrodat5$COUNTRY2=="MLW",1,0)
Afrodat5$dCOUNTRY_MOR<-ifelse(Afrodat5$COUNTRY2=="MOR",1,0)
Afrodat5$dCOUNTRY_MOZ<-ifelse(Afrodat5$COUNTRY2=="MOZ",1,0)
Afrodat5$dCOUNTRY_NAM<-ifelse(Afrodat5$COUNTRY2=="NAM",1,0)
Afrodat5$dCOUNTRY_NGR<-ifelse(Afrodat5$COUNTRY2=="NGR",1,0)
Afrodat5$dCOUNTRY_NIG<-ifelse(Afrodat5$COUNTRY2=="NIG",1,0)
Afrodat5$dCOUNTRY_SAF<-ifelse(Afrodat5$COUNTRY2=="SAF",1,0)
Afrodat5$dCOUNTRY_SEN<-ifelse(Afrodat5$COUNTRY2=="SEN",1,0)
Afrodat5$dCOUNTRY_SRL<-ifelse(Afrodat5$COUNTRY2=="SRL",1,0)
Afrodat5$dCOUNTRY_STP<-ifelse(Afrodat5$COUNTRY2=="STP",1,0)
Afrodat5$dCOUNTRY_SUD<-ifelse(Afrodat5$COUNTRY2=="SUD",1,0)
Afrodat5$dCOUNTRY_SWZ<-ifelse(Afrodat5$COUNTRY2=="SWZ",1,0)
Afrodat5$dCOUNTRY_TAN<-ifelse(Afrodat5$COUNTRY2=="TAN",1,0)
Afrodat5$dCOUNTRY_TOG<-ifelse(Afrodat5$COUNTRY2=="TOG",1,0)
Afrodat5$dCOUNTRY_TUN<-ifelse(Afrodat5$COUNTRY2=="TUN",1,0)
Afrodat5$dCOUNTRY_UGA<-ifelse(Afrodat5$COUNTRY2=="UGA",1,0)
Afrodat5$dCOUNTRY_ZAM<-ifelse(Afrodat5$COUNTRY2=="ZAM",1,0)
Afrodat5$dCOUNTRY_ZIM<-ifelse(Afrodat5$COUNTRY2=="ZIM",1,0)
#34 African countries

#----Afrodat4
table(Afrodat4$COUNTRY)
Afrodat4$COUNTRY2<-substr(Afrodat4$RESPNO,1,3)    #はじめの3文字が国名
table(Afrodat4$COUNTRY2)

Afrodat4$dCOUNTRY_BEN<-ifelse(Afrodat4$COUNTRY2=="BEN",1,0)
Afrodat4$dCOUNTRY_BFO<-ifelse(Afrodat4$COUNTRY2=="BFO",1,0)
Afrodat4$dCOUNTRY_BOT<-ifelse(Afrodat4$COUNTRY2=="BOT",1,0)
Afrodat4$dCOUNTRY_CVE<-ifelse(Afrodat4$COUNTRY2=="CVE",1,0)
Afrodat4$dCOUNTRY_GHA<-ifelse(Afrodat4$COUNTRY2=="GHA",1,0)
Afrodat4$dCOUNTRY_KEN<-ifelse(Afrodat4$COUNTRY2=="KEN",1,0)
Afrodat4$dCOUNTRY_LES<-ifelse(Afrodat4$COUNTRY2=="LES",1,0)
Afrodat4$dCOUNTRY_LIB<-ifelse(Afrodat4$COUNTRY2=="LIB"|Afrodat4$COUNTRY2=="LIb",1,0)
Afrodat4$dCOUNTRY_MAD<-ifelse(Afrodat4$COUNTRY2=="MAD",1,0)
Afrodat4$dCOUNTRY_MLI<-ifelse(Afrodat4$COUNTRY2=="MLI",1,0)
Afrodat4$dCOUNTRY_MLW<-ifelse(Afrodat4$COUNTRY2=="MLW",1,0)
Afrodat4$dCOUNTRY_MOZ<-ifelse(Afrodat4$COUNTRY2=="MOZ",1,0)
Afrodat4$dCOUNTRY_NAM<-ifelse(Afrodat4$COUNTRY2=="NAM",1,0)
Afrodat4$dCOUNTRY_NIG<-ifelse(Afrodat4$COUNTRY2=="NIG",1,0)
Afrodat4$dCOUNTRY_SAF<-ifelse(Afrodat4$COUNTRY2=="SAF",1,0)
Afrodat4$dCOUNTRY_SEN<-ifelse(Afrodat4$COUNTRY2=="SEN",1,0)
Afrodat4$dCOUNTRY_TAN<-ifelse(Afrodat4$COUNTRY2=="TAN",1,0)
Afrodat4$dCOUNTRY_UGA<-ifelse(Afrodat4$COUNTRY2=="UGA",1,0)
Afrodat4$dCOUNTRY_ZAM<-ifelse(Afrodat4$COUNTRY2=="ZAM",1,0)
Afrodat4$dCOUNTRY_ZIM<-ifelse(Afrodat4$COUNTRY2=="ZIM",1,0)
#20 African Countries
#Benin, Botswana, Burkina Faso, Cape Verde, Ghana, Kenya, Lesotho, Liberia, Madagascar, Malawi, Mali, 
#Mozambique, Namibia, Nigeria, Senegal, South Africa, Tanzania, Uganda, Zambia, Zimbabwe)

#----Afrodat3
table(Afrodat3$COUNTRY)
Afrodat3$COUNTRY2<-substr(Afrodat3$RESPNO,1,3)    #はじめの3文字が国名
table(Afrodat3$COUNTRY2)

Afrodat3$dCOUNTRY_BEN<-ifelse(Afrodat3$COUNTRY2=="BEN",1,0)
Afrodat3$dCOUNTRY_BOT<-ifelse(Afrodat3$COUNTRY2=="BOT",1,0)
Afrodat3$dCOUNTRY_CVE<-ifelse(Afrodat3$COUNTRY2=="CVE",1,0)
Afrodat3$dCOUNTRY_GHA<-ifelse(Afrodat3$COUNTRY2=="GHA",1,0)
Afrodat3$dCOUNTRY_KEN<-ifelse(Afrodat3$COUNTRY2=="KEN",1,0)
Afrodat3$dCOUNTRY_LES<-ifelse(Afrodat3$COUNTRY2=="LES",1,0)
Afrodat3$dCOUNTRY_MAD<-ifelse(Afrodat3$COUNTRY2=="MAD",1,0)
Afrodat3$dCOUNTRY_MLI<-ifelse(Afrodat3$COUNTRY2=="MLI",1,0)
Afrodat3$dCOUNTRY_MOZ<-ifelse(Afrodat3$COUNTRY2=="MOZ",1,0)
Afrodat3$dCOUNTRY_MWI<-ifelse(Afrodat3$COUNTRY2=="MWI",1,0)
Afrodat3$dCOUNTRY_NAM<-ifelse(Afrodat3$COUNTRY2=="NAM",1,0)
Afrodat3$dCOUNTRY_NIG<-ifelse(Afrodat3$COUNTRY2=="NIG",1,0)
Afrodat3$dCOUNTRY_SAF<-ifelse(Afrodat3$COUNTRY2=="SAF",1,0)
Afrodat3$dCOUNTRY_SEN<-ifelse(Afrodat3$COUNTRY2=="SEN",1,0)
Afrodat3$dCOUNTRY_TAN<-ifelse(Afrodat3$COUNTRY2=="TAN",1,0)
Afrodat3$dCOUNTRY_UGA<-ifelse(Afrodat3$COUNTRY2=="UGA",1,0)
Afrodat3$dCOUNTRY_ZAM<-ifelse(Afrodat3$COUNTRY2=="ZAM",1,0)
Afrodat3$dCOUNTRY_ZIM<-ifelse(Afrodat3$COUNTRY2=="ZIM",1,0)
#18 African Countries
#(Benin, Botswana, Cape Verde, Ghana, Kenya, Lesotho, Madagascar, Malawi, Mali, Mozambique,
#Namibia, Nigeria, Senegal, South Africa, Tanzania, Uganda, Zambia, Zimbabwe)


#----Afrodat2
table(Afrodat2$COUNTRY)
Afrodat2$COUNTRY2<-substr(Afrodat2$RESPNO,1,3)    #はじめの3文字が国名
table(Afrodat2$COUNTRY2)

Afrodat2$dCOUNTRY_BOT<-ifelse(Afrodat2$COUNTRY2=="BOT",1,0)
Afrodat2$dCOUNTRY_CVE<-ifelse(Afrodat2$COUNTRY2=="CVE"|Afrodat2$COUNTRY2=="cve",1,0)
Afrodat2$dCOUNTRY_GHA<-ifelse(Afrodat2$COUNTRY2=="GHA",1,0)
Afrodat2$dCOUNTRY_KEN<-ifelse(Afrodat2$COUNTRY2=="KEN",1,0)
Afrodat2$dCOUNTRY_LES<-ifelse(Afrodat2$COUNTRY2=="LES",1,0)
Afrodat2$dCOUNTRY_MLI<-ifelse(Afrodat2$COUNTRY2=="MLI",1,0)
Afrodat2$dCOUNTRY_MOZ<-ifelse(Afrodat2$COUNTRY2=="MOZ",1,0)
Afrodat2$dCOUNTRY_MWI<-ifelse(Afrodat2$COUNTRY2=="MWI",1,0)
Afrodat2$dCOUNTRY_NAM<-ifelse(Afrodat2$COUNTRY2=="NAM",1,0)
Afrodat2$dCOUNTRY_NIG<-ifelse(Afrodat2$COUNTRY2=="NIG",1,0)
Afrodat2$dCOUNTRY_SAF<-ifelse(Afrodat2$COUNTRY2=="SAF",1,0)
Afrodat2$dCOUNTRY_SEN<-ifelse(Afrodat2$COUNTRY2=="SEN",1,0)
Afrodat2$dCOUNTRY_TAN<-ifelse(Afrodat2$COUNTRY2=="TAN",1,0)
Afrodat2$dCOUNTRY_UGA<-ifelse(Afrodat2$COUNTRY2=="UGA",1,0)
Afrodat2$dCOUNTRY_ZAM<-ifelse(Afrodat2$COUNTRY2=="ZAM",1,0)
Afrodat2$dCOUNTRY_ZIM<-ifelse(Afrodat2$COUNTRY2=="ZIM",1,0)

#16 African Countries
#1=Botswana; 2=Ghana; 3=Lesotho; 4=Malawi; 5=Mali; 6=Namibia; 7=Nigeria; 8=South Africa;
#9=Tanzania; 10=Uganda; 11=Zambia; 12=Zimbabwe; 13=Cape Verde; 14=Kenya; 15=Mozambique; 16=Senegal

#----Afrodat1
table(Afrodat1$COUNTRY2)
Afrodat1$dCOUNTRY_BOT<-ifelse(Afrodat1$COUNTRY2=="BOT",1,0)
Afrodat1$dCOUNTRY_GHA<-ifelse(Afrodat1$COUNTRY2=="GHA",1,0)
Afrodat1$dCOUNTRY_LES<-ifelse(Afrodat1$COUNTRY2=="LES",1,0)
Afrodat1$dCOUNTRY_MLI<-ifelse(Afrodat1$COUNTRY2=="MLI",1,0)
Afrodat1$dCOUNTRY_MLW<-ifelse(Afrodat1$COUNTRY2=="MLW",1,0)
Afrodat1$dCOUNTRY_NAM<-ifelse(Afrodat1$COUNTRY2=="NAM",1,0)
Afrodat1$dCOUNTRY_NIG<-ifelse(Afrodat1$COUNTRY2=="NIG",1,0)
Afrodat1$dCOUNTRY_SAF<-ifelse(Afrodat1$COUNTRY2=="SAF",1,0)
Afrodat1$dCOUNTRY_TAN<-ifelse(Afrodat1$COUNTRY2=="TAN",1,0)
Afrodat1$dCOUNTRY_UGA<-ifelse(Afrodat1$COUNTRY2=="UGA",1,0)
Afrodat1$dCOUNTRY_ZAM<-ifelse(Afrodat1$COUNTRY2=="ZAM",1,0)
Afrodat1$dCOUNTRY_ZIM<-ifelse(Afrodat1$COUNTRY2=="ZIM",1,0)
#Botswana, Ghana, Lesotho, Malawi, Mali, Namibia, Nigeria, South Africa, Tanzania, Uganda, Zambia and Zimbabwe Äb0


#----年齢
#Question Number: Q1
#Question: How old are you?
#Variable Label: Q1. Age
#Values: 18-105, 998-999, -1
#Value Labels: 98=Refused to answer, 999=Don’t know, -1=Missing
Afrodat6$Age<-ifelse(Afrodat6$Q1<0|Afrodat6$Q1>105|Afrodat6$Q1==98,NA,Afrodat6$Q1)
table(Afrodat6$Age,exclude=NULL)

Afrodat5$Age<-ifelse(Afrodat5$Q1<0|Afrodat5$Q1>105|Afrodat5$Q1==98,NA,Afrodat5$Q1)
table(Afrodat5$Age,exclude=NULL)

Afrodat4$Age<-ifelse(Afrodat4$Q1<0|Afrodat4$Q1>105|Afrodat4$Q1==98,NA,Afrodat4$Q1)
table(Afrodat4$Age,exclude=NULL)

Afrodat3$Age<-ifelse(Afrodat3$q1<0|Afrodat3$q1>105|Afrodat3$q1==98,NA,Afrodat3$q1)
table(Afrodat3$Age,exclude=NULL)

Afrodat2$Age<-ifelse(Afrodat2$q80<0|Afrodat2$q80>105|Afrodat2$q80==98,NA,Afrodat2$q80)
table(Afrodat2$Age,exclude=NULL)

Afrodat1$Age<-ifelse(Afrodat1$age<0|Afrodat1$age>105|Afrodat1$age==98,NA,Afrodat1$age)
table(Afrodat1$Age,exclude=NULL)

#----言語　
#----Afrodat5
Afrodat5$Language<-ifelse(Afrodat5$Q2<0|Afrodat5$Q2>=9998,NA,Afrodat5$Q2)
table(Afrodat5$Language,exclude=NULL)
d5<-data.frame(table(Afrodat5$Language,exclude=NULL))
d5[order(d5[,2],decreasing=T),]

Afrodat5$dlang_English<-ifelse(Afrodat5$Language==1,1,0)
Afrodat5$dlang_French<-ifelse(Afrodat5$Language==2,1,0)
Afrodat5$dlang_Portuguese<-ifelse(Afrodat5$Language==3,1,0)
Afrodat5$dlang_Swahili<-ifelse(Afrodat5$Language==4,1,0)
Afrodat5$dlang_Arabic<-ifelse(Afrodat5$Language==5,1,0)
Afrodat5$dlang_Swahili<-ifelse(Afrodat5$Language==6,1,0)
Afrodat5$dlang_Afrikaans<-ifelse(Afrodat5$Language==7,1,0)

#Afrodat5$dlang_Chichewa<-ifelse(Afrodat5$Language==463,1,0)
Afrodat5$dlang_Akan<-ifelse(Afrodat5$Language==260,1,0)
Afrodat5$dlang_Other<-ifelse(Afrodat5$Language==9995,1,0)
#Afrodat5$dlang_Egyptian_Arabic<-ifelse(Afrodat5$Language==1460,1,0)
Afrodat5$dlang_Crioulo<-ifelse(Afrodat5$Language==220,1,0)
Afrodat5$dlang_Kirund<-ifelse(Afrodat5$Language==1180,1,0)
Afrodat5$dlang_Sesotho<-ifelse(Afrodat5$Language==340,1,0)
Afrodat5$dlang_Sudanese_Arabic<-ifelse(Afrodat5$Language==1540,1,0)
Afrodat5$dlang_Creole<-ifelse(Afrodat5$Language==900,1,0)
Afrodat5$dlang_siSwati<-ifelse(Afrodat5$Language==1620,1,0)
Afrodat5$dlang_Shona<-ifelse(Afrodat5$Language==861,1,0)
Afrodat5$dlang_Algerian_Arabic<-ifelse(Afrodat5$Language==1420,1,0)
#----その他上位
Afrodat5$dlang_Akan<-ifelse(Afrodat5$Language==260,1,0)
Afrodat5$dlang_siSwati<-ifelse(Afrodat5$Language==35,1,0)
Afrodat5$dlang_Crioulo<-ifelse(Afrodat5$Language==220,1,0)
Afrodat5$dlang_Kirund<-ifelse(Afrodat5$Language==1180,1,0)
Afrodat5$dlang_Sesotho<-ifelse(Afrodat5$Language==340,1,0)
Afrodat5$dlang_Creole<-ifelse(Afrodat5$Language==900,1,0)
Afrodat5$dlang_Shona<-ifelse(Afrodat5$Language==861,1,0)
Afrodat5$dlang_Hausa<-ifelse(Afrodat5$Language==17,1,0)
Afrodat5$dlang_Malagasy<-ifelse(Afrodat5$Language==421,1,0)
Afrodat5$dlang_Yoruba<-ifelse(Afrodat5$Language==39,1,0)
Afrodat5$dlang_Wolof<-ifelse(Afrodat5$Language==660,1,0)

#----Afrodat4
Afrodat4$Language<-ifelse(Afrodat4$Q3<0|Afrodat4$Q3>=9998,NA,Afrodat4$Q3)
table(Afrodat4$Language,exclude=NULL)
d4<-data.frame(table(Afrodat4$Language,exclude=NULL))
d4[order(d4[,2],decreasing=T),]

Afrodat4$dlang_English<-ifelse(Afrodat4$Language==1,1,0)
Afrodat4$dlang_French<-ifelse(Afrodat4$Language==2,1,0)
Afrodat4$dlang_Portuguese<-ifelse(Afrodat4$Language==3,1,0)
Afrodat4$dlang_Swahili<-ifelse(Afrodat4$Language==4,1,0)
Afrodat4$dlang_Arabic<-ifelse(Afrodat4$Language==5,1,0)
Afrodat4$dlang_Swahili<-ifelse(Afrodat4$Language==6,1,0)
Afrodat4$dlang_Afrikaans<-ifelse(Afrodat4$Language==7,1,0)

Afrodat4$dlang_Chichewa<-ifelse(Afrodat4$Language==463,1,0)
Afrodat4$dlang_Akan<-ifelse(Afrodat4$Language==260,1,0)
Afrodat4$dlang_Other<-ifelse(Afrodat4$Language==9995,1,0)
Afrodat4$dlang_Egyptian_Arabic<-ifelse(Afrodat4$Language==1460,1,0)
Afrodat4$dlang_Crioulo<-ifelse(Afrodat4$Language==220,1,0)
Afrodat4$dlang_Kirund<-ifelse(Afrodat4$Language==1180,1,0)
Afrodat4$dlang_Sesotho<-ifelse(Afrodat4$Language==340,1,0)
Afrodat4$dlang_Sudanese_Arabic<-ifelse(Afrodat4$Language==1540,1,0)
Afrodat4$dlang_Creole<-ifelse(Afrodat4$Language==900,1,0)
Afrodat4$dlang_siSwati<-ifelse(Afrodat4$Language==1620,1,0)
Afrodat4$dlang_Shona<-ifelse(Afrodat4$Language==861,1,0)
Afrodat4$dlang_Algerian_Arabic<-ifelse(Afrodat4$Language==1420,1,0)
#----その他上位
Afrodat4$dlang_Shona<-ifelse(Afrodat4$Language==861,1,0)
Afrodat4$dlang_Setswana<-ifelse(Afrodat4$Language==140,1,0)
Afrodat4$dlang_Malagasy<-ifelse(Afrodat4$Language==421,1,0)
Afrodat4$dlang_Wolof<-ifelse(Afrodat4$Language==660,1,0)
Afrodat4$dlang_Mooré<-ifelse(Afrodat4$Language==182,1,0)
Afrodat4$dlang_Oshivambo<-ifelse(Afrodat4$Language==583,1,0)
Afrodat4$dlang_Bambara<-ifelse(Afrodat4$Language==501,1,0)

#----Afrodat3
Afrodat3$Language<-ifelse(Afrodat3$q114<0|Afrodat3$q114>=9998,NA,Afrodat3$q114)
table(Afrodat3$Language,exclude=NULL)
d3<-data.frame(table(Afrodat3$Language,exclude=NULL))
d3[order(d3[,2],decreasing=T),]

Afrodat3$dlang_English<-ifelse(Afrodat3$Language==1,1,0)
Afrodat3$dlang_French<-ifelse(Afrodat3$Language==2,1,0)
Afrodat3$dlang_Portuguese<-ifelse(Afrodat3$Language==3,1,0)
Afrodat3$dlang_Swahili<-ifelse(Afrodat3$Language==4,1,0)
Afrodat3$dlang_Arabic<-ifelse(Afrodat3$Language==5,1,0)
Afrodat3$dlang_Swahili<-ifelse(Afrodat3$Language==6,1,0)
Afrodat3$dlang_Afrikaans<-ifelse(Afrodat3$Language==7,1,0)

#----その他上位
Afrodat3$dlang_Creole<-ifelse(Afrodat3$Language==165,1,0)
Afrodat3$dlang_Malgasy<-ifelse(Afrodat3$Language==240,1,0)
Afrodat3$dlang_Crioulo<-ifelse(Afrodat3$Language==220,1,0)
Afrodat3$dlang_Oshiwambo<-ifelse(Afrodat3$Language==326,1,0)
Afrodat3$dlang_Chewa<-ifelse(Afrodat3$Language==263,1,0)
Afrodat3$dlang_Wolof<-ifelse(Afrodat3$Language==360,1,0)
Afrodat3$dlang_Bambara<-ifelse(Afrodat3$Language==280,1,0)
Afrodat3$dlang_Shona<-ifelse(Afrodat3$Language==441,1,0)
Afrodat3$dlang_Akan<-ifelse(Afrodat3$Language==180,1,0)
Afrodat3$dlang_Xhosa<-ifelse(Afrodat3$Language==102,1,0)
Afrodat3$dlang_Luganda<-ifelse(Afrodat3$Language==417,1,0)
Afrodat3$dlang_Yoruba<-ifelse(Afrodat3$Language==342,1,0)
Afrodat3$dlang_Other<-ifelse(Afrodat3$Language==995,1,0)


#economic condition、living conditions----------
#Afrodat5----------
#$Question Number: Q3A
#Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t know, 998=Refused to answer, -1=Missing
#Source: NDB, Zambia96
Afrodat5$Cond_econ<-repNA05(Afrodat5$Q3A)
Afrodat5$Cond_your_liv<-repNA05(Afrodat5$Q3B)

#Afrodat4----------
#Question Number: Q4A
Afrodat4$Cond_econ<-repNA05(Afrodat4$Q4A)
Afrodat4$Cond_your_liv<-repNA05(Afrodat4$Q4B)

#Afrodat3----------
#Question Number: Q4A
Afrodat3$Cond_econ<-repNA05(Afrodat3$q4a)
Afrodat3$Cond_your_liv<-repNA05(Afrodat3$q4b)

#Afrodat2----------
#Question Number: Q1A
Afrodat2$Cond_econ<-repNA05(Afrodat2$q1a)
Afrodat2$Cond_your_liv<-repNA05(Afrodat2$q1b)

#//Afrodat1----------
#Value Labels: 1=Much less satisfied/Much worse, 2=Slightly less satisfied/worse, 3=About the same,
#4=Slightly more satisfied/Better, 5=Much more satisfied/Much better, 9=Don’t Know, 98=Refused,99=Missing Data 
Afrodat1$Cond_econ<-repNA05(Afrodat1$pfepas)


##living conditions
#Afrodat5----------
#Question Number: Q4
#Value Labels: 1=Much worse, 2=Worse, 3=Same, 4=Better, 5=Much better, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Relative_live<-ifelse(Afrodat5$Q4<0|Afrodat5$Q4>=9,NA,Afrodat5$Q4)
table(Afrodat5$Relative_live,exclude=NULL)        #

#Afrodat4----------
#Question Number: Q5
Afrodat4$Relative_live<-ifelse(Afrodat4$Q5<0|Afrodat4$Q5>=9,NA,Afrodat4$Q5)
table(Afrodat5$Relative_live,exclude=NULL)        #

#Afrodat3----------
#Question Number: Q5
Afrodat3$Relative_live<-ifelse(Afrodat3$q5<0|Afrodat3$q5>=9,NA,Afrodat3$q5)
table(Afrodat3$Relative_live,exclude=NULL)        #

#//Afrodat2----------
#Question Number: Q2B
#Question: In general, how do you rate: Your living conditions compared to those of other {countrymen]?
#Value Labels: 1=Much worse, 2=Worse, 3=Same, 4=Better, 5=Much better, 9=Don’t Know, 98=Refused to
Afrodat2$Relative_live<-ifelse(Afrodat2$q2b<0|Afrodat2$q2b>=9,NA,Afrodat2$q2b)
table(Afrodat2$Relative_live,exclude=NULL)        #

#//Afrodat1----------
#Variable name: pfeerd
#Variable label: Own living conditions compared to others
#Value Labels: 1=Much worse, 2=Worse, 3=About the same, 4=Better, 5=Much better, 9=Don’t Know,98=Refused to Answer, 99=Missing Data
Afrodat1$Relative_live<-ifelse(Afrodat1$pfeerd<0|Afrodat1$pfeerd>=9,NA,Afrodat1$pfeerd)
table(Afrodat1$Relative_live,exclude=NULL)        #


##Without xx
#Afrodat5----------
#Question Number: Q8A
#Value Labels: 0=Never, 1=Just once or twice, 2=Several times, 3=Many times, 4=Always, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$gone_food<-repNA04(Afrodat5$Q8A)
Afrodat5$gone_water<-repNA04(Afrodat5$Q8B)
Afrodat5$gone_med<-repNA04(Afrodat5$Q8C)
Afrodat5$gone_fuel<-repNA04(Afrodat5$Q8D)
Afrodat5$gone_cash<-repNA04(Afrodat5$Q8E)

#Afrodat4----------
Afrodat4$gone_food<-repNA04(Afrodat4$Q8A)
Afrodat4$gone_water<-repNA04(Afrodat4$Q8B)
Afrodat4$gone_med<-repNA04(Afrodat4$Q8C)
Afrodat4$gone_fuel<-repNA04(Afrodat4$Q8D)
Afrodat4$gone_cash<-repNA04(Afrodat4$Q8E)

#Afrodat3----------
Afrodat3$gone_food<-repNA04(Afrodat3$q8a)
Afrodat3$gone_water<-repNA04(Afrodat3$q8b)
Afrodat3$gone_med<-repNA04(Afrodat3$q8c)
Afrodat3$gone_fuel<-repNA04(Afrodat3$q8d)
Afrodat3$gone_cash<-repNA04(Afrodat3$q8e)

#Afrodat2----------
Afrodat2$gone_food<-repNA04(Afrodat2$q9a)
Afrodat2$gone_water<-repNA04(Afrodat2$q9b)
Afrodat2$gone_med<-repNA04(Afrodat2$q9c)
Afrodat2$gone_fuel<-repNA04(Afrodat2$q9e)
Afrodat2$gone_cash<-repNA04(Afrodat2$q9f)
Afrodat2$gone_electricity<-repNA04(Afrodat2$q9d) #R2とR1のみ

#Afrodat1----------
Afrodat1$gone_food<-repNA04(Afrodat1$povfoo)
Afrodat1$gone_water<-repNA04(Afrodat1$povwat) 
Afrodat1$gone_med<-repNA04(Afrodat1$povhth) #Gone without healthcare 
#Afrodat2$gone_fuel<-repNA04(Afrodat2$q8e)
Afrodat1$gone_cash<-repNA04(Afrodat1$povinc)
Afrodat1$gone_electricity<-repNA04(Afrodat1$povelc)　#R2とR1のみ

# news source
#Afrodat5----------　SNSなし
#Question Number: Q13A,C,D
#Question: How often do you get news from the following sources: Radio? 
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 998=Refused to answer, -1=Missing 
Afrodat5$News_Radio<-repNA04(Afrodat5$Q13A)
Afrodat5$News_Television<-repNA04(Afrodat5$Q13B)
Afrodat5$News_Newspaper<-repNA04(Afrodat5$Q13C)
Afrodat5$News_Internet<-repNA04(Afrodat5$Q13D)

#Afrodat4----------　インターネット　SNSなし
#Question Number: Q13A,B,C 
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Every day, 9=Don’t know, 998=Refused to answer, -1=Missing 
Afrodat4$News_Radio<-repNA04(Afrodat4$Q12A)
Afrodat4$News_Television<-repNA04(Afrodat4$Q12B)
Afrodat4$News_Newspaper<-repNA04(Afrodat4$Q12C)

#Afrodat3----------　インターネット　SNSなし
#Question Number: Q15A,B,C
Afrodat3$News_Radio<-repNA04(Afrodat3$q15a)
Afrodat3$News_Television<-repNA04(Afrodat3$q15b)
Afrodat3$News_Newspaper<-repNA04(Afrodat3$q15c)

#Afrodat2----------　インターネット　SNSなし
#Question Number: Q26A,B,C
Afrodat2$News_Radio<-repNA04(Afrodat2$q26a)
Afrodat2$News_Television<-repNA04(Afrodat2$q26b)
Afrodat2$News_Newspaper<-repNA04(Afrodat2$q26c)

#Afrodat1----------　インターネット　SNSなし
#Question Number: medrad,medtv,mednew
Afrodat1$News_Radio<-repNA04(Afrodat1$medrad)
Afrodat1$News_Television<-repNA04(Afrodat1$medtv)
Afrodat1$News_Newspaper<-repNA04(Afrodat1$mednew)

#----電気　　R6あり、R5、R4にはなし


#---Public affairへの興味
#Afrodat5----------
#Question Number: Q14 
#Question: How interested would you say you are in public affairs? 
#Variable Label: Interest in public affairs 
#Values: 0-3, 9, 998, -1 
#Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don’t know, 998=Refused to answer, -1=Missing 
#Source: SAB 
#Note : Interviewer was instructed to prompt if necessary with “You know, in politics and government.” 
Afrodat5$Interest_pubaff<-repNA03(Afrodat5$Q14)

#Afrodat4----------
#Question Number: Q13
Afrodat4$Interest_pubaff<-repNA03(Afrodat4$Q13)

#Afrodat3----------
#Question Number: Q16
Afrodat3$Interest_pubaff<-repNA03(Afrodat3$q16)

#//Afrodat2----------尋ね方(レベル)が違う
#Question Number: Q27
#Question Number: How interested are you in public affairs?
#Value Labels: 0=Not interested, 1=Somewhat interested, 2=Very interested, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
Afrodat2$Interest_pubaff<-repNA02(Afrodat2$q27)

#//Afrodat1----------government and public affairsについて尋ねている
#Variable label: Interested in politics
#Value Labels: 1=Very interested/Always/Most of the time, 2=Somewhat interested/Some of the time,
#3=Now and then, 4=Not interested/Hardly, 5=Don’t Know, 98=Refused to Answer, 99=Missing Data
#Some people seem to follow what’s going on in government and public affairs most of the time, whether
#there’s an election going on or not. Others aren’t that interested. Would you say you follow what’s going
#on in government and public affairs:________?
Afrodat1$Interest_pubaff<-repNA04(Afrodat1$scint)


#政治についての会話
#Afrodat5----------
#Question Number: Q15
#Value Labels: 0=Never, 1=Occasionally, 2=Frequently, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Discuss_politics<-repNA02(Afrodat5$Q15)
#政治についての会話 dummy
Afrodat5$dDiscuss_politics<-ifelse((Afrodat5$Discuss_politics==1)|(Afrodat5$Discuss_politics==2),1,0) 

#Afrodat4----------
#Question Number: Q14
Afrodat4$Discuss_politics<-repNA02(Afrodat4$Q14)
#政治についての会話 dummy
Afrodat4$dDiscuss_politics<-ifelse((Afrodat4$Discuss_politics==1)|(Afrodat4$Discuss_politics==2),1,0)

#Afrodat3----------
#Question Number: Q17
Afrodat3$Discuss_politics<-repNA02(Afrodat3$q17)
#政治についての会話 dummy
Afrodat3$dDiscuss_politics<-ifelse((Afrodat3$Discuss_politics==1)|(Afrodat3$Discuss_politics==2),1,0) 

#//Afrodat2----------尋ね方（レベル）が異なる
#Question Number: Q25A
#Variable label: Discuss politics
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes,
#several times, 4=Yes, often, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
Afrodat2$Discuss_politics<-repNA04(Afrodat2$q25a)
#政治についての会話 dummy
Afrodat2$dDiscuss_politics<-ifelse((Afrodat2$Discuss_politics==1)|(Afrodat2$Discuss_politics==2)|(Afrodat2$Discuss_politics==3)|(Afrodat2$Discuss_politics==4),1,0) 

#//Afrodat1----------尋ね方（レベル）が異なる
#scdsc
#When you get together with friends, would you say you discuss political matters…?
# Never Occasionally Frequently Don’t know (DNR) 
#Value Labels: 0=Never, 1=Sometimes/Occasionally/Only once, 2=Often/Frequently, 9=Don’t Know, 98=Refused to Answer, 99=Missing Data
Afrodat1$Discuss_politics<-repNA03(Afrodat1$scdsc)
#政治についての会話 dummy
Afrodat1$dDiscuss_politics<-ifelse((Afrodat1$Discuss_politics==1)|(Afrodat1$Discuss_politics==2),1,0) 

#コミュニティ
#Afrodat5---------- 1=Inactive Member, 2=Active Member, 3=Official Leader
#Question Number: Q25A
Afrodat5$Mem_religious<-repNA03(Afrodat5$Q25A)

#Afrodat4----------
#Question Number: Q22A
Afrodat4$Mem_religious<-repNA03(Afrodat4$Q22A)

#Afrodat3----------
#Question Number: Q28A
Afrodat3$Mem_religious<-repNA03(Afrodat3$q28a)

#//Afrodat2---------- 
#Question Number: Q24D
#Question: Now I am going to read out a list of groups that people join or attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or not a member: A community development or self-help association?
#Value Labels: 0=Not a Member, 1=Inactive Member, 2=Active Member, 3=Official Leader, 9=Don’t Know,98=Refused to Answer, -1=Missing Data
Afrodat2$Mem_voluntary<-repNA03(Afrodat2$q24d)

#//Afrodat1---------- Yes/No
#Now I am going to read out a list of voluntary organizations. For each one, could you tell me whether you are an official leader, an active member, an inactive member or not a member of that type of
#organization: [Community development association]?
# Official leader Active member Inactive member Not a member  memdev
#Labels value 0 No, not a member/Never attend,1     Yes, a member/ever attend
Afrodat1$Mem_voluntary<-repNA02(Afrodat1$memdev)

#コミュニティvoluntary
#Afrodat5----------
#Question Number: Q25B
Afrodat5$Mem_voluntary<-repNA03(Afrodat5$Q25B)    #

#Afrodat4----------
#Question Number: Q22B
Afrodat4$Mem_voluntary<-repNA03(Afrodat4$Q22B)    #

#//Afrodat3---------- Member of trade union or farmers associationとして聴取
#Question Number: Q28B
Afrodat3$Mem_voluntary<-repNA03(Afrodat3$q28b)

#actions as citizens Raise an issue
#Afrodat5----------
#Question Number: Q26A
#Value Labels: 0=No, would never do this, 1=No, but would do if had the chance, 2=Yes, once or twice, 3=Yes, several times, 4=Yes, often, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Cit_action_Attend_meeting<-repNA03(Afrodat5$Q26A)    #
Afrodat5$Cit_action_raise_issue<-repNA03(Afrodat5$Q26B)    #

#Afrodat4----------
#Question Number: Q23A
Afrodat4$Cit_action_Attend_meeting<-repNA03(Afrodat4$Q23A)    #
Afrodat4$Cit_action_raise_issue<-repNA03(Afrodat4$Q23B)    #

#Afrodat3----------
#Question Number: Q31A
Afrodat3$Cit_action_Attend_meeting<-repNA03(Afrodat3$q31a)    #
Afrodat3$Cit_action_raise_issue<-repNA03(Afrodat3$q31b)    #

#Afrodat2----------
#Question Number: Q25A
Afrodat2$Cit_action_Attend_meeting<-repNA03(Afrodat2$q25a)    #
Afrodat2$Cit_action_raise_issue<-repNA03(Afrodat2$q25b)    #

#Afrodat1----------
#Question Number: parcom
Afrodat1$Cit_action_Attend_meeting<-repNA03(Afrodat1$parcom)    #
Afrodat1$Cit_action_raise_issue<-repNA03(Afrodat1$pariss)    #


#選挙　R5,R6のみ
#Request Action
#Afrodat5----------
#Question Number: Q29A
#Question Number: Q29B
#Question Number: Q29D
Afrodat5$Ele_campaign_rally<-repNA02(Afrodat5$Q29A)
Afrodat5$Ele_Attend_persuade<-repNA02(Afrodat5$Q29B)
Afrodat5$Ele_Attend_Work<-repNA02(Afrodat5$Q29C)


#議論・行動 :Contact official 
#Afrodat5----------
#"Question Number: Q30A
Afrodat5$Democ_pref<-repNA03(Afrodat5$Q30A)
#Afrodat4----------
#"Question Number: Q25C
Afrodat4$Democ_pref<-repNA03(Afrodat4$Q25C)
#Afrodat3----------
#"Question Number: Q32C
Afrodat3$Democ_pref<-repNA03(Afrodat3$Q32C)
#Afrodat2----------
#"Question Number: Q29C
Afrodat2$Democ_pref<-repNA03(Afrodat2$q29c)
#Afrodat1なし
 

#議論・行動：request action from government
#Afrodat5----------
Afrodat5$Diss_Contact_official<-repNA04(Afrodat5$Q30C)
#Afrodat4----------
Afrodat4$Diss_Contact_official<-repNA04(Afrodat4$Q25C)
#Afrodat3----------
Afrodat3$Diss_Contact_official<-repNA04(Afrodat3$q32c)
#Afrodat2----------
Afrodat2$Diss_Contact_official<-repNA04(Afrodat2$q29c)

#議論・行動：Refuse Tax　R6、R5のみ
#Afrodat5----------
Afrodat5$Diss_Refuse2pay<-repNA04(Afrodat5$Q26C)

#議論・行動(Attend demonstration)
#Afrodat5----------
Afrodat5$Diss_Attend_demonstration<-repNA04(Afrodat5$Q26D)
#Afrodat4----------
Afrodat4$Diss_Attend_demonstration<-repNA04(Afrodat4$Q23C)
#Afrodat3----------
Afrodat3$Diss_Attend_demonstration<-repNA04(Afrodat3$q31c)
#Afrodat2----------
Afrodat2$Diss_Attend_demonstration<-repNA04(Afrodat2$q25d)
#//Afrodat1----------
#Variable name: pardem
#0=Never, 1=Sometimes/Occasionally/Only once, 2=Often/Frequently,
Afrodat1$Diss_Attend_demonstration<-repNA02(Afrodat1$scdsc)


#議論・行動(Democracy pref)
#Afrodat5----------
#Question Number: Q32
#Question: Which of these three statements is closest to your own opinion?
#Value Labels: 1=Statement 3: Doesn’t matter, 
#2=Statement 2: Sometimes non-democratic preferable, 
#3=Statement 1: Democracy preferable, 9=Don’t know, 98=Refused to answer, -1=Missing
#Source: Latinobarometer (LB)
Afrodat5$Democ_pref<-repNA04(Afrodat5$Q32)
#Democ_pref ダミー
Afrodat5$dDemoc_pref<-ifelse((Afrodat5$Democ_pref==3),1,0)

#Afrodat4----------
#Question Number: Q30
#Question: Which of these three statements is closest to your own opinion?
#Value Labels: 1=Statement 3: Doesn’t matter, 
#2=Statement 2: Sometimes non-democratic preferable, 
#3=Statement 1: Democracy preferable, 9=Don’t know, 998=Refused to answer, -1=Missing data
Afrodat4$Democ_pref<-repNA04(Afrodat4$Q30)
#Democ_pref ダミー
Afrodat4$dDemoc_pref<-ifelse((Afrodat4$Democ_pref==3),1,0)

#//Afrodat3----------
#Question Number: Q37
#Question: Which of these three statements is closest to your own opinion?
#A: Democracy is preferable to any other kind of government.
#B: In some circumstances, a non-democratic government can be preferable.
#C: For someone like me, it doesn’t matter what kind of government we have.
#Value Labels: 1=Statement C: Doesn’t matter, 
#2=Statement B: Sometimes non-democratic preferable, 
#3=Statement A: Democracy preferable, 9=Don’t Know, 98=Refused to Answer, -1=Missing DataAfrodat4$Democ_pref<-repNA04(Afrodat4$Q30)
Afrodat3$Democ_pref<-repNA04(Afrodat3$q37)
#Democ_pref ダミー
Afrodat3$dDemoc_pref<-ifelse((Afrodat3$Democ_pref==3),1,0)

#//Afrodat2----------
#Question Number: Q38
#Value Labels: 1=Statement C: Doesn’t matter, 
#2=Statement B: Sometimes non-democratic preferable, 
#3=Statement A: Democracy preferable, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
Afrodat2$Democ_pref<-repNA04(Afrodat2$q38)
#Democ_pref ダミー
Afrodat2$dDemoc_pref<-ifelse((Afrodat2$Democ_pref==3),1,0)

#//Afrodat1----------
#Question Number: supdem
#Value Labels: 1=Democracy is preferable to any other form of government, 
#2=To people like me, it doesn't matter what form of government, 
#3=In certain situations, a non-democratic government can be preferable, 4=Don't know, 97=Not applicable, 98=Refused to answer, 99=Missing Data
Afrodat1$Democ_pref<-repNA04(Afrodat1$supdem)
#Democ_pref ダミー
Afrodat1$dDemoc_pref<-ifelse((Afrodat1$Democ_pref==1),1,0) #反対


#議論・行動(Extent of democracy)
#Afrodat5----------
#Variable Label: Q42
#Value Labels: 1=Not a democracy, 2=A democracy, with major problems, 3=A democracy, but with minor problems, 4=A full democracy, 8=Do not understand question/ do not understand what ‘democracy’ is, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Democ_nation<-ifelse(Afrodat5$Q42<1|Afrodat5$Q42>4,NA,Afrodat5$Q42)
table(Afrodat5$Democ_nation,exclude=NULL)        #

#Afrodat4----------
#Variable Label: Q42A
#Value Labels: 1=Not a democracy, 2=A democracy, with major problems, 3=A democracy, but with minor problems, 4=A full democracy, 8=Do not understand question/ do not understand what ‘democracy’ is, 9=Don’t know, 998=Refused to answer,
Afrodat4$Democ_nation<-ifelse(Afrodat4$Q42A<1|Afrodat4$Q42A>4,NA,Afrodat4$Q42A)
table(Afrodat4$Democ_nation,exclude=NULL)        #

#Afrodat3----------
#Variable Label: Q46
Afrodat3$Democ_nation<-ifelse(Afrodat3$q46<1|Afrodat3$q46>4,NA,Afrodat3$q46)
table(Afrodat3$Democ_nation,exclude=NULL)        #


#議論・行動(Democracy Support)
#Afrodat5----------
#Question Number:Q43
#Value Labels: Value Labels: 0=the country is not a democracy, 
#1=Not at all satisfied, 2=Not very satisfied, 3=Fairly satisfied, 4=Very satisfied, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Democ_satis<-repNA04(Afrodat5$Q43)
#Afrodat4----------
#Question Number:Q43
Afrodat4$Democ_satis<-repNA04(Afrodat4$Q43)
#Afrodat3----------
#Question Number:Q47
Afrodat$Democ_satis<-repNA04(Afrodat3$q47)
#Afrodat2----------
#Question Number:Q40
Afrodat2$Democ_satis<-repNA04(Afrodat2$q40)
#Afrodat1----------
#//Question Number:dmpsat
#Very dissatisfied  Somewhat dissatisfied  Somewhat satisfied  Very satisfied  Uganda is not a democracy Not applicable
Afrodat1$Democ_satis<-ifelse(Afrodat1$dmpsat<1|Afrodat1$dmpsat>4,NA,Afrodat1$dmpsat)
Afrodat1$Democ_satis<-repNA04(Afrodat1$dmpsat)

#Trust: President R6,R5,R4,R3のみ
#Afrodat5---------- Trust_traditional_leadersなし
Afrodat5$Trust_president<-repNA04(Afrodat5$Q59A)
Afrodat5$Trust_parliament<-repNA04(Afrodat5$Q59B)
Afrodat5$Trust_police<-repNA04(Afrodat5$Q59H)
#Afrodat5$Trust_traditional_leaders<-repNA04(Afrodat5$Q59K)
Afrodat5$Trust_religious_leaders<-repNA04(Afrodat5$Q59E)

#Afrodat4---------- 
Afrodat4$Trust_president<-repNA04(Afrodat4$Q49A)
Afrodat4$Trust_parliament<-repNA04(Afrodat4$Q49B)
Afrodat4$Trust_police<-repNA04(Afrodat4$Q49G)
Afrodat4$Trust_traditional_leaders<-repNA04(Afrodat4$Q49I)
Afrodat4$Trust_religious_leaders<-repNA04(Afrodat4$Q49D)

#Afrodat3----------
Afrodat3$Trust_president<-repNA04(Afrodat3$q55a)
Afrodat3$Trust_parliament<-repNA04(Afrodat3$q55b)
Afrodat3$Trust_police<-repNA04(Afrodat3$q55h)
#Afrodat3$Trust_traditional_leaders<-repNA04(Afrodat3$q55a)
Afrodat3$Trust_religious_leaders<-repNA04(Afrodat3$q55d)

#腐敗、賄賂(は参考)　年によって尋ね方が異なる
#//Afrodat3----------
#Question Number:Q65J
#Question: How well or badly would you say the current government is handling the following matters, or haven’t you heard enough about them to say: Fighting corruption in government?
#Variable Label: Handling fighting corruption
#Value Labels: 1=Very Badly, 2=Fairly Badly, 3=Fairly Well, 4=Very Well, 9=Don’t Know/Haven’t heard enough, 98=Refused to Answer, -1=Missing Data
Afrodat3$corruption<-repNA04(Afrodat3$q65j)

#//Afrodat2----------
#Question Number: Q51A
#Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
#about them to say: The President and Officials in his Office?
#Variable label: Corruption: Office of the Presidency
#Values: 0-3, 9, 98, -1
#Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
#-1=Missing Data
#Source: SAB
Afrodat2$corruption<-repNA04(Afrodat2$q51a)

#//Afrodat1----------
#Variable name: pfpcr2
#Variable label: Extent of corruption/bribery
#Values: 1-5, 9, 98, 99
#Value Labels: 1=Strongly disagree/Almost all, 2=Disagree/Most, 3=Agree/A few/Some, 4=Strongly
#agree/Almost none, 9=Don’t Know, 98=Refused to Answer, 99=Missing Data 
Afrodat1$corruption<-repNA04(Afrodat1$pfpcr2)


#所有-------------R6,R5,R4のみ
#Afrodat6
#Q91a. Own radio
#Q91b. Own television
#Q91c. Own motor vehicle, car, or motorcycle
#Q91d. Own mobile phone
#Afrodat5----------
#Question Number: Q90A
Q90A:Radio
Q90B:Television
Q90C:Own motor vehicle, car, or motorcycle
Afrodat5$Own_Radio <-repNA01(Afrodat5$Q90A)
Afrodat5$Own_TV <-repNA01(Afrodat5$Q90B)

#Afrodat4----------
#Question Number: Q90A
Q90A:Radio
Q90B:Television
Q90C:Own motor vehicle, car, or motorcycle
Afrodat4$Own_Radio <-repNA01(Afrodat4$Q90A)
Afrodat4$Own_TV <-repNA01(Afrodat4$Q90B)
#Afrodat3----------
#Question Number: Q93A
Q93B:Radio
Q93C:Television
Q93D:Bicycle
Q93E:Motorcycle
Q93F:Motor vehicle or car.
Afrodat3$Own_Radio <-repNA01(Afrodat3$q93b)
Afrodat3$Own_TV <-repNA01(Afrodat3$q93c)

#使用-------------R6,R5,R4のみ
#Afrodat5---------- インターネットとコンピュータのみ
#Question Number: Q91B 
#Question: How often do you use: The Internet?
#Value Labels: 0=Never, 1=Less than once a month, 2=A few times a month, 3=A few times a week, 4=Everyday, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Use_Inet <-repNA04(Afrodat5$Q91B)

#Question Number: Q92 ##誰のmobileを使ったか
#Question: Do you ever use a mobile phone? If so, who owns the mobile phone that you use most often?
#Value Labels: 0= No, I never use a mobile phone, 1= Yes, I use a mobile phone that I own, 2= Yes, I use a mobile phone owned by someone else in my household, 3= Yes, I use a mobile phone owned by someone outside my household, 9=Don’t know, 998=Refused to answer, -1=Missing
Afrodat5$Use_Mbphone <-repNA03(Afrodat5$Q92)
Afrodat5$dUse_Mbphone <-ifelse((Afrodat5$Use_Mbphone==1)|(Afrodat5$Use_Mbphone==2),1,0)

#Afrodat4----------
Q88A:use a cell phone
Q88B:How often use a computer
Q88C:How often do you use: The Internet?
Afrodat4$Use_Mbphone <-repNA04(Afrodat4$Q88A)
Afrodat4$Use_Inet <-repNA04(Afrodat4$Q88C)
