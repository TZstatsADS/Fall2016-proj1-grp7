install.packages("dplyr")
install.packages("data.table")
install.packages("DT")
install.packages("ggplot2")
install.packages("plotly")
install.packages("lhs")
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
library(lhs)

#Load data and select variables:
selected.variables = c("ST","SCHL", "NATIVITY","PWGTP","INDP","SOCP","COW","ESR","POBP","POVPIP","WAGP","FINCP","HINCP","FFINCP","FHINCP","OIP")
hus.a.data = fread("~/Desktop/ss14pusa.csv", select = selected.variables)
hus.b.data = fread("~/Desktop/ss14pusb.csv", select = selected.variables)
db = rbind(hus.a.data, hus.b.data)
rm(hus.a.data, hus.b.data)

# Get country names and add to db:
countries = fread("~/Desktop/countrynames.csv")
db = db %>%
  left_join(., countries, by = c("POBP" = "code")) %>%
  mutate(COB_name = name) %>%
  select(-name)

#Get state names and add to db:
states = fread("~/Desktop/statenames.csv")
db = db %>% 
  left_join(., states, by = c("ST" = "code")) %>%
  mutate(State = abbr) %>%
  select(-c(name, abbr))

# Find top countries of immigration:
top_n = 20 # the top how many countries of immigration
top_countries = db %>% 
  filter(NATIVITY==2) %>%
  group_by(COB_name) %>%
  summarise(sum(PWGTP)) %>%
  arrange(desc(`sum(PWGTP)`)) %>%
  head(top_n)


select_countries = c('Mexico','China', 'Cuba', 'Canada', 'Germany') # top_countries$COB_name
plot_data = filter(db, NATIVITY==2 & COB_name %in% select_countries)

#bar chart about class of worker in the typical countries, use the variable COW
species = c(rep("Mexico",10),rep("China",10),rep("Cuba",10),rep("Canada",10),rep("Germany",10))
conditions = rep(c("for wages","none-profit","local government","state government","fedral government","Self-employed in own not incorporated","Self-employed in own incorporated","Working without pay(family business or farm)","unemployed","less than 16"),5)

count_country_class_num = matrix(nrow = 5,ncol = 10)
for(i in 1:5){
  for(j in 0:9){
    count_country_class_num[i,j]=dim(filter(plot_data,COW==j & COB_name==select_countries[i]))[1]
  }
}
less_than_16 = vector()
for (i in 1:5){
  less_than_16[i] = sum(is.na(plot_data$COW) & plot_data$COB_name==select_countries[i])
}
count_country_class_num[,10]=less_than_16

values = as.numeric(matrix(data = count_country_class_num,nrow = 1,byrow = FALSE))
data = data.frame(species,conditions,values)
p <- ggplot(data, aes(fill=conditions, y=values, x=species))
p + geom_bar(position="dodge", stat="identity") + labs(title='Class of worker', x='Countries', y='Values', fill='Class')

  
#bar chart about the Employment status recode, using the variable ESR
under16 = vector()
for ( i in 1:5){
  under16[i]=sum(is.na(filter(plot_data,plot_data$COB_name==select_countries[i])$ESR))
  print(table(filter(plot_data,plot_data$COB_name==select_countries[i])$ESR))
}
values_esr = c(3574,52489,1241,3855,61,3,29786,1104,8909,288,531,20,0,7370,345,4853,93,382,9,0,4346,14,4118,149,201,10,0,3928,121,2489,74,128,14,0,3629)
species_esr= c(rep("Mexico",7),rep("China",7),rep("Cuba",7),rep("Canada",7),rep("Germany",7))
conditions_esr = rep(c("under 16","civilian employed and at work","civilian employed without work","unemployed","armed forces, at work","armed forces without work","not in labor force"),5)
data_esr = data.frame(species_esr,conditions_esr,values_esr)
p <- ggplot(data_esr, aes(fill=conditions_esr, y=values_esr, x=species_esr))
p + geom_bar(position="dodge", stat="identity") + labs(title='Employment Status Recode', x='Countries', y='Values', fill='Employment Status')

#bar chart about Industry recode using the variable INDP
#classify industry through INDP fisrt three letters
AGR = c(170,180,190,270,280,290)
EXT = c(370,380,390,470,490)
UTL = c(570,580,590,670,680,690)
CON = c(770)
MFG = c(1070,1080,1090,1170,1180,1190,1270,1280,1290,1370,1390,1470,1480,1490,1570,1590,1670,1680,1690,1770,1790,1870,1880,1890,1990,2070,2090,2170,2180,2190,2270,2280,2290,2370,2380,2390,2470,2480,2490,2570
        ,2590,2670,2680,2690,2770,2780,2790,2870,2880,2890,2970,2980,2990,3070,3080,3095,3170,3180,3190,3365,3370,3380,3390,3470,3490,3570,3580,3590,3670,3680,3690,3770,3780,3790,3875,3895,3960,3970,3980,3990)
WHL = c(4070,4080,4090,4170,4180,4195,4265,4270,4280,4290,4370,4380,4390,4470,4480,4490,4560,4570,4580,4585,4590)
RET = c(4670,4680,4690,4770,4780,4795,4870,4880,4890,4970,4980,4990,5070,5080,5090,5170,5180,5190,5275,5280,5295,5370,5380,5390,5470,5480,5490,5570,5580,5590,5591,5592,5670,5680,5690,5790)
TRN = c(6070,6080,6090,6170,6180,6190,6270,6280,6290,6370,6380,6390)
INF = c(6470,6480,6490,6570,6590,6670,6672,6680,6690,6695,6770,6780)
FIN = c(6870,6880,6890,6970,6990,7070,7080,7170,7180,7190)
PRF = c(7270,7280,7290,7370,7380,7390,7460,7470,7480,7490,7570,7580,7590,7670,7680,7690,7770,7780,7790)
EDU = c(7860,7870,7880,7890)
MED = c(7970,7980,7990,8070,8080,8090,8170,8180,8190,8270,8290)
SCA = c(8370,8380,8390,8470)
ENT = c(8560,8570,8580,8590,8660,8670,8680,8690)
SRV = c(8770,8780,8790,8870,8880,8970,8980,8990,9070,9080,9090,9160,9170,9180,9190,9290)
ADM = c(9370,9380,9390,9470,9480,9490,9470,9480,9490,9570,9590)
MIL = c(9670,9680,9690,9770,9780,9790,9870)
UNEMPLOYED = 9920
foreignkey1 = c(AGR,EXT,UTL,CON,MFG,WHL,RET,TRN,INF,FIN,PRF,EDU,MED,SCA,ENT,SRV,ADM,MIL,UNEMPLOYED)
foreignkey2 = c(rep("AGR",length(AGR)),rep("EXT",length(EXT)),rep("UTL",length(UTL)),rep("CON",length(CON))
                ,rep("MFG",length(MFG)),rep("WHL",length(WHL)),rep("RET",length(RET)),rep("TRN",length(TRN))
                ,rep("INF",length(INF)),rep("FIN",length(FIN)),rep("PRF",length(PRF)),rep("EDU",length(EDU))
                ,rep("MED",length(MED)),rep("SCA",length(SCA)),rep("ENT",length(ENT)),rep("SRV",length(SRV))
                ,rep("ADM",length(ADM)),rep("MIL",length(MIL)),rep("UNEMPLOYED",length(UNEMPLOYED)))
foreignkeys = as.data.frame(cbind(foreignkey1,foreignkey2))
names(foreignkeys)[1] = "INDP"
plot_data1 = merge(plot_data, foreignkeys, by=c("INDP"))
Mexico = filter(plot_data1,COB_name=="Mexico")
China = filter(plot_data1,COB_name =="China")
Cuba = filter(plot_data1,COB_name == "Cuba")
Canada = filter(plot_data1,COB_name == "Canada")
Germany = filter(plot_data1,COB_name =="Germany")
Mexico_num = as.numeric(table(Mexico$foreignkey2))
China_num = as.numeric(table(China$foreignkey2))
Cuba_num = as.numeric(table(Cuba$foreignkey2))
Canada_num = as.numeric(table(Canada$foreignkey2))
Germany_num = as.numeric(table(Germany$foreignkey2))
num_mat = rbind(Mexico_num,China_num,Cuba_num,Canada_num,Germany_num)
less_than_16_INPD = vector()
for (i in 1:5){
  less_than_16_INPD[i]=sum(is.na(filter(plot_data,COB_name == select_countries[i])$INDP))
}
total_num_mat = cbind(less_than_16_INPD,num_mat)
total_num_val = matrix(total_num_mat,nrow = 1,byrow = TRUE)
species_INDP = c(rep("Mexico",20),rep("China",20),rep("Cuba",20),rep("Canada",20),rep("Germany",20))
conditions_INDP = rep(c("under 16","AGR","EXT","UTL","CON","MFG","WHL","RET","TRN","INF","FIN","PRF","EDU","MED","SCA","ENT","SRV","ADM","MIL","UNEMPLYED"),5)
data_INDP = data.frame(species_INDP,conditions_INDP,total_num_val)
p<-ggplot(data_INDP,aes(fill=conditions_INDP,y=total_num_val,x=species_INDP))
p+geom_bar(position = "dodge",stat = "identity")+labs(title="Industry fields record in different countries")