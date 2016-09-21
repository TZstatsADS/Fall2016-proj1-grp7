#analysis about incomes; use the variable WAGP
install.packages("plotly")
library(plotly)
income_plot = filter(db, NATIVITY==2 & COB_name==top_countries$COB_name)
income_plot$WAGP=income_plot$WAGP/10000
plot_ly(income_plot[1:2000,],x=COB_name,y=WAGP,mode = "markers",color = COB_name,size=WAGP)

##highest salary
plot_ly(income_plot,color = COB_name,x= COB_name, y = WAGP)


##classification
income_plot$WAGP=income_plot$WAGP*10000
income_plot$WAGP[income_plot$WAGP>0 & income_plot$WAGP<=20000]=1
income_plot$WAGP[income_plot$WAGP>20000 & income_plot$WAGP<=40000]=2
income_plot$WAGP[income_plot$WAGP>40000 & income_plot$WAGP<=60000]=3
income_plot$WAGP[income_plot$WAGP>60000 & income_plot$WAGP<=80000]=4
income_plot$WAGP[income_plot$WAGP>80000 & income_plot$WAGP<=100000]=5
income_plot$WAGP[income_plot$WAGP>100000 & income_plot$WAGP<=150000]=6
income_plot$WAGP[income_plot$WAGP>150000]=7
no_record = vector()
no_payment = vector()
m = matrix(nrow = 5,ncol = 7)
for (i in 1:5){
  no_record[i] = sum(is.na(filter(income_plot,COB_name==select_countries[i])$WAGP))
  matr =filter(income_plot,COB_name==select_countries[i])
  for(j in 1:7){
    m[i,j]=dim(filter(matr,WAGP==j))[1]
  }
  no_payment[i] = dim(filter(matr,WAGP==0))[1]
}
fin_num = cbind(no_payment,m,no_record)
vals = matrix(t(fin_num),ncol = 1)
spes = c(rep("Mexico",9),rep("China",9),rep("Cuba",9),rep("Canada",9),rep("Germany",9))
cons = rep(c("no payment","0-20000","20000-40000","40000-60000","60000-80000","80000-100000","100000-150000",">150000","no record"),5)
data_income = data.frame(spes,cons,vals)
p <- ggplot(data_income, aes(fill=cons, y=vals, x=spes))
p + geom_bar(stat = "identity", position="dodge") + labs(title="Income differences in different countries")





