# R-code
####安装不了包时把镜像切换到Austria 上面，他是主库####

####快捷键####
ctrl+shift+c #将语句变成注释语句

####把变量名命成统一的格式####

####若自变量为分类变量，则首先要转化成因子####

####将原始数据标准化处理完（主要是变量重命名，选取有效变量）以后导出一个备份的数据集，缺失值的删除应该具体问题具体分析####

####检查某一特征（列）和样本（行）的缺失量是否超过5%####
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)

####在第几分量求到最小（大）值####
which.min(x)
which.max(x)

####建立自定义函数用于计算OR值及其95%置信区间，并输出β、S.E和P值####
poolTable<-function(fit.pool){
  
  Table<-summary(glm_pool)
  
  OR<-round(exp(Table$estimate),3)
  
  LCI<-round(exp(Table$estimate-1.96*Table$std.error),3)
  
  UCI<-round(exp(Table$estimate+1.96*Table$std.error),3)
  
  β<-round(Table$estimate,3)
  
  S.E<-round(Table$std.error,3)
  
  P<-round(Table$p.value,4)
  
  library(reshape)
  
  library(tidyr)
  
  Table<-rename(Table,c(term="Variable"))
  
  Table<-cbind(Table[1],β,S.E,OR,LCI,UCI,P)
  
  Table$k1<-"(";Table$k2<-"-";Table$k3<-")"
  
  Table<-Table[,c("Variable","β","S.E","OR","k1","LCI","k2","UCI","k3","P")]
  
  Table<-unite(Table,"OR(95%CI)",c(OR,k1,LCI,k2,UCI,k3),sep = "",remove = T)
  
  return(Table)
  
}

####error_bar####
mean <- aggregate(df$X8周VB12, by=list(df$CKD, df$DOSE), FUN=mean, na.rm=T)
sd <- aggregate(df$X8周VB12, by=list(df$CKD, df$DOSE), FUN=sd, na.rm=T)
df_res <- data.frame(mean, sd=sd$x)
colnames(df_res) = c("CKD", "DOSE", "Mean", "Sd")

df_res$CKD<-as.factor(df_res$CKD)
df_res$DOSE<-as.factor(df_res$DOSE)

ggplot(df_res, aes(x=DOSE, y=Mean, fill=CKD)) +
  geom_bar(stat="identity",position="dodge",color="black", width=.6) +
  ylab("VB12")+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd),position=position_dodge(.6), width=.2)+
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  theme(legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))+
  scale_fill_discrete(labels=c("Non-CKD","CKD"))+
  labs(fill="group") +
  scale_fill_manual(values = c('#2a9d8f', '#e63946'), 
                    labels = c('Non-CKD', 'CKD'))
ggsave("E:/qiangqiangshixiong/结果/error_bar/VB12.jpg",width=15,height=10)

####利用R语言的Mice包的md.pattern函数来查看数据的缺失模式####
md.pattern(data)

####设置随机种子以便复现结果####
set.seed(12345)

####列出当前工作空间的所有对象####
ls()

####查看数据结构####
str(data)

####打乱数据的样本顺序####
iris_rand <- iris[order(runif(150)),]

####将数据标准化####
iris_z <- as.data.frame(scale(iris_rand[,-5]))

####每页显示两张图####
layout(matrix(c(1,2),1,2))
####每页显示一张图####
layout(matrix(1))

####返回行列数####
dim(data)

####查看数据集的详细信息####
?longley

####默认小数位数####
options(digits = 2)

####检查内容####
检查数据样本量/检查变量结构/检查图的坐标轴范围是否合理/检查数据是否有备份/检查各分类组的人数是否可比

####将几个变量批量转化成因子####
df[,cols] <- lapply(df[,cols],as.factor)

####查看某个变量的类型####
class(f1$x0wPRO2)

####查看某个变量是否有缺失值####
table(is.na(e2$x0wPRO1))

####将几分类变量转化为二分类变量####
Affairs$ynaffairs[Affairs$affairs>0]<-1
Affairs$ynaffairs[Affairs$affairs==0]<-0

####将变量转化为因子型变量且添加标签####
Affairs$ynaffairs<-factor(Affairs$ynaffairs,levels = c(0,1),labels = c("NO","YES"))

####以Date为切割变量，对每组数据进行转换transform####
library(gcookbook)#为了使用数据
library(plyr)#为了使用函数
#数据做了转换后赋值给一个新的数据集
ce<-ddply(cabbage_exp,"Date",transform,percent_weight=Weight/sum(Weight)*100)
####根据日期和性别对数据进行排序####
ce<-arrange(cabbage_exp,Date,Cultivar)

####产生序列####
age=seq(17,57,10)

####查看数据集####
View(dataset)

####删除数据集####
rm()
rm(r1,r2)

####变量重编码####
c$CKD5[c$CKD4==0]<-0
c$CKD5[c$CKD4==1 & c$eGFR<=45]<-1
c$CKD5[c$CKD4==1 & c$eGFR>45 & c$eGFR<=60]<-2
c$CKD5[c$CKD4==1 & c$eGFR>60]<-3

####导入csv文件####
rawdata<-read.csv("E:/lintingshijie/data.csv",header = T,stringsAsFactors = F)

####导入excel文件####
library(xlsx)
rawdata<-read.xlsx("E:/lintingshijie/data.xlsx",sheet=1)


####学会查看帮助文档####
help()
?()
####查看变量取值情况####
unique(b$rs1801131)

####查看变量结构####
names()
table(y21$eGFR)
names(y1)
summary(y1$eGFR)
str()

####求变量的图基五数####
fivenum()

####更改变量名####
library(plyr)
score<-rename(score,c(pl="chinese"))

####筛选某个变量不等于空字符的子集####
gene_analysis<-subset(b, rs1801131 != '')
####筛选一部分变量作为子集####
gene_analysis<-subset(b, select=c("",""))

####求数据集的行数和列数####
nrow()
ncol()
####求每列的均值####
colMeans(y)

####ggplot画图####
ggplot(y21,aes(x=eGFR,y=x0wSAH))+geom_line()

####作回归####
model<-glm(eGFR~x0wSAH,family=gaussian(link = "identity"),data=y21)

####拟合曲线####
####两个连续变量拟合曲线####
#library(mgcv)
y3<-subset(y1,!is.na(y1$eGFR)&!is.na(y1$x0wSAM))
y3$eGFR<-round(y3$eGFR,0)
y3$x0wSAM<-round(y3$x0wSAM,0)
model<-glm(eGFR~x0wSAM,family=gaussian(link = "identity"),data=y3)
summary(model)
#y为连续变量
p2<-plot(gam(eGFR~s(x0wSAM,fx=T,k=3),
         family=gaussian(link = "identity"),data=y21),shift=131.22878,ylim = c(0,150),
         xlab = "SAM,ng/mL",ylab = "eGFR,ml/min.1.73m^2" ,cex.lab=1.5,cex.axis=0.8)
####logic拟合曲线####
#y为分类变量
plot(gam(Death~s(smk.in0,fx=T,k=3)+age+bmi+dbp+sbp+as.factor(alh)+glu+chol+trig+hdl+edu+occu,
family="binomial"(link="logit"),data=m),xlim = c(0,60),
xlab = "Pack-years",ylab = "LogOR for Death" ,cex.lab=1.5,cex.axis=0.8,cex.main=1.5,main = "A")

####导出csv文件####
write.csv(tabler,"E:/lintingshijie/结果/pro和几个指标_基线/表一.csv",row.names=T)

####导出图片####

####规定整数表示方式####
options(scipen = 100)

####匹配数据####
data1<-read.csv("E:/yapingshijie/精准叶酸数据匹配/1657.csv",header = T,stringsAsFactors = F)
data2<-read.csv("E:/yapingshijie/精准叶酸数据匹配/422.csv",header = T,stringsAsFactors = F)
merge_data<-merge(data1,data2,by="pid",all=T)
write.csv(merge_data,"E:/yapingshijie/精准叶酸数据匹配/merge_data.csv",row.names=T)

####识别缺失值、不可能值、无穷值####
is.na() is.nan() is.infinite()

####complete.cases()可以用来识别矩阵或数据框中没有缺失值的行####
data(sleep, package = "VIM")
sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]

####使用sum()和mean()函数来获取关于缺失数据的有用信息####
####Dream变量的缺失数####
sum(is.na(sleep$Dream))
####Dream变量缺失数所占的比例####
mean(is.na(sleep$Dream))
####数据集含缺失值的观测所占的比例####
mean(!complete.cases(sleep))

####mice包中的md.pattern()函数可生成一个矩阵或数据框形式展示缺失值模式的表格####
library(mice)
data(sleep, package = "VIM")
md.pattern(sleep)

####图形探索缺失数据####
library("VIM")
aggr(sleep, prop = F, numbers = T)

matrixplot(sleep)

####含有缺失值的行删除####
newdata <- mydata[complete.cases(mydata), ]
newdata <- na.omit(mydata)

####R数据框合并(merge)的几种方式####
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3))) 
df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("Alabama", 2), rep("Ohio", 1)))
merge(x = df1, y = df2, by = "CustomerId", all = TRUE) 
merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
merge(x = df1, y = df2, by = NULL)



####求均值####
mean(weight)

####求标准差####
sd(weight)

####求两个变量的相关系数####
cor(age,weight)

####显示当前的工作路径####
getwd()

####设置当前的工作路径####
setwd()

####生成两个

####R画不出来图输入命令####
dev.new()

####交互作用####

####中位数加减四分位数折线图####

####提取模型参数####
summary(model)$coefficients[c(2,3,4,5),c(1,4)] 
model$p.value
####求模型的系数####
summary(model)$coefficients
coefficients(model)

####求系数的置信区间####
confint(model)

####表一（不同组之间的基线特征）####
library(tableone)
var=c('v22.age','BMI','sbp.v22','dbp.v22','tcho.v22.new','tg.v22.new',
      'hdl.v22.new','glu.v22.new','folate','hcy.v22.new',"egfr.v22.new","CIMT",
      'newsmk','newalh',"center.y",'c677t')
tabler<-CreateTableOne(vars =var,strata = c("sex"),varsToFactor,data = m)
####展现变量为非正态分布——中位数（四分位数）####
tabler<-print(tabler,nonnormal = c('tcho.v22.new','tg.v22.new',
                                   'hdl.v22.new','glu.v22.new','hcy.v22.new','folate',"egfr.v22.new"),showAllLevels = TRUE,contDigits=1)
####展现变量为正态分布——均值（标准差）####
tabler<-print(tabler,showAllLevels = TRUE,contDigits=2)

write.csv(tabler,"table1-sex.csv",row.names=T)

####检验数据是否为正态分布####
qqnorm(crabs$CW, main ="QQ for Crabs")
qqline(crabs$CW)
shapiro.test(h$HCY)

####导出png格式图片####
dev.new()
png(filename = "Rplot%03d.png",width = 480, height = 480, res=300,units = "px")
plot(gam(x0wSAH~s(eGFR,fx=T,k=3),
         family=gaussian(link = "identity"),data=y21),shift=24.409602,xlim = c(0,150),
     xlab = "eGFR,ml/min.1.73m^2",ylab = "SAH,ng/mL" ,cex.lab=1.5,cex.axis=0.8)
dev.off()

####求两个变量的相关性并做相关关系的显著性检验####
####两个连续变量使用pearso检验####
cor(eGFR, x0wSAH, use = "complete.obs",
    method = "pearson")
cor.test(x,y,alternative="less",method="pearson")

####按照分位数删除离群点####
new_ckd <- subset(new_ckd,new_ckd$HCY_chage < quantile(new_ckd$HCY_chage,0.75, na.rm = T) + 1.5*(quantile(new_ckd$HCY_chage,0.75, na.rm = T) - quantile(new_ckd$HCY_chage,0.25, na.rm = T)) &
                    new_ckd$HCY_chage > quantile(new_ckd$HCY_chage,0.25, na.rm = T) - 1.5*(quantile(new_ckd$HCY_chage,0.75, na.rm = T) - quantile(new_ckd$HCY_chage,0.25, na.rm = T)))

####在一张画布里面排列多张图####
par(mfrow=c(2,4))

####连接函数####
paste0(sprintf("%.2f", -0.316), '(', sprintf("%.3f", -0.701), ',',sprintf("%.3f", 0.068), ')')

####连接β值、置信区间、p值####
r1<-round(summary(model)$coefficients[2,c(1,4)],3)
r2<-round(confint(model)[2,],3)
r <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])
####连接多个β值、置信区间、p值####
r1<-round(summary(model1)$coefficients[c(2,3,4),c(1,4)],3)
r2<-round(confint(model1)[c(2,3,4),],3)
p51<-c(paste0(r1[,1], '(', r2[,1],",", r2[,2], ')'), r1[,2])

####在相同剂量下按不同周次画HCY的箱线图####
library(tidyr)
data11 <- j_HCY[, c('Dose', 'x0wHCY', 'x8wHCY')]
data22 <- gather(data11, week, value, -c(Dose))
data22$Dose <- as.factor(data22$Dose)
ggplot(data22, aes(x = Dose, y = value, fill = week)) +
geom_boxplot()

####将两个变量合并成为一个新的变量####
library(tidyr)
data22 <- gather(data11, week, value, -c(Dose))

####用ggplot画箱线图####
ggplot(data22, aes(x = Dose, y = value, fill=week)) +
  geom_boxplot(width=0.5)+ylim(0,60)+xlab("Dose,mg") + ylab("HCY,umol/L")+labs(fill=NULL)+scale_fill_discrete(labels=c("HCY_0W","HCY_8W"))+
  theme(axis.title.x = element_text(size = 15, family = "myFont"),axis.title.y = element_text(size = 15, family = "myFont"))+
  theme(axis.text.x = element_text(size = 15, family = "myFont"),axis.text.y = element_text(size = 15, family = "myFont"))+
  theme(panel.grid =element_blank())###删除网格和灰色背景
#修改颜色
ggplot(data22, aes(x = Dose, y = value, fill=week)) +
  geom_boxplot(width=0.5)+ylim(0,60)+xlab("Dose,mg") + ylab("HCY,umol/L")+labs(fill=NULL)+scale_fill_discrete(labels=c("HCY_0W","HCY_8W"))+
  theme(axis.title.x = element_text(size = 15, family = "myFont"),axis.title.y = element_text(size = 15, family = "myFont"))+
  theme(axis.text.x = element_text(size = 15, family = "myFont"),axis.text.y = element_text(size = 15, family = "myFont"))+
  scale_fill_manual(values = c('white', 'black'), aesthetics = c('colour','fill'))+
  theme(panel.grid =element_blank()) 


####在图中添加文本####
plot(gam(x0wSAH~s(eGFR,fx=T,k=3),
         family=gaussian(link = "identity"),data=i1),shift=18.631198,xlim = c(0,150),ylim=c(15,40),
     xlab = "eGFR,ml/min.1.73m^2",ylab = "SAH,ng/mL" ,cex.lab=1.5,cex.axis=1.5)
text(55,35,"β=-0.024 (-0.029, -0.019)
     p<0.01",cex=1.3,pos=4)
####画多幅画的组合图####
par(mfrow=c(4,2)) 
####获得当前图形边界参数####
par()$mar 
####设置图形边界参数（下左上右）####
par(mar=c(5.1,5,4.1,4))

####按照某个变量分组计算其他变量的均值####
tapply(shuju[,3],shuju$year,mean)#以年份为组，求shuju表第三列的均值
aggregate(shuju[,3:4],list(shuju[,2]),mean)#以年份为均值，求数据表第三列，第四列的均值
aggdata<-aggregate(gene_ABC,by=list(gene_ABC$dose,gene_ABC$group),FUN=mean,na.rm=T)
aggdata$Group.1<-as.factor(aggdata$Group.1)
aggdata$Group.2<-as.factor(aggdata$Group.2)
myvars<-c("","","")
aggdata<-aggregate(gene_ABC[myvars],by=list(gene_ABC$dose,gene_ABC$group),FUN=mean,na.rm=T)

####改变因子顺序####
databind$status <- factor(databind$status, levels = c('non-CKD', 'CKD'))
####ggplot画箱线图####
RF<-ggplot(databind, aes(Dose,x8wHCY,fill=status)) +
  geom_bar(stat = 'identity',position="dodge",width=0.8) +ylab("HCY")+scale_x_discrete(breaks=c("0","1"),labels=c("non-CKD","CKD"))+
  theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20),axis.text.y = element_text(size = 20))+
  theme(panel.grid =element_blank()) 

####ggplot保存图片####
ggsave("E:/qiangqiangshixiong/结果/柱状图更新/x8wHCY.jpg",width=15,height=10)

####修改刻度标签的文本：离散文本####
scale_x_discrete(breaks=c("0","1"),labels=c("non-CKD","CKD"))+

####移除坐标轴标签####
xlab("")

####求变量的百分数####
quantile(roster$score,c(.8,.6,.4,.2))
####限定小数点后数字的位数####
options(digits=2)
####将变量标准化####
z<-scale(roster[,2:4])
####计算每一行的均值####
score<-apply(z,1,mean)
####按列合并####
cbind(roster,score)
####应用strsplit()以空格为界把学生姓名拆分为姓氏和名字####
name<-strsplit((roster$student)," ")
####使用sapply函数提取列表中每个成分的第i个元素，并储存为一个新的变量,最后与原始数据集按列合并####
firstname<-sapply(name,"[",1)
lastname<-sapply(name,"[",2)
roster<-cbind(firstname,lastname,roster[,-1])
####使用order()依姓氏和名字对数据进行排序####
roter[order(lastname,firstname),]
####控制流####
####重复和循环####
####for结构####
for(i in 1:10) print("Hello")
####while结构####
i<-10
while(i>0){
  print("hello");
  i<-i-1
}
####条件执行####
####if-else结构####
if(is.character(grade))grade<-as.factor(grade)
if(!is.factor(garde))grade<-as.factor(grade) else print("grade already is a factor")
####ifelse结构####
outcome<-ifelse(score>0.5,"passed","failed")
####switch语句####
feelings<-c("sad","afraid")
for(i in feelings)
  print(
    switch(i,
           happy="1",
           afraid="2",
           sad="3",
           angry="4"
    )
  )
####分组计算描述性统计量####
myvars<-c("mpg","hp","wt")
aggregate(mtcars[myvars],by=list(am=mtcars$am),mean)
aggregate(mtcars[myvars],by=list(am=mtcars$am),sd)

####合并两个数据集，并增加一个识别两个数据集的变量####
databind <- rbind(aggdata1, aggdata2)
databind$status <- c(rep('non-CKD', nrow(aggdata1)), 
                     rep('CKD', nrow(aggdata2)))

####一维列联表####
mytable<-with(Arthritis,table(Improved))

####使用prop.table()将频数转化为比例值####
prop.table(mytable)

####将比例值转化为百分比####
prop.table(mytable)*100

####二维列联表####
mytable<-table(A,B)
mytable<-xtabs(~Treatment+Improved,data=Arthritis)

####边际频数####
margin.table(mytable,1)
####边际频率####
prop.table(mytable,1)
####各单元格所占比例####
prop.table(mytable)
####添加边际和####
addmargins(mytable)
####添加列边际和####
addmargins(prop.table(mytable,2),1)

####三维列联表####
mytable<-xtabs(~Treatment+Sex+Improved,data=Arthritis)
ftable(mytable)

####使用chisq.test()对二维表的行变量和列变量进行卡方独立性检验####
mytable<-xtabs(~Treatment+Improved,data=Arthritis)
chisq.test(mytable)

####Fisher精确检验，fisher.test()函数可以在任意行列数大于等于2的二维列联表上使用，但不能用于2*2的列联表####
mytable<-xtabs(~Treatment+Improved,data=Arthritis)
fisher.test(mytable)

####检验两个变量在第三个变量的每一层中是否独立####
mytable<-xtabs(~Treatment+Improved+Sex,data=Arthritis)#检验治疗情况和改善情况在性别的每一水平下是否独立
mantelhaen.test(mytable)

####二维列联表的相关性度量####
mytable<-xtabs(~Treatment+Improved,data=Arthritis)
assocstats(mytable)

####协方差和相关系数####
states<-state.x77[,1:6]
cov(states)
cor(states)#默认计算pearson积差相关系数
cor(states,method = "spearman")
####计算非方形的相关矩阵####
x<-states[,c("population","income","illiteracy")]
y<-states[,c("life exp","murder")]
cor(x,y)

####相关性检验####
cor.test(x,y,alternative = ,method = )

####独立样本的t检验 t检验属于参数检验####
library(MASS)#使用数据集
t.test(Prob~So,data=UScrime)#比较南方和非南方各州的监禁概率

####非独立样本的t检验####
####检验亚拉巴马州年轻男性和年长男性的失业率是否相同####
library(MASS)#使用数据集
sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime,t.test(U1,U2,paired = T))

####使用方差分析对对于两个的组进行比较####

####组间差异的非参数检验####

####OLS普通最小二乘回归：简单线性回归、多项式回归、多元线性回归####

####用lm()拟合回归模型####
myfit<-lm(formula,data)

####获得更多模型额外信息的函数####
summary(myfit)#展示拟合模型的详细结果
coefficients(myfit)#列出拟合模型的模型参数
confint(myfit)#提供模型参数的置信区间
fitted(myfit)#列出拟合模型的预测值
residuals(myfit)#列出拟合模型的残差值
anova()#生成一个拟合模型的方差分析表，或者比较两个或更多模型的方差分析表
vcov()#列出模型参数的协方差矩阵
AIC()#输出赤池信息统计量
plot()#生成评价拟合模型的诊断图
predict()#用拟合模型对新的数据集预测响应变量值

####简单线性回归####
fit<-lm(weight~height,data=women)
summary(fit)
fitted(fit)
residuals(fit)
plot(women$height,women$weight)
abline(fit)

####多项式回归####
fit2<-lm(weight~height+I(height^2),data=women)
plot(women$height,women$weight)
lines(women$height,fitted(fit2))

####多元线性回归####
states<-as.data.frame(states.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)
summary(fit)

####有交互项的多元线性回归####
fit<-lm(mpg~hp+wt+hp:wt,data=mtcars)
summary(fit)

####模型比较####
states<-as.data.frame(states.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit1<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)
fit2<-lm(Murder~Population+Illiteracy,data=states)
####用anova()函数比较####
anova(fit1,fit2)
####用AIC来比较模型####
AIC(fit1,fit2)

####向后回归法筛选变量####
library(MASS)

states<-as.data.frame(states.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)
stepAIC(fit,direction = "backward")

####查看子集的变量情况####
CKD_dose_gene<-matrix(1:24,ncol = 8)
mydose<-c(0, 0.4, 0.6, 0.8, 1.2, 1.6, 2, 2.4)
genetype<-c(1,2,3)
for(i in 1:8){
  for(j in 1:3){
    # i=1
    # j=1
    CKD_dose_gene[j,i]<-nrow(subset(b_HCY_new,CKD3==1 & Dose==mydose[i] & Genes==genetype[j]))
  }
}

####画带有标准差的均值柱状图（误差柱状图）####
aggdata3<-aggregate(b[,c(3,7,10,15)],by=list(b$CKD3),FUN=mean,na.rm=T)
aggdata3$sd <- aggregate(b[,c(3,7,10,15)],by=list(b$CKD3),FUN=sd,na.rm=T)
#若是两个组别
frame_errorbar <- data.frame(matrix(1:8, ncol = 4))[-(1:2), ]
#若是三个组别
frame_errorbar <- data.frame(matrix(1:12, ncol = 4))[-(1:3), ]
frame_errorbar <- rbind(frame_errorbar, cbind(aggdata3[ ,1:2], aggdata3[ ,2] - aggdata3[ ,6][ ,2], aggdata3[ ,2] + aggdata3[ ,6][ ,2]))
colnames(frame_errorbar) <- c('Group', 'FA_0', 'Lower', 'Upper')

ggplot(frame_errorbar, aes(x=Group, y=FA_0)) +
  geom_bar(aes(fill = Group), stat = 'identity', width = 0.3) +
  ylab("FA") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  scale_x_discrete(breaks=c("0","1"),labels=c("non-CKD","CKD")) +
  xlab("")+
  theme(axis.title.x = element_text(size = 25),axis.title.y = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 25),axis.text.y = element_text(size = 25))+
  theme(panel.grid.major=element_line(colour=NA),
        legend.position = 'none') +
  scale_fill_manual(breaks = c(0, 1), values = c('#06d6a0', '#ffd166'))
ggsave("E:/qiangqiangshixiong/结果/柱状图更新/x0wFA.jpg", width=15, height=10) 

####条件logistic回归####
library(epiDisplay)
library(survival)
data("VC1to1")
str(VC1to1)
clogit1 <- clogit(case ~ smoking + alcohol + rubber + strata(matset), data = VC1to1)   
clogit1
drop1(clogit1, test = "Chisq")
clogit2 <- clogit(case ~ alcohol + rubber + strata(matset), data = VC1to1)
clogit2
drop1(clogit2, test = "Chisq")
clogit3 <- clogit(case ~ alcohol + strata(matset), data = VC1to1)
AIC(clogit1, clogit2, clogit3)
summary(clogit3)
clogistic.display(clogit3)

####生存分析####
library(survival)
data("ovarian")
str(ovarian)
head(ovarian)
ovarian$resid.ds <- factor(ovarian$resid.ds, levels = c(1,2), labels = c("no", "yes"))
ovarian$rx <- factor(ovarian$rx, levels = c(1,2), labels = c("A", "B"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, levels = c("1","2"), labels = c("good", "bad"))
summary(ovarian$age)
ovarian$agegr <- cut(ovarian$age, breaks = c(0, 50, 75), labels = c("<=50", ">50"))
table(ovarian$agegr)
####创建生存对象####
surv.obj <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv.obj 
surv.all <- survfit(surv.obj ~ 1)
summary(surv.all)
summary(surv.all, censored = T)
plot(surv.all, mark.time = T)
surv.treat <- survfit(surv.obj ~ rx, data = ovarian)
summary(surv.treat)
####在同一个图中显示多条生存曲线有助于生存率的比较####
plot(surv.treat, mark.time = T, conf.int = T, lty = c(1, 2), col = c("blue", "red"))
legend(60, 0.3, legend = c("A", "B"), lty = c(1, 2), col = c("blue", "red"))
####检验两组生存率是否有统计学差异：log rank test(时序检验) ####
survdiff(surv.obj ~ rx, data = ovarian)
####将所有协变量包含进来建立Cox回归模型####
cox1 <- coxph(surv.obj ~ rx + resid.ds + agegr + ecog.ps, data = ovarian)
summary(cox1)
####使用函数step()基于AIC值进行变量选择####
step.cox <- step(cox1)
cox2 <- coxph(surv.obj ~ rx + resid.ds + agegr, data = ovarian)
####生存的预测####
####建立新的数据####
newdata <- data.frame(rx = c("A", "B"), resid.ds = c("no", "no"), agegr = c(">50", ">50"))
newdata
hr <- predict(cox2, newdata = newdata, type = "risk")
hr
hr[1]/hr[2]
###拟合新数据集的KM生存曲线####
cox.fit <- survfit(cox2, newdata = newdata, type = "kaplan-meier")
plot(cox.fit, lty = c(1,2), col = c(2,4))
title(main = "Cox survival curves by treatment for age > 50, no residual disease patients",
      xlab = "Duration in days",
      ylab = "Survival probability",
      las = 1)
legend(5, 0.3, c("Treatment A", "Treatment B"), lty = c(1, 2), col = c(2, 4))

####gam()函数拟合广义可加模型####
install.packages("gam")
library(gam)
fit <- gam(hyper ~ s(age), data = f1, family = binomial)
summary(fit)
plot(fit, se = T)

####roc()函数绘制ROC曲线并找出界值：设有数据f1，二分类因变量y, 连续自变量x ####
install.packages("pROC")
library(pROC)
rr <- roc(f1$y, f1$x)
plot(rr, print.thres = T, print.auc = T)

####判别分析####
####距离判别####
data(iris)
cor(iris[, 1:4])
####分别计算3种鸢尾花的4个指标的均值，作为各个类别的中心点####
m.setosa <- colMeans(iris[1:50, 1:4])
m.versicolor <- colMeans(iris[51:100, 1:4])
m.virginica <- colMeans(iris[101:150, 1:4])
####检验三种鸢尾花的协方差矩阵是否相等####
install.packages("biotools")
library(biotools)
boxM(iris[, -5], iris[, 5])
####分别计算三种类别下的协方差矩阵####
v.setosa <- cov(iris[1:50, 1:4])
v.versicolor <- cov(iris[51:100, 1:4])
v.virginica <- cov(iris[101:150, 1:4])
####计算每个样品与3个中心点的距离####
d.setosa <- mahalanobis(iris[, 1:4], m.setosa, v.setosa)
d.versicolor <- mahalanobis(iris[, 1:4], m.versicolor, v.versicolor)
d.virginica <- mahalanobis(iris[, 1:4], m.virginica, v.virginica)
d <- data.frame(d.setosa, d.versicolor, d.virginica)
index <- apply(d, MARGIN = 1, FUN = which.min)
####将数字换成标签####
type <- factor(index, labels = c("setosa", "versicolor", "virginica"))
####生成混淆矩阵####
table(type, iris$Species)
####找出三个被错判的样品的编号####
which(type == "virginica" & iris$Species =="versicolor")
####K最邻近判别####
set.seed(1234)
nrow(iris)
s <- sample(1:150, 100)
train <- iris[s, ]
test <- iris[-s, ]
cl <- train[, 5]
library(class)
####默认k=1####
iris.knn <- knn(train[, -5], test[, -5], cl)
iris.knn
confusion.matrix <- table(iris.knn, test[, 5])
confusion.matrix
accuracy <- sum(diag(confusion.matrix))/nrow(test)
accuracy
####调整参数k的值并比较正确率####
accuracy <- vector(length = 20)
for (i in 1:20) {
  iris.knn <- knn(train[, -5], test[, -5], cl, k = i)
  confusion.matrix <- table(iris.knn, test[, 5])
  accuracy[i] <- sum(diag(confusion.matrix))/nrow(test)
}
accuracy
plot(accuracy, type = "b", xlab = "k")
####Fisher 判别####
library(MASS)
iris.ld <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
iris.pred <- predict(iris.ld)
iris.pred$class 
table(iris.pred$class, iris$Species)
which(iris.pred$class == "virginica" & iris$Species == "versicolor")
which(iris.pred$class == "versicolor" & iris$Species == "virginica")
LD1 <- iris.pred$x[, 1]
LD2 <- iris.pred$x[, 2]
col <- as.numeric(iris$Species)
pch <- as.numeric(iris$Species)
plot(LD1, LD2, col = col, pch = pch)
table(iris$Species)
legend("top", legend = c("setosa", "versicolor", "virginica"), col = 1:3, pch = 1:3)
points(LD1[c(71, 84)], LD2[c(71, 84)], cex = 2)
points(LD1[134], LD2[134], cex = 2)
####Bayes判别####
install.packages("klaR")
library(klaR)
iris.bayes <- NaiveBayes(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)

####生存分析####
library(survival)
data("ovarian")
str(ovarian)
head(ovarian)
ovarian$resid.ds <- factor(ovarian$resid.ds, levels = c(1,2), labels = c("no", "yes"))
ovarian$rx <- factor(ovarian$rx, levels = c(1,2), labels = c("A", "B"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, levels = c("1","2"), labels = c("good", "bad"))
summary(ovarian$age)
ovarian$agegr <- cut(ovarian$age, breaks = c(0, 50, 75), labels = c("<=50", ">50"))
table(ovarian$agegr)
####创建生存对象####
surv.obj <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv.obj 
surv.all <- survfit(surv.obj ~ 1)
summary(surv.all)
summary(surv.all, censored = T)
plot(surv.all, mark.time = T)
surv.treat <- survfit(surv.obj ~ rx, data = ovarian)
summary(surv.treat)
####在同一个图中显示多条生存曲线有助于生存率的比较####
plot(surv.treat, mark.time = T, conf.int = T, lty = c(1, 2), col = c("blue", "red"))
legend(60, 0.3, legend = c("A", "B"), lty = c(1, 2), col = c("blue", "red"))
####检验两组生存率是否有统计学差异：log rank test(时序检验) ####
survdiff(surv.obj ~ rx, data = ovarian)
####将所有协变量包含进来建立Cox回归模型####
cox1 <- coxph(surv.obj ~ rx + resid.ds + agegr + ecog.ps, data = ovarian)
summary(cox1)
####使用函数step()基于AIC值进行变量选择####
step.cox <- step(cox1)
cox2 <- coxph(surv.obj ~ rx + resid.ds + agegr, data = ovarian)
####生存的预测####
####建立新的数据####
newdata <- data.frame(rx = c("A", "B"), resid.ds = c("no", "no"), agegr = c(">50", ">50"))
newdata
hr <- predict(cox2, newdata = newdata, type = "risk")
hr
hr[1]/hr[2]
####拟合新数据集的KM生存曲线####
cox.fit <- survfit(cox2, newdata = newdata, type = "kaplan-meier")
plot(cox.fit, lty = c(1,2), col = c(2,4))
title(main = "Cox survival curves by treatment for age > 50, no residual disease patients",
      xlab = "Duration in days",
      ylab = "Survival probability",
      las = 1)
legend(5, 0.3, c("Treatment A", "Treatment B"), lty = c(1, 2), col = c(2, 4))

####gam()函数拟合广义可加模型####
install.packages("gam")
library(gam)
fit <- gam(hyper ~ s(age), data = f1, family = binomial)
summary(fit)
plot(fit, se = T)

####roc()函数绘制ROC曲线并找出界值：设有数据f1，二分类因变量y, 连续自变量x ####
install.packages("pROC")
library(pROC)
rr <- roc(f1$y, f1$x)
plot(rr, print.thres = T, print.auc = T)

####去掉离群点####
z <- temp3$SAM.2.ng.ml
boxplot(z)
boxplot.stats(z)$out
zlist<-which(z %in% boxplot.stats(z)$out)
temp4 <- temp3[-zlist, ]
boxplot(temp4$SAM.2.ng.ml)

####随机抽样####
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
####ggplot画图去掉格子和灰色背景####
theme_bw() +
  theme(panel.grid =element_blank())
####ggplot画图调整坐标轴刻度和标签大小####
theme(axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15))

####ggplot绘制两个连续变量的平滑拟合曲线####
ggplot(final1, aes(hcy, SAH.ng.ml)) +
  geom_smooth(method = "gam", fomula = y ~ s(x))+
  theme_bw() +
  theme(panel.grid =element_blank())+
  theme(axis.title.x = element_text(size = 15, family = "myFont"),axis.title.y = element_text(size = 15, family = "myFont"))+
  theme(axis.text.x = element_text(size = 15, family = "myFont"),axis.text.y = element_text(size = 15, family = "myFont"))
ggsave("E:/课题/keti/探索中的图/SAH_HCY.jpg",width=15,height=10)

####分组做正态性检验####
tapply(x,group,shapiro.test)

####将连续变量按照分位数进行分段####
quantile_hcy_4 <- quantile(final1$hcy, c(1/4, 2/4, 3/4))
final1$hcy_4 <- ifelse(final1$hcy <= quantile_hcy_4[1],0,  
                       ifelse(quantile_hcy_4[1] < final1$hcy & final1$hcy <= quantile_hcy_4[2],1,
                              ifelse(quantile_hcy_4[2] < final1$hcy & final1$hcy <= quantile_hcy_4[3],2,3)))
table(final1$hcy_4)

####制作回归模型表（自变量和因变量都为连续变量）####
#粗模型
model <- lm(SAH.ng.ml ~ hcy, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p1 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])

model <- lm(SAM.2.ng.ml ~ hcy, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p2 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])

model <- lm(SAH_SAM ~ hcy, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p3 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])

model <- lm(SAM_SAH ~ hcy, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p4 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])
crude <- rbind(p1, p2, p3, p4)

#调整模型
model<-lm(SAH.ng.ml ~ hcy+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p1 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])

model<-lm(SAM.2.ng.ml ~ hcy+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p2 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])

model<-lm(SAH_SAM ~ hcy+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p3 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])

model<-lm(SAM_SAH ~ hcy+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[2,c(1,4)],3)
r2 <- round(confint(model)[2,],3)
p4 <- c(paste0(r1[1], '(', r2[1],",", r2[2], ')'), r1[2])

adjusted <- rbind(p1, p2, p3, p4)

continuous <- as.data.frame(cbind(crude, adjusted))
colnames(continuous) <- c("crude β(95%CI)", "p", "adjusted β(95%CI)", "p")
rownames(continuous) <- c("SAH", "SAM", "SAH/SAM", "SAM/SAH")
write.csv(continuous,"E:/课题/keti/探索中的表/回归表hcy_continuous.csv.csv",row.names=T)
####制作回归模型表（自变量为分类变量，因变量为连续变量）####
#粗模型
model <- lm(SAH.ng.ml ~ hcy_3, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p1 <- rbind(p10, p11)

model <- lm(SAM.2.ng.ml ~ hcy_3, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p2 <- rbind(p10, p11)

model <- lm(SAH_SAM ~ hcy_3, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p3 <- rbind(p10, p11)

model <- lm(SAM_SAH ~ hcy_3, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p4 <- rbind(p10, p11)

crude <- rbind(p1, p2, p3, p4)

#调整模型
model<-lm(SAH.ng.ml ~ hcy_3+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p1 <- rbind(p10, p11)

model<-lm(SAM.2.ng.ml ~ hcy_3+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p2 <- rbind(p10, p11)

model<-lm(SAH_SAM ~ hcy_3+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p3 <- rbind(p10, p11)

model<-lm(SAM_SAH ~ hcy_3+sex+age+bmi+sbp+dbp+tg+tcho+fa,family=gaussian(link = "identity"),data= final1)
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])
p4 <- rbind(p10, p11)

adjusted <- rbind(p1, p2, p3, p4)

three <- as.data.frame(cbind(crude, adjusted))
colnames(three) <- c("crude β(95%CI)", "p", "adjusted β(95%CI)", "p")
rownames(three) <- c("SAH1", "SAH2", "SAM1", "SAM2", "SAH/SAM1", "SAH/SAM2", "SAM/SAH1", "SAM/SAH2")
write.csv(three,"E:/课题/keti/探索中的表/回归表hcy_three.csv",row.names=T)
####β值和p值对应连接####
model <- lm(SAH.ng.ml ~ hcy_3, family=gaussian(link = "identity"), data=final1) 
summary(model)
r1 <- round(summary(model)$coefficients[c(2,3),c(1,4)],3)
r2 <- round(confint(model)[c(2,3),],3)
p10 <- c(paste0(r1[1,1], '(', r2[1,1],",", r2[1,2], ')'), r1[1,2])
p11 <- c(paste0(r1[2,1], '(', r2[2,1],",", r2[2,2], ')'), r1[2,2])

####画带误差线的折线图####
ggplot(tgc, aes(x=thickness,   
                y=temperature,colour=factor(Laser.energy),shape=factor(Laser.energy),group=Laser.energy)) + 
  geom_errorbar(aes(ymin=temperature-se, ymax=temperature+se),colour="black",width=.1) +
  geom_line()+
  geom_point(size=3,fill="white")+
  xlab("thickness[mm]")+
  ylab("temperature[℃]")+
  ggtitle("LASER ALL-CERAMIC CROWN REMOVE")+
  scale_colour_discrete( name  ="Laser energy",
                         breaks=c(300,250,200,150,100,50),
                         labels=c("300 mJ", "250 mJ","200 mJ","150 mJ","100 mJ","50 mJ"))+
  scale_shape_discrete( name  ="Laser energy",
                        breaks=c(300,250,200,150,100,50),
                        labels=c("300 mJ", "250 mJ","200 mJ","150 mJ","100 mJ","50 mJ"))
####计算偏度####
library(moments)
skewness(m1$residuals) #计算残差偏度

####BOX-COX变换（因变量为非正态变量想做线性回归）####
library(moments)
library(MASS)
m1 <- lm(SAH.ng.ml~hcy, data = final1)

hist(m1$residuals)#绘制残差频率分布图

skewness(m1$residuals)#计算偏度 结果1.89

b <- boxcox(SAH.ng.ml~hcy, data = final1)

b

lambda <- b$x

lik <- b$y

bc <- cbind(lambda, lik)

bc[order(-lik),]#结果λ=-0.3时lik值最大，因此λ取值-0.3

m2 <- lm(SAH.ng.ml^(1/2) ~ hcy, data=final1)

hist(m2$residuals)

skewness(m2$residuals)#结果-0.89

summary(m2)
########查看数据集每一列的缺失情况######
missing <- colSums(is.na(mydata))
##########绘制回归曲线########
install.packages("basicTrendline")
library(basicTrendline)
x <- c(1, 3, 6,  9,  13,   17)
y <- c(5, 8, 11, 13, 13.2, 13.5)
trendline(x, y, model="line2P", ePos.x = "topleft", summary=TRUE, eDigit=5)
trendline(x, y, model="line3P", CI.fill = FALSE, CI.color = "black", CI.lty = 2, linecolor = "blue")
trendline(x, y, model="log2P", ePos.x= "top", linecolor = "red", CI.color = NA)
trendline(x, y, model="exp2P", show.equation = TRUE, show.Rpvalue = FALSE)


#########创建表并图解灵敏度、特异度、阳性预测值和阴性预测值########
table1 <- as.table(cbind(c(80,20), c(10,90)))
dimnames(table1) <- list("检测结果"=c("阳性","阴性"),"金标准"=c("有病","无病"))
mosaicplot(t(table1),col = c("red", "white"),main = " ")

table2 <- as.table(cbind(c(80,20), c(190,1710)))
dimnames(table2) <- list("检测结果"=c("阳性","阴性"),"金标准"=c("有病","无病"))
mosaicplot(t(table2),col = c("red", "white"),main = " ")
######## 建立logistic回归模型,并展示模型参数 ########
data(infert)
fit <- glm(case ~ induced + spontaneous, family = binomial, data = infert)
library(epiDisplay)
logistic.display(fit)
#绘制logistic模型的roc曲线
lroc(fit,line.col = "red",lwd = 3)

##########用ggplot画饼图###############
library(ggplot2)
library(scales)
#CSPPT_NCC_Non_CKD
table(ncc_0$c677t)
df <- data.frame(
  group = c("CC", "CT", "TT"),
  value = c(476, 945, 502)
)
head(df)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
pie + blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = c(502, 945, 476)), size=10)+
  scale_fill_manual(values=c("#2a9d8f", "#ffb703", "#d00000"))+
  ggtitle("CSPPT_NCC_Non_CKD")
ggsave("E:/qiangqiangshixiong/饼图/1.jpg",width=15,height=10)

################ 求相关系数 #############
library(psych)
cont.vars <- as.data.frame(cbind(mydata$VB5.ng.ml, mydata$age, mydata$sex, mydata$bmi, mydata$sbp, mydata$dbp,
                                 mydata$tcho, mydata$tg, mydata$hdl, mydata$fa, mydata$hcy,mydata$b12,mydata$egfr.v0,mydata$center,mydata$c677t,mydata$smk.grp,mydata$alh.grp))
colnames(cont.vars) <- c('VB5.ng.ml', 'age', 'sex', 'bmi', 'sbp', 'dbp',
                         'tcho', 'tg', 'hdl', 'fa', 'hcy' ,'b12', 'egfr.v0', 'center', 'c677t', 'smk.grp', 'alh.grp')
is.na(cont.vars)
cont.vars <- na.omit(cont.vars)
dim(cont.vars)
correlation <- cor(cont.vars, method = "pearson")
write.csv(correlation,"E:/ziyishixiong/VB5/correlation.csv",row.names=T)

corr.test(cont.vars)

######################### 均值柱状图 ################### 
mean <- aggregate(mydata$V0_cys, by=list(mydata$ckd), FUN=mean, na.rm=T)
mydata_res <- data.frame(mean)
mydata_res$Group.1 <- as.factor(mydata_res$Group.1)

ggplot(mydata_res, aes(x=Group.1, y=x, fill=Group.1)) +
  geom_bar(stat="identity",position="dodge",color="black", width=.4) +
  xlab("group")+
  ylab("cysteine")+
  theme_bw() +
  theme(panel.grid =element_blank()) +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  theme(legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))+
  scale_fill_discrete(labels=c("Non-CKD","CKD"))+
  labs(fill="group") +
  scale_fill_manual(values = c('#2a9d8f', '#e63946'), 
                    labels = c("Non-CKD","CKD"))
ggsave("E:/qiangqiangshixiong/CKD课题/结果/cysteine.jpg",width=10,height=10)
##################### 平滑曲线 ########################
setwd("E:/ziyishixiong/VB5/基线平滑曲线")
library(mgcv)
smooth_plot <- function(x, y, datax) {
  dev.new()
  tiff(file = paste0(y, ".tiff"), width = 6000, height =6000, res = 1200, compression = "lzw")
  formula1 <- paste0(y, '~', 's(', x, ',', 'fx = T', ',', 'k = 4', ')')
  formula1 <- as.formula(formula1)
  lm1 <-  gam(formula1, data = datax, family = gaussian(link = 'identity'))
  intercept <- summary(lm1)$p.coeff
  plot(lm1, lwd = 1, shade = F, shift = intercept,
       xlab = x, ylab = y)
  dev.off()
}
smooth_plot('VB5', 'age', mydata)

####################### 判断两列数据是否完全相同 #####################
x1 <- c(0,0,1,1)
x2 <- c(0,1,0,1)
logic <- data.frame(x1,x2)
logic$and <- as.numeric(x1 & x2)
logic$and <- as.numeric(x1 == x2)
#################### 求残差 ########################
residual<- y- predict(model, data)
################### 求多列的残差 ##################
x.var <- colnames(mydata)[35:54]
residual_x_var <- ''
for (i in 1:length(x.var)) {
  residual_x_var[i] <- paste0('residual_', x.var[i])
  glmformula <- as.formula(paste0(x.var[i], '~sex + center+ bmi+age+I(age^2)'))
  glmfit <- glm(glmformula, family = gaussian(link='identity'), data = mydata)
  
  mydata[, residual_x_var[i]] <- mydata[, x.var[i]] - predict(glmfit, mydata)
  
}

residual_vars <- colnames(mydata)[55:74]




