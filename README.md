Comparing-Logistic-Coefficients-Across-Groups-Methods
=====================================================

#Code for comparing logistic coefficients

Allisonfxnboth2<-function(y,x1,x2,df,condition){
  this_data<-df[condition,]  
  attach(this_data)
  logitt<-glm(y~x1+x2,data=this_data,family="binomial")##run the logistic regression for male group
  logLik<-logLik(logitt)
  detach(this_data)
  mylist<-list(groupsum=summary(logitt), logLik=logLik)
  mylist
}

##Next Wald fuctions. Waldnew is used in Wald_any
Wald_any<-function(group0,group1,num){
  bG1<-group1$groupsum$coefficients[num,1] # coefficient for articles 
  sd1<-group1$groupsum$coefficients[num,2] #SD for articles
  bG0<-group0$groupsum$coefficients[num,1] # coefficient for articles 
  sd0<-group0$groupsum$coefficients[num,2] #SD for articles
  T1<-Waldnew(bG0,bG1,sd0,sd1) #gives statistic 5.39 in this case
  T1
}
Waldnew<-function(bG0,bG1,sd0,sd1){
  num<-(bG0-bG1)^2
  den<-(sd0)^2+(sd1)^2
  stat<-num/den
  pvalue<-pchisq(stat,1,lower.tail=FALSE)
  mylist<-list(stat=stat,pvalue=pvalue)
  mylist
}
Allison_coeff_con<-function(y,x1,x2,G,df){
  attach(df)
  h1<-hetglm(y~x1+x2+G|G, data=df, family=binomial) #G is female(gender), or group category
  lambda<-h1[[1]]$scale
  delta<-(1 - exp(lambda))/ exp(lambda) 
  int <- 1+delta*G
  x11 <- x1*int
  x22 <- x2*int
  G <- G*int
  coefcon <- glm(y~-1+int+x11+x22+G, data=df, family=binomial)
  logLikcc<-logLik(coefcon)
  detach(df)
  p_value_chi<-summary(h1)$coefficients$scale[1,4] #note chi-square stat is (Deltacoef/sddelta)^2, with 1 df. z-test is approx same result
  mylist2<-list(coefcon=summary(coefcon),delta=delta, del_p_value_chi=p_value_chi,logLikcc=logLikcc)
  mylist2
  
}

##logliktest
LogLikTest<-function(group1,group0,groupcc){
  Lcc<-groupcc$logLikcc[[1]]
  dfLcc<-attr(groupcc$logLikcc,"df")+1 #7 df. but should be 8, counting delta, so add 1 df
  SumL<-group1$logLik[[1]]+group0$logLik[[1]] #just give log likelihood
  dfSumL<-attr(group1$logLik,"df")+attr(group0$logLik,"df")#6+6, so 12 df total
  diffLL<-abs(2*(Lcc-(SumL)))
  totaldf<-dfSumL-dfLcc
  pvalueLL<-pchisq(diffLL,totaldf,lower.tail=FALSE)
  mylist3<-list(diffLL=diffLL,totaldf=totaldf,pvalueLL=pvalueLL)
  mylist3
}
#group0 and group1 are from AllisonfxnBoth
allisonall2<-function(y,x1,x2,df,group0,group1,num,alpha){
  wald.out<-Wald_any(group0,group1,num=num)
  if(wald.out$pvalue > alpha){
    print("Difference is not significant for given coeff, Wald result is: ")
    print(wald.out)
    wald.out$pvalue      
  }else{
    print("Difference btwn given coeff is significant, constrained model is:")
    groupcc<-Allison_coeff_con(y,x1,x2,df$G,df) #Note G is the vector with both groups in it, 0s and 1s
    print(groupcc)
    likelihood.out<-LogLikTest(group0,group1,groupcc) 
    if(likelihood.out$pvalueLL > alpha){
      print("delta not significant,can use naive comparison, LRT result is: ")
      print(likelihood.out)   
    }else{
      print("delta is significant,there exists residual variation, LRT result is:")
      print(likelihood.out)
    }
  }
}
#the function that combines it all
allison3<-function(y,x1,x2,G, num=num, alpha=0.05){
  df<-as.data.frame(cbind(y,x1,x2,G))
  group0<-Allisonfxnboth2(y,x1,x2,df, which(G==0))
  print(group0)
  group1<-Allisonfxnboth2(y,x1,x2,df, which(G==1))
  print(group1)
  allisonall2(y,x1,x2,df,group0,group1,num,alpha) #Significant for num=2,not num=3
}
#--------Scott Long part
#Just splits the groups and variables of interest into 2 separate dataframes #num is in x1 range that u want to try
MakeFrames<-function(y,x1,x2,G,xseq=xseq){ #xseq is just a #
  mydf<<-as.data.frame(cbind(G,y,x1,x2)) #put G var first
  group0df<-mydf[which(mydf$G==0),]  
  #print(head(group0df))
  group1df<-mydf[which(mydf$G==1),]  
  attach(group0df)
  group0glm <<- glm(y~x1, data=group0df, family=binomial) # NOTE GLOBAL
  print(summary(group0glm))
  print("The model predicts y for group0 using only x1:")
  print(exp(summary(group0glm)$coef[2,1]))
  print("The predicted probability of y=1 for group0 with x1=num  is")
  g0.pp<- predict(group0glm, newdata=data.frame(x1=xseq), type="response")
  print(g0.pp)
  detach(group0df)
  
  attach(group1df)
  group1glm <<- glm(y~x1, data=group1df, family=binomial) #NOTE GLOBAL
  print(summary(group1glm))
  print("The model predicts y for group1 using only x1:")
  print(exp(summary(group1glm)$coef[2,1]))
  print("The predicted probability of y=1 for group1 with x1=num  is")
  g1.pp<- predict(group1glm, newdata=data.frame(x1=xseq), type="response")
  print(g1.pp)
  detach(group1df)
  #z(x1,group0glm,group1glm)
  
  listings<-list(group0glm=group0glm,group1glm=group1glm)
  listings
  #xseq<<-seq(min(x1), max(x1),length.out=50) #sequence for x1, z_gfactor
  z.test1(xseq, group0glm,group1glm) 
}  

pi.diff <- function(xseq, group0glm, group1glm) {
  a <- predict(group0glm, newdata=data.frame(x1=xseq), type="response")
  b <- predict(group1glm, newdata=data.frame(x1=xseq), type="response")
  d <- a-b
  return(d)
}

std.err.pp <- function(xseq, group0glm, group1glm) {
  a <- predict(group0glm, newdata=data.frame(x1=xseq), se.fit=TRUE, type="response")
  b <- predict(group1glm, newdata=data.frame(x1=xseq), se.fit=TRUE, type="response")
  d <- sqrt((a$se.fit^2)+(b$se.fit^2))
  return(d)
}

z <- function(xseq, group0glm, group1glm) {
  a <- pi.diff(xseq, group0glm,group1glm)/std.err.pp(xseq, group0glm, group1glm)
  return(c(a, pnorm(a)))
}

z.test1 <- function(xseq, group0glm, group1glm) {
  a <- pi.diff(xseq, group0glm, group1glm)/std.err.pp(xseq, group0glm, group1glm)
  ci <- pi.diff(xseq, group0glm, group1glm)+c(-1,1)*qnorm(0.975)*std.err.pp(xseq, group0glm, group1glm)
  z.test <- pnorm(a)
  Y <- ifelse(z.test >= .99, "Y", "N")
  return(cat("z=", a, "\n", 
             "When applying the z.test:",  z.test, "\n",
             "The CIs are:", ci, "\n", 
             "Can we reject the null hypothesis (at the .01 level)?", Y, "\n"))}
MakeGraphs<-function(x1,xseq=xseq){
  #xseq<-seq(min(x1), max(x1),length=50) #Range of x1, can be modified
  y.group0 <- predict(group0glm, newdata=data.frame(x1=xseq),type="response")
  y.group1 <- predict(group1glm, newdata=data.frame(x1=xseq), type="response")
  plot(xseq, y.group0, col="blue", bg=3, ylab="Pr(Y=1)", xlab="X1", 
       ylim=c(0,1), pch=16, cex=.7)
  points(xseq, y.group1, col="deeppink3", pch=16,cex=.7)
  abline(h=c(.2, .4, .6, .8), lty=3)
  legend("bottomleft", bty="n", c("Group0", "Group1"), lwd=c(2, 2), 
         col=c("blue", "deeppink3"))
  axis(side=1, at=c(-2,-1,0,1,2))
  title(main="Probability Y=1 for Group 0 vs Group 1")
  #pdf(file="fig4LP.pdf")
  
  
} 

ConGraph<-function(x1,xseq=xseq){
  ci.upper <- pi.diff(xseq, group0glm, group1glm)+qnorm(0.975)*std.err.pp(xseq, group0glm, group1glm)
  ci.lower <- pi.diff(xseq, group0glm, group1glm)-qnorm(0.975)*std.err.pp(xseq, group0glm, group1glm)
  plot(xseq, pi.diff(xseq, group0glm, group1glm), ylab="Pr(hs|group0)-Pr(hs|group1)", 
       xlab="x1", ylim=c(-.1,1))
  polygon(c(xseq, rev(xseq)), c(ci.upper, rev(ci.lower)), col='steelblue')
  points(xseq, pi.diff(xseq, group0glm, group1glm), pch=16, lty=2)
  lines(xseq, ci.upper, col="steelblue")
  lines(xseq, ci.lower, col="steelblue")
  abline(h=c(.2,.4,.6,.8), lty=3)
  abline(h=0, col="red", lwd=3)
  #axis(side=1, at=c(2,4,6,8,10,12,14,16,18,20,25,30,35,40,45,50))
  legend("topleft", bty="n", c("Group0-Group1 difference", "95% Confidence Interval"), 
         col=c("black", "steelblue"), lwd=c(2.5, 2.5), pch=c(16,NA))
  title(main="Group Differences in Probability of Y=1")
}
