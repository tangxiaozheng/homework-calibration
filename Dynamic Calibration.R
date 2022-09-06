library(readxl)
library(xlsx)
path <- "D:\\Document\\新建文件夹"#the path of workspace
setwd(path)
STDlist <- list()#the list of every concentration STD
ALLSTD <- data.frame() #the chart of all STD
jzsf <-
  read_xlsx("jzsf.xlsx") #the original file where there are only needed data exists #
coefficient_for_DynALL<-c(2E-07,0.0002,0.3284)
getRRFandTime <-
  function(jzsf, c, isSTD = 1)
    #the function to extract data, if isSTD=1,will extract STDdata,or extract others#
  {
    i = 1
    j = 1
    temprrf <- vector()
    
    temptime <- vector()
    temptype <- vector()
    tempcon <- vector()
    while (i <= nrow(jzsf))
    {
      if (isSTD == 1)
      {
        if (grepl("STD", jzsf[i, 2]) && (c %in% jzsf[i, 5]))
          #check what we need,if you want this program more usability, you should change
          #the number of jzsf[,2] into jzsf[,"string"] 
        {
          temprrf[j] = unlist(jzsf[i, "RRF"])
          temptime[j] = unlist(jzsf[i, "TIME"])
          temptype[j] = unlist(jzsf[i, 2])
          tempcon[j] = c
          j = j + 1
        }
      }
      if (isSTD == 0)
      {
        if (!grepl("STD", jzsf[i, 2]) && (c %in% jzsf[i, 5]))
        {
          temprrf[j] = unlist(jzsf[i, "RRF"])
          temptime[j] = unlist(jzsf[i, "TIME"])
          temptype[j] = unlist(jzsf[i, 2])
          tempcon[j] = c
          j = j + 1
        }
      }
      i = i + 1
    }
    temp = data.frame(
      "Type" = temptype,
      "concentration" = tempcon,
      "RRF" = temprrf,
      "TIME" = temptime
    )
    return(temp)
  }
getResult <- function(jzsf, c = -10, isSTD = 1)
    #save the data as list for convenience and append to the xlsx file
    #if the STD has other concentration, this function need to change
  {
    if (isSTD == 1)
    {
      result <- list(
        "twenty" = getRRFandTime(jzsf, 20),
        "forty" = getRRFandTime(jzsf, 40),
        "eighty" = getRRFandTime(jzsf, 80),
        "one_six_zero" = getRRFandTime(jzsf, 160),
        "three_two_zero" = getRRFandTime(jzsf, 320),
        "six_four_zero" = getRRFandTime(jzsf, 640),
        "twelve_eighty" = getRRFandTime(jzsf, 1280)
      )
      #for (i in names(result))
      #{
      #write.xlsx(result[[i]],file = "jzsf.xlsx",sheetName = i,append = TRUE,)
      #} all comments before write or for are order to stop this programme write down the
      #original file, if not necessary, for one file, write should only run once.
    }
    
    else
    {
      result <- getRRFandTime(jzsf, c, isSTD = 0)
      #write.xlsx(result,file="jzsf.xlsx",sheetName= as.character(c),append = TRUE)
    }
    return(result)
  }
#init<-function () #this function has bug, we'd better use getresult twice instead of using it.
#{
STDlist <- getResult(jzsf)#save all std, classified by concentration
for (i in names(STDlist))
{
  ALLSTD <- rbind(ALLSTD, STDlist[[i]])
}
get_root<-function(a,b,c,RRFC)
  #calculate the quadratic equations like aX^2+bX+c,RRFC=RRF*C
{
  tmp<-b*b-4*a*(c-RRFC)
  if (tmp<0)
    return (Inf)
  x1<-((-b)+sqrt(tmp))/(2*a)
  #x2<-((-b)-sqrt(tmp))/(2*a)
  return (x1)
}

get_data_root<-function(a,b,c,sample)
  #you can use this function to calculate the quadratic equations like the previous function. 
  #sample means data.frame of sample
{
  RRFC<-sample[,"concentration"]*sample[,"RRF"]
  re<-c()
  for (i in 1:nrow(sample))
    re<-append(re,get_root(a,b,c,RRFC[[i]]))
  return (re)
}
#sample<-getResult(jzsf,c=30,isSTD=0) this comment is order to get sample, when needing,restore it



#upon this is the preparation ,below this is the calculation


Stat_All <-mean(ALLSTD$RRF)
  #this order is for calculation the Stat_ALL
   
#close <-list()
#before you use this program, you should enter close firstly.
#unless you needn't use the functions about close or closest.
#also, using the Formal Variables,you can change this variables as other name.

#the onlt thing you should remember is that
# if you use close, you may enter 2 data.frame in this list.
# if you use closest, you may enter only 1 data.frame.


Stat_Close <- function(close)
{
  close1 <- close[[1]]
  close2 <- close[[2]]
  tmp <- rbind(close1, close2)#bind them and get the mean
  return (mean(tmp[["RRF"]]))
}

Stat_Closest <- function(close)
  #to avoid error about "close", I encapsulate it with a function
{
  return (mean(close$RRF))
}

Stat_RRF<-function(sample,close,closest)
  #output the result of whole StatRRF
{
  tmp<-list(close,closest)
  Stat_All_tmp<-rep(Stat_All,6)
  nRRF<-data.frame(
    "ALL"= Stat_All_tmp,
    "close"=Stat_Close(tmp),
    "closest"=Stat_Closest(closest)
  )
  AAIS<-sample$RRF*sample$concentration/500
  result<-AAIS*500/nRRF
  return (result)
}
#upon this is statRRF
Dyn_Close <- function(close)
{
  Y <- c()
  X <- c()
  Y <-(close[[1]]$RRF + close[[2]]$RRF) / 2
  X <-(close[[1]]$TIME + close[[2]]$TIME) / 2
  return (lm(Y ~ I(X ^ 2)+X))
}
Dyn_ALL<-function(sample)
{
  a<-coefficient_for_DynALL[1]
  b<-coefficient_for_DynALL[2]
  c<-coefficient_for_DynALL[3]
  result<-a*sample$TIME^2+b*sample$TIME+c
  return (result)
}
Dyn_Closest <- function(close)
  #to avoid error, I encapsulate it with a function
{
  lm(close$RRF ~ close$TIME)
}
Dyn_RRF<-function(sample,close,closest)
  # output the result of whole StatRRF
{
  tmp<-Dyn_Closest(closest)[[1]]
  est<-tmp[1]+sample$TIME*tmp[2]
  tmpclose<-list(close,closest)
  tmp<-Dyn_Close(tmpclose)[[1]]
  ose<-tmp[1]+tmp[2]*sample$TIME^2+tmp[3]*sample$TIME
  nRRF<-data.frame(
    "ALL"=Dyn_ALL(sample),
    "close"=ose,
    "closest"=est
  )
  AAIS<-sample$RRF*sample$concentration/500 #AAIS=A sample / A IS
  result<-AAIS*500/nRRF
  return(result)
}
#upon this is DynRRF, if you want to do the regression in the excel
#you can change the return order to return Y and X, which is easy to send into excel
#the Cal ways can also do that.

Stat_CalPart1 <- function(STDlist, method)#the result is coefficients
  #method = 1 , 2, 3, 4
  #way 1,3 is linear
  #way 1,2 has a forced 0-intercept
{
  Y<-c()
  X<-c()
  for (i in STDlist)
  {
    Y <- append(Y, mean(i$RRF*i$concentration[1])) # mean RRF*c of each concentration level
    X <- append(X, i$concentration[1]) #concentration
  }
  #get concentration and mean RRF
    #return(list(Y,X)) this comment will be helpful if you want to do the regression in the excel
    if (method == 1)
      return (lm(Y ~ X + 0))
    if (method == 2)
      return (lm(Y ~ I(X ^ 2) + X + 0))
    if (method == 3)
      return (lm(Y ~ X))
    if (method == 4)
      return (lm(Y ~ I(X ^ 2) + X))
  # 0 means forcing 0-intercept
}
Stat_Cal<-function(STDlist,sample)
  #output the result of whole StatRRF
{
  b<-Stat_CalPart1(STDlist,2)[[1]]
  a<-Stat_CalPart1(STDlist,3)[[1]]
  c<-Stat_CalPart1(STDlist,4)[[1]]
  RRFC<-sample[,"concentration"]*sample[,"RRF"]
  for (i in 1:nrow(sample))
  {
    bxtmp<-get_root(b[1],b[2],0,RRFC[i])
    cxtmp<-get_root(c[2],c[3],c[1],RRFC[i])
  }
  result<-data.frame(
    "bx"=RRFC/Stat_CalPart1(STDlist,1)[[1]],
    "bx+a"=(RRFC-a[1])/a[2],
    "bx^2+ax"=bxtmp,
    "bx^2+ax+c"=cxtmp
  )
  return (result)
}
Dyn_CalPart1 <- function(time, method)
  #this function is one part of DynCal, to calculate the coefficient between RRFC and concentration
{
  time<-as.numeric(time)
  c <- c()
  tmp <- list()
  tmp2 <- c()
  for (i in STDlist)
    # do regression about time on each concentration,save the coefficients into tmp
  {
    c <- append(c, i$concentration[1]) #get concentration
    tmp <- append(tmp, lm(i$RRF ~ i$TIME)$coefficients) #tmp get the coefficient RRF-t
  }
  for (i in seq(1,14,by=2))
    tmp2 <- append(tmp2, tmp[[i + 1]] * time + tmp[[i]])
  #calculate on each time, each concentration's RRF
  #then do regression between RRF(in theory) with concentration
  Y <- tmp2#Y=RRF
  X <- c#concentration
  Y<-Y*X #RRFC
  if (method == 1)
    return (lm(Y ~ X + 0))
  if (method == 2)
    return (lm(Y ~ I(X ^ 2) + X + 0))
  if (method == 3)
    return (lm(Y ~ X))
  if (method == 4)
    return (lm(Y ~ I(X ^ 2) + X))
}

Dyn_CalPart2<-function(time,method)
  #this function can sort the result
  #place one vector of time in it, it will return a list about coefficients 
  
  #method = 1 , 2, 3, 4
  #way 1,3 are linear
  #way 2,4 are squares
  #way 1,2 has a forced 0-intercept
{
  result<-list()
  for (i in time)
  {
    tmp<-Dyn_CalPart1(i,method)
    result<-append(result,tmp$coefficients)
  }
    
  return (result)
}
Dyn_CalPart3<-function(sample,method)
  #this function can calculate the concentration and need the sample,
  #not the sample's time  as the varible 
{
  RRFC<-sample[,"RRF"]*sample[,"concentration"]
  result<-c()
  if (method==1)#y=bx
  {
    re<-Dyn_CalPart2(sample[["TIME"]],1)
    for (i in 1:nrow(sample))
      result<-append(result,RRFC[[i]]/re[[i]])
  }
  
  if (method==2)#y=bx^2,ax
  {
    re<-Dyn_CalPart2(sample[["TIME"]],2)
    for (i in 1:nrow(sample))
    {
      j=i*2
      result<-append(result,get_root(re[[j-1]],re[[j]],0,RRFC[[i]]))
    }
  }
  if (method==3)#y=bx+a
    {
      re<-Dyn_CalPart2(sample[["TIME"]],3)
      for (i in 1:nrow(sample))
      {
        j=i*2
        result<-append(result,(RRFC[[i]]-re[[j-1]])/re[[j]])
      }
    }
  if (method==4)#y=bx^2+ax+C
  {
    re<-Dyn_CalPart2(sample[["TIME"]],4)
    for (i in 1:nrow(sample))
    {
      j=3*i
      result<-append(result,get_root(re[[j-1]],re[[j]],re[[j-2]],RRFC[[i]]))
    }
  }
  return (result)
}
Dyn_Cal<-function(sample)
  #output the result of whole StatRRF
{
  result<-data.frame(
    "bx"=Dyn_CalPart3(sample,1),
    "bx^2+ax"=Dyn_CalPart3(sample,2),
    "bx+a"=Dyn_CalPart3(sample,3),
    "bx^2+ax+c"=Dyn_CalPart3(sample,2)
  )
  return (result)
}
output<-function(sample,close,closest)
  #output everything
{
  result<-list(
  "StatRRF"=Stat_RRF(sample,close,closest),
  "DynRRF"=Dyn_RRF(sample,close,closest),
  "StatCal"=Stat_Cal(STDlist,sample),
  "DynCal"=Dyn_Cal(sample)
  )
  return (result)
  
}
