#this program is made for verify the usability of 
#"Dynamic Calibration Approach for Determining Catechins and Gallic Acid in Green Tea Using LC-ESI/MS". 
# this program can not work on its own. the "Dyn_RFFAll" must be used by Excel"
#and the excel must be prepared like the sample.

#reference:
#Bedner, Mary, and David L. Duewer. ‘Dynamic Calibration Approach for Determining Catechins and Gallic Acid in Green Tea Using LC-ESI/MS’. 
#Analytical Chemistry, vol. 83, no. 16, Aug. 2011, pp. 6169–76. 
#Web of Science Nextgen, https://doi.org/10.1021/ac200372d.


library(readxl)
library(xlsx)
path <- "D:\\Document\\新建文件夹"#the path of workspace
setwd(path)
STDlist <- list()#the list of every concentration STD
ALLSTD <- data.frame() #the chart of all STD
jzsf <-
  read_xlsx("jzsf.xlsx") #the original file where there are only needed data exists #
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
getResult <-
  function(jzsf, c = -10, isSTD = 1)
    #save the data as list for convenience and append to the xlsx file
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
STDlist <- getResult(jzsf)
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


Stat_All <-mean(ALLSTD$RRF) #this order is for calculation the Stat_ALL
close <-list()
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
  tmp <- rbind(close1, close2)
  return (mean(tmp[["RRF"]]))
}

Stat_Closest <- function(close)
  #to avoid error about "close", I encapsulate it with a function
{
  return (mean(close$RRF))
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
Dyn_closest <- function(close)
  #to avoid error, I encapsulate it with a function
{
  lm(close$RRF ~ close$TIME)
}

#upon this is DynRRF, if you want to do the regression in the excel
#you can change the return order to return Y and X, which is easy to send into excel
#the Cal ways can also do that.

Stat_Cal <- function(STDlist, method)
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

Dyn_CalPart1 <- function(time, method)
  #this function is one part of Dyn_cal, to calculate the result
{
  time<-as.numeric(time)
  c <- c()
  tmp <- list()
  tmp2 <- c()
  for (i in STDlist)
  {
    c <- append(c, i$concentration[1]) #get concentration
    tmp <- append(tmp, lm(i$RRF ~ i$TIME)$coefficients) 
    # do regression about time on each concentration,save the coefficients into tmp
  }
  for (i in seq(1,14,by=2))
    tmp2 <- append(tmp2, tmp[[i + 1]] * time + tmp[[i]])
  #calculate on each time, each concentration's RRF
  #then do regression between RRF(in theory) with concentration
  Y <- tmp2#Y=RRF
  X <- c#concentration
  Y<-Y*X
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
