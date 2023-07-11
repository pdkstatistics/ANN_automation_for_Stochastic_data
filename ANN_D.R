## PACKAGES --------------------------------------------------------------------

#general
easypackages::packages("tibble","psych","dplyr",
                       "magrittr", "data.table",
                       "plyr","tidyr","ggplot2",
                       "cowplot","rje")

#ANN
easypackages::packages("forecast","lmtest",
                       "MASS","neuralnet","DescTools")
#options(max.print=999999)

## EXPORT RESULT ---------------------------------------------------------------
#sink("ANN_Results.txt")  # START
#sink()  # END

## DATA IMPORT -----------------------------------------------------------------

DT=xlsx::read.xlsx(file.choose(),sheetIndex = 1)
#E:\STATISTICS\PUBLICATIONS\PP ANN Sugarcane TN +2\Analysis
head(DT)

 

## DESCRIPTIVE------------------------------------------------------------------
psych::describe(DT[,-1])



## LAG SELECTION ---------------------------------------------------------------

TS=ts(DT[,2],start=1961)#
TS

#1. PACF
A=ggPacf(TS, lag.max =50)+
  ggtitle("PACF of  Original Series")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

L=3



## SCALING AND SPLITTING -------------------------------------------------------

# 1. LAG DATA
LAG=as.vector(TS)
for (i in 1:L) {
  LAG=cbind(LAG,quantmod::Lag(as.vector(TS),k=i))
  if(i==L){
    LAG=LAG[-(1:L),] 
  }
}
LAG


# 2. SCALING
S.LAG=apply(LAG,2,mmx_scale)
summary(S.LAG)  #scaled


# 3. SPLITTING
T=1:round(0.90*nrow(LAG)) #
Tr=S.LAG[T,]  ;dim(Tr)
Tt=S.LAG[-T,] ;dim(Tt)


## NEURON SELECTION ------------------------------------------------------------

MxN=8         # MxN: maximum neurons
HL =3         # HL: no of hidden layers
Neuron=data.frame(Var1=1:MxN)   # Total Neuron inputs

for (i in 2:HL) {
  Neuron=rbind.fill(Neuron,expand.grid(rep(list(1:(MxN-i)),i)))
}
Neuron


## MODELLING -------------------------------------------------------------------

## INPUTS ##

#lg1[,1]  # DEPENDENT
#tr;tt
nrow(Neuron)   #Neuron
D.Tr=c()
D.Tt=c()
FUN=c("logistic","tanh")



{set.seed(123)
  
  for (i1 in 1:nrow(Neuron)) {
    for (i2 in 1:2) {
      
      # Neuron Input
      N=as.numeric(Neuron[i1,])[!is.na(as.numeric(Neuron[i1,]))]
      
      # Fitting the model
      NN=neuralnet::neuralnet(LAG~.,
                              data=Tr,
                              hidden=N,
                              act.fct =FUN[i2],
                              algorithm = 'rprop+',
                              linear.output=FALSE,
                              threshold=0.01,
                              learningrate=0.07,
                              likelihood = TRUE)
      
      ## Training data ###########################
      
      # tabling the estimates
      X.Tr=tibble(S.Y =as.vector(Tr[,1]),
                  S.Ey=as.vector(neuralnet::compute(NN,Tr[,-1])$net.result),
                  Y   =mmx_b.scale(LAG[,1],S.Y),
                  Ey  =mmx_b.scale(LAG[,1],S.Ey),
                  Residuals=Y-Ey)
      
      #  Diagnostic statistics 
      D.Tr=rbind(D.Tr,as.data.frame(with(X.Tr,
                                         list(L1=paste(N,collapse=","),
                                              FUNC=i2,
                                              MAE=DescTools::MAE(Ey,Y),
                                              MAPE=DescTools::MAPE(Ey,Y),
                                              SMAPE=DescTools::SMAPE(Ey,Y),
                                              MSE=DescTools::MSE(Ey,Y),
                                              RMSE=DescTools::RMSE(Ey,Y),
                                              TheilU=DescTools::TheilU(Y,Ey),
                                              AIC=NN$result.matrix[4,1],
                                              BIC=NN$result.matrix[5,1],
                                              ACCURACY=acc(Y,Ey))))) 
      
      
      ## Testing data ###########################
      
      # tabling the estimates
      X.Tt=tibble(S.Y =as.vector(Tt[,1]),
                  S.Ey=as.vector(neuralnet::compute(NN,Tt[,-1])$net.result),
                  Y   =mmx_b.scale(LAG[,1],S.Y),
                  Ey  =mmx_b.scale(LAG[,1],S.Ey),
                  Residuals=Y-Ey)
      
      #  Diagnostic statistics
      D.Tt=rbind(D.Tt,as.data.frame(with(X.Tt,
                                         list(L1=paste(N,collapse=","),
                                              FUNC=i2,
                                              MAE=DescTools::MAE(Ey,Y),
                                              MAPE=DescTools::MAPE(Ey,Y),
                                              SMAPE=DescTools::SMAPE(Ey,Y),
                                              MSE=DescTools::MSE(Ey,Y),
                                              RMSE=DescTools::RMSE(Ey,Y),
                                              TheilU=DescTools::TheilU(Y,Ey),
                                              AIC=NN$result.matrix[4,1],
                                              BIC=NN$result.matrix[5,1],
                                              ACCURACY=acc(Y,Ey)))))  
    }
  }
  rownames(D.Tr)=c()
  rownames(D.Tt)=c()
}


D.Tr
D.Tt

## MODEL SELECTION--------------------------------------------------------------

# 1. TRAIN SET
{R.D.Tr=c()
for (i in 3:dim(D.Tr)[2]) {
  
  if(colnames(D.Tr)[i]!="ACCURACY"){
    R.D.Tr=cbind(R.D.Tr,rank(D.Tr[[i]]))
    colnames(R.D.Tr)[i-2]=colnames(D.Tr)[i]
  }else{
    R.D.Tr=cbind(R.D.Tr,rank(-D.Tr[[i]]))
    colnames(R.D.Tr)[i-2]=colnames(D.Tr)[i]
  }
}
R.D.Tr =as.data.frame(R.D.Tr)
}
R.D.Tr=cbind(D.Tr[,1:2],R.D.Tr,Avg=rowMeans(R.D.Tr[,c(1:6,9)]))
R.D.Tr[order(R.D.Tr[,"Avg"]),][1:15,]


# 2. TEST SET
{R.D.Tt=c()
  for (i in 3:dim(D.Tt)[2]) {
    
    if(colnames(D.Tt)[i]!="ACCURACY"){
      R.D.Tt=cbind(R.D.Tt,rank(D.Tt[[i]]))
      colnames(R.D.Tt)[i-2]=colnames(D.Tt)[i]
    }else{
      R.D.Tt=cbind(R.D.Tt,rank(-D.Tt[[i]]))
      colnames(R.D.Tt)[i-2]=colnames(D.Tt)[i]
    }
  }
  R.D.Tt =as.data.frame(R.D.Tt)
}
R.D.Tt=cbind(D.Tt[,1:2],R.D.Tt,Avg=rowMeans(R.D.Tt[,c(1:6,9)]))
R.D.Tt[order(R.D.Tt[,"Avg"]),][1:15,]


# 3. SELECTION
RANK=data.frame(L1=D.Tt$L1,FUNC=D.Tt$FUNC)
RANK$AVG=(R.D.Tr[,"Avg"]+R.D.Tt[,"Avg"])/2
RANK[order(RANK[,"AVG"]),][1:15,]
Select=row.names(RANK[order(RANK[,"AVG"]),][1:15,])
D.Tr[Select,]
D.Tt[Select,]

# BEST FIT ---------------------------------------------------------------------

# checks    #***#
# 1. the data set
TS

# 2. Selected Neuron [ZZ]
ZZ=as.numeric(c(2,5,3))

# 2. Selected Function 
FUN

{set.seed(123)
  for (i1 in 2) {

        # Fitting the model
        BF.NN=neuralnet::neuralnet(LAG~.,
                                data=S.LAG,
                                hidden=ZZ,
                                act.fct =FUN[i1],
                                algorithm = 'rprop+',
                                linear.output=FALSE,
                                threshold=0.01,
                                learningrate=0.07,
                                likelihood = TRUE)
      
      # Training data
      # tabling the estimates
      X.BF.NN=tibble(S.Y =as.vector(S.LAG[,1]),
                S.Ey=as.vector(neuralnet::compute(BF.NN,S.LAG[,-1])$net.result),
                  Y   =mmx_b.scale(LAG[,1],S.Y),
                  Ey  =mmx_b.scale(LAG[,1],S.Ey),
                  Residuals=Y-Ey)
                      
      #  Diagnostic statistics 
      D.BF.NN=as.data.frame(with(X.BF.NN,
                           list(L1=paste(ZZ,collapse=","),
                                FUNC=i1,
                                MAE=DescTools::MAE(Ey,Y),
                                MAPE=DescTools::MAPE(Ey,Y),
                                SMAPE=DescTools::SMAPE(Ey,Y),
                                MSE=DescTools::MSE(Ey,Y),
                                RMSE=DescTools::RMSE(Ey,Y),
                                TheilU=DescTools::TheilU(Y,Ey),
                                AIC=NN$result.matrix[4,1],
                                BIC=NN$result.matrix[5,1],
                                ACCURACY=acc(Y,Ey))))
  }
}

X.BF.NN %>% print(n=Inf)
D.BF.NN
plot(BF.NN)




# RESIDUAL DIAGNOSTICS ---------------------------------------------------------
RESID.NN=X.BF.NN$Residuals

# 1. Normality tests
# 1.1 Box-Pierce test
Box.test(RESID.NN, lag=10, fitdf=0)             ####
# 1.2 Box-Ljung test
Box.test(RESID.NN,lag=10, fitdf=0, type="Lj")   ####
# 1.3 Shapiro Wilks test
shapiro.test(RESID.NN)                          ####


# 2. Residual plots
# 2.1 Final table
X.BF.NN$Date=time(TS)[-(1:L)]
X.BF.NN

# 2.2 Original Series
B=ggplot(X.BF.NN,aes(Date,Y))+
  #scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  geom_line(colour="Blue")+
  labs(title = "Original Series",
       x = "Time",
       y = "Production(MT)") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


# 2.3 plot residuals
C=ggplot(X.BF.NN,aes(Date,Residuals))+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  geom_line(colour="Blue")+
  geom_hline(yintercept=0, color = "red")+
  labs(title = "Residual Plot",
       x = "Time",
       y = "Residuals") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


# 2.4 Histogram of Residuals
D=ggplot(X.BF.NN,aes(Residuals))+
  scale_x_continuous(expand = c(0,0)) +
  geom_histogram(fill="#66FF99",bins=8)+
  geom_vline(xintercept=0, color = "red")+
  labs(title = "Histogram of Residuals",
       x = "Time",
       y = "Residuals") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


# 2.5 ACF of residuals
E=ggAcf(X.BF.NN$Residuals, lag.max =50)+
  ggtitle("ACF of residuals")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



# FORECASTING-------------------------------------------------------------------
BF.NN
F=7 # f1 : forecast length
L    # l  : Lag variables 
S=S.LAG[,1]# s1 : scaled observed y 


{FOR.X=matrix(NA,1,L)
  for(i in 1:F){
    for (j in 1:L){
      if (i==1){
        FOR.X[i,j]=S[length(S)+1-j]
      } 
      else{
        if(j==1){
          FOR.X=rbind(FOR.X,matrix(NA,1,L))
          m=t(as.matrix(FOR.X[i-1,]))
          FOR.X[i,j]=neuralnet::compute(BF.NN,m)$net.result[1,] 
        }
        else{
          FOR.X[i,j]=FOR.X[i-1,j-1]
        }
      }
    }
  }
  print(FOR.X)  # Scaled input for forecast
  neuralnet::compute(BF.NN,FOR.X)$net.result # Scaled forecast
  mmx_b.scale(LAG[,1],neuralnet::compute(BF.NN,FOR.X)$net.result)
}


## FINALIMAGES------------------------------------------------------------------

ggdraw() +
  draw_plot(B, x = 0, y = .5, width = 1, height = .50) +
  draw_plot(A, x = 0, y = 0, width = 1, height = .50) +
  draw_plot_label(label = c("A","B"), size = 10,
                  x = c(0, 0), y = c(1, 0.5))


ggdraw() +
  draw_plot(C, x = 0, y = .5, width = .6, height = .5) +
  draw_plot(E, x = 0, y = 0, width = .6, height = .5) +
  draw_plot(D, x =0.6, y = 0.1, width = 0.4, height = 0.8) +
  draw_plot_label(label = c("A", "C", "B"), size = 10,
                  x = c(0, 0,0.6), y = c(1, 0.5, 0.9))


################################################################################

