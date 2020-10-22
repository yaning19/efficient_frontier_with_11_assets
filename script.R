##LOAD THE DATA
DJI<-read.csv(file = "^DJI.csv")
GSPC<-read.csv(file = "^GSPC.csv")
GSPTSE<-read.csv(file = "^GSPTSE.csv")
HUI<-read.csv(file = "^HUI.csv")
IXIC<-read.csv(file = "^IXIC.csv")
N225<-read.csv(file = "^N225.csv")
TYX<-read.csv(file = "^TYX.csv")
XOI<-read.csv(file = "^XOI.csv")
VBMFX<-read.csv(file = "VBMFX.csv")
VEIEX<-read.csv(file = "VEIEX.csv")
VEURX<-read.csv(file = "VEURX.csv")
library(ggplot2)
library(tidyverse)
library(knitr)
##EXTRACT ALL Adj.close PRICE in ONE MATRIX

AdjClose<-cbind(DJI[,6],GSPC[,6],GSPTSE[,6],HUI[,6],HUI[,6],
                N225[,6],TYX[,6],XOI[,6],VBMFX[,6],VEIEX[,6],VEURX[,6])
colnames(AdjClose)<-c("DJI","GSPC","GSPTSE","HUI","IXIC",
                      "N225","TYX","XOI","VBMFX","VEIEX",
                      "VEURX")

##GET ALL DAILY RETURNS IN 1 MATRIX
Return<-matrix(NA,nrow=284,ncol=11)
colnames(Return)<-c("DJIretuen","GSPCreturn","GSPTSEreturn","HUIreturn","IXICreturn",
                    "N225return","TYXreturn","XOIreturn","VBMFXreturn","VEIEXreturn",
                    "VEURXreturn")

for (i in 1:11){
  for (j in 1:284){
    Return[j,i]=(AdjClose[j+1,i]-AdjClose[j,i])/AdjClose[j,i]
  }
}

##GET EXPECTED RETURN AND VOLATILITY FOR EACH STOCK
Data<-matrix(NA,nrow=11,ncol=2)
rownames(Data)=colnames(AdjClose)
colnames(Data)=c("Expected Return","Volatility")
for (i in 1:11){
  Data[i,1]=mean(Return[,i],na.rm = T)
  Data[i,2]=sd(Return[,i],na.rm = T)
}
cov_mat<-cov(Return,use = "complete.obs")

##will run 5000 possible portfolios with different weights
num_port <- 5000

# Creating empty matrix and vectors to store the weights, return and volatility

all_wts <- matrix(nrow = num_port,
                  ncol = 11)
port_returns <- vector('numeric', length = 5000)
port_risk <- vector('numeric', length = 5000)

for (i in 1:5000) {
##randomly generate 5000 weights that sums to 1
##by allowing weight to be negative I allow shortselling 
  wts<-runif(11,-1,1)
  wts <- wts/sum(wts)
  dim(wts)<-c(1,11)

# Storing weight in the matrix
  
  all_wts[i,] <- wts
  
#Creating and storing portfolio returns
  
  port_ret <- sum(wts * Data[,1])
  port_returns[i] <- port_ret
  
  
#Creating and storing portfolio risk
  
  port_sd <- sqrt(wts %*% cov_mat %*% t(wts))
  port_risk[i] <- port_sd
  
}  
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk)
  
ggplot(portfolio_values,aes(x = Risk, y = Return)) +
  geom_point(size=0.01) +
  theme_classic()+
  scale_y_continuous(labels = scales::percent,limits = c(-0.015,0.02)) +
  scale_x_continuous(labels = scales::percent,limits = c(0,0.2)) +
  labs(title = "Efficient Frontier") 
  
  
