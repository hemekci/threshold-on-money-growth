# Load in Data
rm(list=ls())
sink(NULL)

# Set working directory to script location (optional, comment out if running from project root)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data file path (relative to project root)
file_path <- "../data/ArticleData.xlsx"
library(readxl)

data <- read_excel(file_path, sheet="ArticleData", col_names = FALSE)

# Combine the first two rows to create column names
colnames(data) <- paste(data[1, ], data[2, ], sep = "_")

# Remove the first two rows since they have been used as column names
data <- data[-c(1, 2), ]
numeric_cols <- names(data)[grepl("_(M3LogDiff|Deflator|RealGDP|YEAR)$", names(data))]
data[, numeric_cols] <- sapply(data[, numeric_cols], as.numeric)
data_original = data

# Load necessary libraries
library(dplyr)
library(zoo)

deflator_columns <- data %>%
  select(contains("_deflator")) %>%
  names()

# Create lagged columns
data <- data %>%
  mutate(across(all_of(deflator_columns), ~lag(.), .names = "{.col}_1"))

data <- data %>%
  select(1, sort(names(data)[-1]))

#data <- na.omit(data)
#data <- data[-1, ] #drop first
#data <- data[-1, ] #drop second
#data <- data[-1, ] #drop third
#data <- data[-1, ] #drop forth


library(tidyr)
df_long <- data %>% 
  gather(key = "Country_Metric", value = "Value", -DATE_YEAR)

#df_long = na.omit(df_long)



df <- df_long %>%
  separate(Country_Metric, into = c("Country", "Metric"), sep = "_", extra = "merge")
df_wide <- df %>%
  pivot_wider(names_from = Metric, values_from = Value)
colnames(df_wide) <- c("Year", "Country", "Deflator", "Deflator_1", "M3LogDiff", "RealGDP")
print(df_wide)
df_wide = na.omit(df_wide)


deflator <- as.numeric(df_wide$Deflator)
deflator1 <- as.numeric(df_wide$Deflator_1)
m3g <- as.numeric(df_wide$M3LogDiff)

# Define variables
y = deflator 
x = m3g 
n = length(y)
z = cbind(matrix(1,n,1),deflator1)


#####################GROWTH.r######################################

# Controls
gammas = seq(0,40,by=0.1)	# Grid on Threshold parameter for estimation
dx = seq(0,121,by=1)		# Grid on regression function for display
level = 0.90			# For confidence sets
boot = 10000			# Number of bootstrap replications
Ceps = c(0.5,1,2,4)		# For numerical delta method bootstrap

############################################


# Some useful functions
reg <- function(X,y) {
  X <- qr(X)
  as.matrix(qr.coef(X,y))
}

pos.part <- function(x) x*(x>0)
neg.part <- function(x) x*(x<0)

pt1 = proc.time()


# Linear Model
x0 = cbind(m3g,z)
k0 = ncol(x0)
kz = ncol(z)
x00 = solve(crossprod(x0))
bols = x00%*%crossprod(x0,y)
e0 = y - x0%*%bols
sse0 = sum(e0^2)
sigols = sse0/n
v0 = x00%*%crossprod(x0*matrix(e0,n,k0))%*%x00*(n/(n-k0))
seols = as.matrix(sqrt(diag(v0)))


# Threshold Model
grid = length(gammas)
rd = length(dx)
sse = matrix(0,grid,1)
k = kz + 3
for (j in 1:grid) {
  gamj=gammas[j]
  x1 = cbind(neg.part(x-gamj),pos.part(x-gamj),z)
  e1 = y - x1%*%reg(x1,y)
  sse[j] = sum(e1^2)
}
gi = which.min(sse)
gammahat = gammas[gi]
ssemin = sse[gi]
x1 = cbind(neg.part(x-gammahat),pos.part(x-gammahat),z)
bt = reg(x1,y)
et = y - x1%*% bt
hg = - (x<gammahat)*bt[1] - (x>gammahat)*bt[2]
x2 = cbind(x1,hg)
hg2 = crossprod(cbind((x<gammahat),(x>gammahat)),et)
xx2 = matrix(0,k,k)
xx2[1:2,k]=hg2
xx2[k,1:2]=t(hg2)
xxi = solve(crossprod(x2) + xx2)
v = xxi%*%crossprod(x2*matrix(et,n,k))%*%xxi*(n/(n-k))
betahat = rbind(bt,gammahat)
se = as.matrix(sqrt(diag(v)))
sig = sum(et^2)/n
wt = n*(sse0-ssemin)/ssemin
wg = n*(sse-ssemin)/ssemin

# Plot Least Squares Criterion, Figure 3 in paper
windows()
plot(gammas,sse,type="l",ylab="Least Squares Criterion",xlab="Threshold Parameter")
savePlot(file="../results/figures/fig3.eps",type="eps",dev.cur())


# Regression Estimate
G = cbind(neg.part(dx-gammahat),pos.part(dx-gammahat),matrix(1,rd,1)%*%colMeans(z))
yf = G%*%bt

# Bootstrap & Testing
waldb = matrix(0,grid,boot)
sseb  = matrix(0,grid,boot)
betab = array(0,c(grid,k-1,boot))
u = matrix(rnorm(n*boot),n,boot)
eb = matrix(e0,n,boot)*u
yb = matrix(x1%*%bt,n,boot) + matrix(et,n,boot)*u
eb0 = eb - x0%*%reg(x0,eb)
bsse0 = colSums(eb0^2)
for (j in 1:grid) {
  gamj = gammas[j]
  x2 = cbind(neg.part(x-gamj),pos.part(x-gamj),z)
  eb0 = eb - x2%*%reg(x2,eb)
  bsse = colSums(eb0^2)
  waldb[j,] = n*(bsse0-bsse)/bsse
  bb = reg(x2,yb)
  eb1 = yb - x2%*%bb
  sseb[j,] = colSums(eb1^2)
  betab[j,,] = bb
}

# Multiplier Bootstrap test for Threshold
wb = apply(waldb,2,max)
pv = mean(wb > matrix(wt,boot,1))
crit = quantile(wb,probs=level)

# Threshold Regression Estimates
gib = apply(sseb,2,which.min)
gamb = gammas[gib]

# Symmetric Percentile Confidence Interval Construction
betahatb = matrix(0,k-1,boot)
ci = matrix(0,k,2)
for (j in 1:(k-1)){
  bj = diag(betab[gib,j,])-bt[j]
  qj = quantile(abs(bj),probs=level)
  ci[j,1] = bt[j] - qj
  ci[j,2] = bt[j] + qj
  betahatb[j,] = t(bj)
}

# Confidence Interval for Threshold
sseb0 = colSums((yb - x1%*%reg(x1,yb))^2)
sseminb = apply(sseb,2,min)
wgb = n*(sseb0-sseminb)/sseminb
qa = qchisq(level,1)
wia = (wg > qa)
cga = c(gammas[which.min(wia)],gammas[grid+1-which.min(rev(wia))])
qb = quantile(wgb,probs=level)
wib = (wg > qb)
cgb = c(gammas[which.min(wib)],gammas[grid+1-which.min(rev(wib))])
ci[k,] = cgb

# Threshold Regression Confidence Intervals
mdx = t(matrix(dx,rd,boot))-gammahat
thetab = t(G%*%betahatb)
cn = length(Ceps)
yf1 = matrix(0,rd,cn)
yf2 = matrix(0,rd,cn)
for (j in 1:cn) {
  eps = Ceps[j]
  h = matrix(gamb-gammahat,boot,rd)*eps
  thetaq = thetab + ((neg.part(mdx-h)-neg.part(mdx))*bt[1] + (pos.part(mdx-h)-pos.part(mdx))*bt[2])/eps
  qf = apply(abs(thetaq),2,quantile,probs=level)
  yf1[,j] = yf - qf
  yf2[,j] = yf + qf
}

pt2 = proc.time()

sink("../results/growth.out")
cat("Linear Model, coefficients and error variance","\n")
print(cbind(bols,seols),digits=2)
cat("Error variance","\n")
print(sigols,digits=4)


cat("Wald Test for Threshold, p-value, & critical value","\n")
print(c(wt,pv,crit,level),digits=3)
cat("Threshold Model Estimates, s.e.'s, and Bootstrap confidence intervals","\n")
print(cbind(betahat,se,ci),digits=2)
cat("Error variance","\n")
print(sig,digits=4)

cat("Bootstrap Critical value for threshold parameter interval",qb,"\n")
cat("Computation Time: replications and seconds","\n")
print(c(boot,pt2[3]-pt1[3]),digits=3)

sink()


# Plot Confidence Interval Construction for Threshold (Figure 4)
windows()
plot(gammas,wg,type="l",ylab="Threshold F Statistic",xlab="Threshold Parameter")
cr1 = matrix(qa,grid,1)
cr2 = matrix(qb,grid,1)
lines(gammas,cr1,lty="dashed",col="blue")
lines(gammas,cr2,lty="dotted",col="red")
arrows(cga[1],qa,cga[1],0,lty="dashed",col="blue")
arrows(cga[2],qa,cga[2],0,lty="dashed",col="blue")
arrows(cgb[1],qb,cgb[1],0,lty="dotted",col="red")
arrows(cgb[2],qb,cgb[2],0,lty="dotted",col="red")
legend("topright",c("Bootstrap Critical Value","Asymptotic Critical Value"),lty=c("dotted","dashed"),col=c("red","blue"))
savePlot(file="../results/figures/fig4.eps",type="eps",dev.cur())



# Scatter plot, regression line, and confidence intervals (Figure 2)
windows()
plot(m3g,deflator,ylab="Deflator",xlab="M3g")
lines(dx,yf)
lines(dx,yf1[,2],lty="dashed",col="blue")
lines(dx,yf2[,2],lty="dashed",col="blue")
yk = colMeans(z)%*%bt[3:(2+kz)]
points(gammahat,yk,col="red",bg="red",pch=22)


windows()
plot(dx,yf,ylab="deflator",xlab="M3g",type="l",ylim=c(-8,80),yaxt="n")
axis(2,at=c(0,40,0,40,10))
lines(dx,yf1[,1],lty="dashed",col="orange")
lines(dx,yf2[,1],lty="dashed",col="orange")
lines(dx,yf1[,2],lty="dotted",col="red")
lines(dx,yf2[,2],lty="dotted",col="red")
lines(dx,yf1[,3],lty="dashed",col="blue")
lines(dx,yf2[,3],lty="dashed",col="blue")
lines(dx,yf1[,4],lty="dotted",col="black")
lines(dx,yf2[,4],lty="dotted",col="black")
points(gammahat,yk,col="red",bg="red",pch=22)
leg1 = bquote(paste(epsilon[n]==.(Ceps[1]),n^-.5))
leg2 = bquote(paste(epsilon[n]==.(Ceps[2]),n^-.5))
leg3 = bquote(paste(epsilon[n]==.(Ceps[3]),n^-.5))
leg4 = bquote(paste(epsilon[n]==.(Ceps[4]),n^-.5))
leg_text = c(as.expression(leg1),as.expression(leg2),as.expression(leg3),as.expression(leg4))

legend("bottomright",leg_text,lty=c("dashed","dotted","dashed","dotted"),col=c("orange","red","blue","black"))
savePlot(file="../results/figures/fig7.eps",type="eps",dev.cur())

###################################simg.r##############################################################


# simg.r

# This is an R file
# It creates some of the simulation work reported in Bruce Hansen "Regression Kink with an Unknown Threshold"


rep = 20 #changed 20000 to 20
boot = 100 #changed 1000 to 10
level = 0.10
step = 1


reg <- function(X,y) {
  X <- qr(X)
  as.matrix(qr.coef(X,y))
}

pos.part <- function(x) x*(x>0)
neg.part <- function(x) x*(x<0)

rqnorm <- function(x) {
  if (x < 0.01) q=qnorm(.01) else if (x > 0.99) q=qnorm(.99) else q=qnorm(x)
  q
}

Dg1 <- function(x,beta1,beta2,gam,epsilon,h) {
  if (epsilon > 0) {
    gamt = gam+h*epsilon
    xL1 = neg.part(x-gam)
    xU1 = pos.part(x-gam)
    xL2 = neg.part(x-gamt)
    xU2 = pos.part(x-gamt)
    dg = ((xL2-xL1)*beta1 + (xU2-xU1)*beta2)/epsilon
  }
  else     dg = -((x < gam)*beta1 + (x > gam)*beta2)*h
  dg
  
}

gammas = seq(0,40,by=step)
grid = length(gammas)
k = 4
y0 = deflator[1]
x = m3g
a0 = 3
a1 = 0.3
sig = 4
ybar = 4
gam = 40 
betas = seq(-0.04,-.16,-0.04)
dx = seq(0,121,by=1)
rd = length(dx)
numb = length(betas)

c1 = .5
c2 = 1
c3 = 2
c4 = 4
eps1 = c1
eps2 = c2
eps3 = c3
eps4 = c4

gstore = array(0,c(rd,numb,8))
gstore1 = matrix(0,rep,rd)
gstore2 = matrix(0,rep,rd)
gstore3 = matrix(0,rep,rd)
gstore4 = matrix(0,rep,rd)
gstore5 = matrix(0,rep,rd)
gstore6 = matrix(0,rep,rd)
gstore7 = matrix(0,rep,rd)
gstore8 = matrix(0,rep,rd)

for (bi in 1:numb) {
  b = betas[bi]
  xgam = (x-gam)*(x > gam)*b
  g0 = a0+a1*ybar+(dx-gam)*(dx>gam)*b
  for (i in 1:rep) {
    u = xgam + rnorm(n)*sig
    ysim = matrix(y0,n+1,1)
    for (t in 1:n) ysim[t+1] = a0 + a1*ysim[t]+ u[t]
    y = ysim[2:(n+1)]
    z = cbind(matrix(1,n,1),ysim[1:n])
    
    sse=matrix(0,grid,1)
    for (j in 1:grid) {
      gamj=gammas[j]
      xL = neg.part(x-gamj)
      xU = pos.part(x-gamj)
      x1 = cbind(z,xL,xU)
      e1 = y - x1%*%reg(x1,y)
      sse[j] = sum(e1^2)
    }
    gi = which.min(sse)
    gamest = gammas[gi]
    ssemin = sse[gi]
    xL = neg.part(x-gamest)
    xU = pos.part(x-gamest)
    x1 = cbind(z,xL,xU)
    bt = reg(x1,y)
    et = y - x1%*% bt
    hg = - (x<gamest)*bt[k-1] - (x>gamest)*bt[k]
    x2 = cbind(x1,hg)
    hg2 = crossprod(cbind((x<gamest),(x>gamest)),et)
    xx2 = matrix(0,k+1,k+1)
    xx2[3:4,k+1]=hg2
    xx2[k+1,3:4]=t(hg2)
    xxi = solve(crossprod(x2) + xx2) 
    v = xxi%*%crossprod(x2*matrix(et,n,k+1))%*%xxi*(n/(n-k-1))
    
    dx1 = neg.part(dx-gamest)
    dx2 = pos.part(dx-gamest)
    yf = bt[1]+bt[2]*ybar+dx1*bt[3]+dx2*bt[4]
    G1 = cbind(matrix(1,rd,1),matrix(ybar,rd,1),dx1,dx2)
    dx3 = -(dx<gamest)*bt[3]-(dx>gamest)*bt[4]
    G = cbind(G1,dx3)
    vg = sqrt(rowSums(G*(G%*%v)))
    yf1 = yf - vg*qnorm(1-level/2)
    yf2 = yf + vg*qnorm(1-level/2)
    gstore1[i,] = t(1 - (g0 > yf2) - (g0 < yf1))
    
    # Bootstrap
    sseb  = matrix(0,grid,boot)
    betabb = array(0,c(grid,k,boot))
    yb = matrix(x1%*%bt,n,boot) + matrix(et,n,boot)*matrix(rnorm(n*boot),n,boot)
    for (j in 1:grid) {
      gamj = gammas[j]
      xL = neg.part(x-gamj)
      xU = pos.part(x-gamj)
      x2 = cbind(z,xL,xU)
      bb = reg(x2,yb)
      eb1 = yb - x2%*%bb
      sseb[j,] = colSums(eb1^2)
      betabb[j,,] = bb
    }
    gib = apply(sseb,2,which.min)
    gamb = gammas[gib]
    betab1 = diag(betabb[gib,1,])
    betab2 = diag(betabb[gib,2,])
    betab3 = diag(betabb[gib,3,])
    betab4 = diag(betabb[gib,4,])
    b1 = t(matrix(betab1,boot,rd))
    b2 = t(matrix(betab2,boot,rd))
    b3 = t(matrix(betab3,boot,rd))
    b4 = t(matrix(betab4,boot,rd))
    
    mdx = t(matrix(dx,rd,boot))
    mg  = matrix(gamb,boot,rd)
    dm1 = t(neg.part(mdx-mg))
    dm2 = t(pos.part(mdx-mg))
    yfb = b1+b2*ybar+dm1*b3+dm2*b4
    yfb1 = apply(yfb,1,quantile,probs=level/2)
    yfb2 = apply(yfb,1,quantile,probs=1-level/2)
    yfq1 = yf*2 - yfb2
    yfq2 = yf*2 - yfb1
    yfq = apply(abs(yfb-yf),1,quantile,probs=1-level)
    gstore2[i,] = t(1 - (g0 > yfb2) - (g0 < yfb1))
    gstore3[i,] = t(1 - (g0 > yfq2) - (g0 < yfq1))
    gstore4[i,] = t(1 - (g0 > (yf+yfq)) - (g0 < (yf-yfq)))
    
    betahatb = t(cbind(betab1,betab2,betab3,betab4)) - matrix(bt,k,boot)
    thetab = t(G1%*%betahatb)
    h = matrix(gamb-gamest,boot,rd)
    
    thetab1 = abs(thetab + Dg1(mdx,bt[3],bt[4],gamest,eps1,h))
    q1 = apply(thetab1,2,quantile,probs=1-level)
    gstore5[i,] = t(1 - (g0 > (yf+q1)) - (g0 < (yf-q1)))
    
    thetab2 = abs(thetab + Dg1(mdx,bt[3],bt[4],gamest,eps2,h))
    q1 = apply(thetab2,2,quantile,probs=1-level)
    gstore6[i,] = t(1 - (g0 > (yf+q1)) - (g0 < (yf-q1)))
    
    thetab3 = abs(thetab + Dg1(mdx,bt[3],bt[4],gamest,eps3,h))
    q1 = apply(thetab3,2,quantile,probs=1-level)
    gstore7[i,] = t(1 - (g0 > (yf+q1)) - (g0 < (yf-q1)))
    
    thetab4 = abs(thetab + Dg1(mdx,bt[3],bt[4],gamest,eps4,h))
    q1 = apply(thetab4,2,quantile,probs=1-level)
    gstore8[i,] = t(1 - (g0 > (yf+q1)) - (g0 < (yf-q1)))
    
  }
  gc1 = colMeans(gstore1)
  gc2 = colMeans(gstore2)
  gc3 = colMeans(gstore3)
  gc4 = colMeans(gstore4)
  gc5 = colMeans(gstore5)
  gc6 = colMeans(gstore6)
  gc7 = colMeans(gstore7)
  gc8 = colMeans(gstore8)
  
  gstore[,bi,1] = gc1
  gstore[,bi,2] = gc2
  gstore[,bi,3] = gc3
  gstore[,bi,4] = gc4
  gstore[,bi,5] = gc5
  gstore[,bi,6] = gc6
  gstore[,bi,7] = gc7
  gstore[,bi,8] = gc8
  
}

leg1=bquote(beta[2]==.(betas[1]))
leg2=bquote(beta[2]==.(betas[2]))
leg3=bquote(beta[2]==.(betas[3]))
leg4=bquote(beta[2]==.(betas[4]))
leg_text = c(as.expression(leg1),as.expression(leg2),as.expression(leg3),as.expression(leg4))

cc = matrix(1-level,rd,1)

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.70,.99),ylab="Coverage Probability",xlab="x") #.95 -> .99
lines(dx,gstore[,1,1],lty="dashed")
lines(dx,gstore[,2,1],lty="dotted",col="red")
lines(dx,gstore[,3,1],lty="dashed",col="blue")
lines(dx,gstore[,4,1],lty="dashed",col="brown")
legend("topright",legend=leg_text,lty=c("dashed","dotted","dashed","dashed"),col=c("black","red","blue","brown"))
savePlot(file="../results/figures/fig5.eps",type="eps",dev.cur())
title("Naive Asymptotic Intervals")

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.70,.99),ylab="Coverage Probability",xlab="x")
lines(dx,gstore[,1,2],lty="dashed")
lines(dx,gstore[,2,2],lty="dotted",col="red")
lines(dx,gstore[,3,2],lty="dashed",col="blue")
lines(dx,gstore[,4,2],lty="dashed",col="brown")
legend("topright",legend=leg_text,lty=c("dashed","dotted","dashed","dashed"),col=c("black","red","blue","brown"))
title("Percentile Intervals")

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.70,.99),ylab="Coverage Probability",xlab="x")
lines(dx,gstore[,1,3],lty="dashed")
lines(dx,gstore[,2,3],lty="dotted",col="red")
lines(dx,gstore[,3,3],lty="dashed",col="blue")
lines(dx,gstore[,4,3],lty="dashed",col="brown")
legend("topright",legend=leg_text,lty=c("dashed","dotted","dashed","dashed"),col=c("black","red","blue","brown"))
title("Inverse Percentile Intervals")

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.70,.99),ylab="Coverage Probability",xlab="x")
lines(dx,gstore[,1,4],lty="dashed")
lines(dx,gstore[,2,4],lty="dotted",col="red")
lines(dx,gstore[,3,4],lty="dashed",col="blue")
lines(dx,gstore[,4,4],lty="dashed",col="brown")
legend("topright",legend=leg_text,lty=c("dashed","dotted","dashed","dashed"),col=c("black","red","blue","brown"))
title("Symmetric Percentile Intervals")

leg1 = bquote(paste(epsilon[n]==.(c1),n^-.5))
leg2 = bquote(paste(epsilon[n]==.(c2),n^-.5))
leg3 = bquote(paste(epsilon[n]==.(c3),n^-.5))
leg4 = bquote(paste(epsilon[n]==.(c4),n^-.5))
leg_text = c(as.expression(leg1),as.expression(leg2),as.expression(leg3),as.expression(leg4))

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.8,1),ylab="Coverage Probability",xlab="x")
lines(dx,gstore[,1,5],lty="dotted",col="blue")
lines(dx,gstore[,1,6],lty="dashed",col="black")
lines(dx,gstore[,1,7],lty="dotted",col="red")
lines(dx,gstore[,1,8],lty="dashed",col="orange")
legend("topright",legend=leg_text,lty=c("dotted","dashed","dotted","dashed"),col=c("blue","black","red","orange"))
savePlot(file="../results/figures/fig6a.eps",type="eps",dev.cur())
title(bquote(paste("Numerical Delta Method Bootstrap Intervals, ",beta[2]==.(betas[1]))))

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.8,1),ylab="Coverage Probability",xlab="x")
lines(dx,gstore[,2,5],lty="dotted",col="blue")
lines(dx,gstore[,2,6],lty="dashed",col="black")
lines(dx,gstore[,2,7],lty="dotted",col="red")
lines(dx,gstore[,2,8],lty="dashed",col="orange")
legend("topright",legend=leg_text,lty=c("dotted","dashed","dotted","dashed"),col=c("blue","black","red","orange"))
savePlot(file="../results/figures/fig6b.eps",type="eps",dev.cur())
title(bquote(paste("Numerical Delta Method Bootstrap Intervals, ",beta[2]==.(betas[2]))))

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.8,1),ylab="Coverage Probability",xlab="x")
lines(dx,gstore[,3,5],lty="dotted",col="blue")
lines(dx,gstore[,3,6],lty="dashed",col="black")
lines(dx,gstore[,3,7],lty="dotted",col="red")
lines(dx,gstore[,3,8],lty="dashed",col="orange")
legend("topright",legend=leg_text,lty=c("dotted","dashed","dotted","dashed"),col=c("blue","black","red","orange"))
savePlot(file="../results/figures/fig6c.eps",type="eps",dev.cur())
title(bquote(paste("Numerical Delta Method Bootstrap Intervals, ",beta[2]==.(betas[3]))))

windows()
plot(dx,cc,type="l",lty="dotted",ylim=c(.8,1),ylab="Coverage Probability",xlab="x")
lines(dx,gstore[,4,5],lty="dotted",col="blue")
lines(dx,gstore[,4,6],lty="dashed",col="black")
lines(dx,gstore[,4,7],lty="dotted",col="red")
lines(dx,gstore[,4,8],lty="dashed",col="orange")
legend("topright",legend=leg_text,lty=c("dotted","dashed","dotted","dashed"),col=c("blue","black","red","orange"))
savePlot(file="../results/figures/fig6d.eps",type="eps",dev.cur())
title(bquote(paste("Numerical Delta Method Bootstrap Intervals, ",beta[2]==.(betas[4]))))


###################################simci.r##############################################################


reg <- function(X,y) {
  X <- qr(X)
  as.matrix(qr.coef(X,y))
}

rqnorm <- function(x) {
  if (x < 0.01) q=qnorm(.01) else if (x > 0.99) q=qnorm(.99) else q=qnorm(x)
  q
}

pos.part <- function(x) x*(x>0)
neg.part <- function(x) x*(x<0)

gammas = seq(0,40,by=step)
grid = length(gammas)
k = 4
y0 = deflator[1]
x = m3g
a0 = 3
a1 = 0.3
sig = 4
ybar = 4
gam = 40 
betas = seq(-0.02,-.16,-0.02)
numb = length(betas)

tgam = matrix(0,numb,6)
tbeta = matrix(0,numb,4)
sgam = matrix(0,rep,6)
sbeta = matrix(0,rep,4)

for (bi in 1:numb) {
  b = betas[bi]
  xgam = pos.part(x-gam)*b
  
  for (i in 1:rep) {
    
    e = rnorm(n)*sig
    ysim = matrix(y0,n+1,1)
    for (t in 1:n) ysim[t+1] = a0 + a1*ysim[t]+ xgam[t] + e[t]
    y = ysim[2:(n+1)]
    z = cbind(matrix(1,n,1),ysim[1:n])
    
    sse = matrix(0,grid,1)
    for (j in 1:grid) {
      gamj=gammas[j]
      xL = neg.part(x-gamj)
      xU = pos.part(x-gamj)
      x1 = cbind(z,xL,xU)
      e1 = y - x1%*%reg(x1,y)
      sse[j] = sum(e1^2)
    }
    gi = which.min(sse)
    gamest = gammas[gi]
    ssemin = sse[gi]
    xL = neg.part(x-gamest)
    xU = pos.part(x-gamest)
    x1 = cbind(z,xL,xU)
    bt = reg(x1,y)
    et = y - x1%*%bt
    hg = - (x<gamest)*bt[k-1] - (x>gamest)*bt[k]
    x2 = cbind(x1,hg)
    hg2 = crossprod(cbind((x<gamest),(x>gamest)),et)
    xx2 = matrix(0,k+1,k+1)
    xx2[3:4,k+1]=hg2
    xx2[k+1,3:4]=t(hg2)
    xxi = solve(crossprod(x2) + xx2) 
    
    v = xxi%*%crossprod(x2*matrix(et,n,k+1))%*%xxi*(n/(n-k-1))
    betahat = rbind(bt,gamest)
    se = as.matrix(sqrt(diag(v)))
    wg = n*(sse-ssemin)/ssemin
    
    # Bootstrap
    sseb  = matrix(0,grid,boot)
    betab = matrix(0,grid,boot)
    eb0 = matrix(et,n,boot)*matrix(rnorm(n*boot),n,boot)
    yb = matrix(x1%*%bt,n,boot) + eb0
    
    for (j in 1:grid) {
      gamj = gammas[j]
      xL = neg.part(x-gamj)
      xU = pos.part(x-gamj)
      x2 = cbind(z,xL,xU)
      bb = reg(x2,yb)
      eb1 = yb - x2%*%bb
      sseb[j,] = colSums(eb1^2)
      betab[j,] = bb[k,]
    }
    
    tg = (gamest-gam)/se[k+1]
    
    gib = apply(sseb,2,which.min)
    gamb = gammas[gib]
    gamq1 = quantile(gamb,probs=level/2)
    gamq2 = quantile(gamb,probs=1-level/2)
    cg2 = c(gamq1,gamq2)
    cg3 = c(gamest*2-gamq2,gamest*2-gamq1)
    gamq3 = quantile(abs(gamb-gamest),probs=1-level)
    cg4 = c(gamest-gamq3,gamest+gamq3)
    
    wi = (wg > qchisq(1-level,1))
    cg5 = c(gammas[which.min(wi)],gammas[grid+1-which.min(rev(wi))])
    
    sseminb = apply(sseb,2,min)
    wgb = n*(sseb[gi,]-sseminb)/sseminb
    qb = quantile(wgb,probs=1-level)
    wib = (wg > qb)
    cg6 = c(gammas[which.min(wib)],gammas[grid+1-which.min(rev(wib))])
    
    sgam[i,1] = ((tg^2) <= qchisq(1-level,1))
    sgam[i,2] = 1 - (gam > cg2[2]) - (gam < cg2[1])
    sgam[i,3] = 1 - (gam > cg3[2]) - (gam < cg3[1])
    sgam[i,4] = 1 - (gam > cg4[2]) - (gam < cg4[1])
    sgam[i,5] = 1 - (gam > cg5[2]) - (gam < cg5[1])
    sgam[i,6] = 1 - (gam > cg6[2]) - (gam < cg6[1])
    
    tb = (bt[k]-b)/se[k]
    
    bk = diag(betab[gib,])
    bk1 = quantile(bk,probs=level/2)
    bk2 = quantile(bk,probs=1-level/2)
    cb2 = c(bk1,bk2)
    cb3 = c(bt[k]*2-bk2,bt[k]*2-bk1)
    bk3 = quantile(abs(bk-bt[k]),probs=1-level)
    cb4 = c(bt[k]-bk3,bt[k]+bk3)
    
    sbeta[i,1] = ((tb^2) <= qchisq(1-level,1))
    sbeta[i,2] = 1 - (b > cb2[2]) - (b < cb2[1])
    sbeta[i,3] = 1 - (b > cb3[2]) - (b < cb3[1])
    sbeta[i,4] = 1 - (b > cb4[2]) - (b < cb4[1])
    
  }
  tgam[bi,]  = colMeans(sgam)
  tbeta[bi,] = colMeans(sbeta)
}

sink("../results/simci.out")

cat("Simulation Replications =",rep,"\n")
cat("Simulation Bootstrap Replications =",boot,"\n")
cat("Nominal Level =",1-level,"\n")

cat("gamma","\n")
print(t(tgam),digits=2)
cat("beta2","\n")
print(t(tbeta),digits=2)

sink()









