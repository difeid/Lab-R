hard = read.table('C:/R-Portable/machine2.txt',header = FALSE, sep =",", dec = ".")
summary(hard)
hist(hard$V6, breaks=15)
hist(hard$V7, breaks=15)
hist(hard$V8, breaks=15)
hist(hard$V9, breaks=15)
hist(hard$V10, breaks=15)
hist(hard$V11, breaks=15)
hist(hard$V12, breaks=15)
hist(hard$V13, breaks=15)

v6= +1/(1+exp((hard$V6-mean(hard$V6))/sd(hard$V6)))
v6= +(hard$V6-min(hard$V6))/(max(hard$V6)-min(hard$V6))
v6
hist(v6, breaks=15)

v7= +1/(1+exp((hard$V7-mean(hard$V7))/sd(hard$V7)))
v7= +(hard$V7-min(hard$V7))/(max(hard$V7)-min(hard$V7))
v7
hist(v7, breaks=15)

v8= +1/(1+exp((hard$V8-mean(hard$V8))/sd(hard$V8)))
v8= +(hard$V8-min(hard$V8))/(max(hard$V8)-min(hard$V8))
v8
hist(v8, breaks=15)

v9= +1/(1+exp((hard$V9-mean(hard$V9))/sd(hard$V9)))
v9= +(hard$V9-min(hard$V9))/(max(hard$V9)-min(hard$V9))
v9
hist(v6, breaks=15)

v10= +1/(1+exp((hard$V10-mean(hard$V10))/sd(hard$V10)))
v10= +(hard$V10-min(hard$V10))/(max(hard$V10)-min(hard$V10))
v10
hist(v10, breaks=15)

v11= +1/(1+exp((hard$V11-mean(hard$V11))/sd(hard$V11)))
v11= +(hard$V11-min(hard$V11))/(max(hard$V11)-min(hard$V11))
v11
hist(v11, breaks=15)

v12= +1/(1+exp((hard$V12-mean(hard$V12))/sd(hard$V12)))
v12= +(hard$V12-min(hard$V12))/(max(hard$V12)-min(hard$V12))
v12
hist(v12, breaks=15)

v13= +1/(1+exp((hard$V13-mean(hard$V13))/sd(hard$V13)))
v13= +(hard$V13-min(hard$V13))/(max(hard$V13)-min(hard$V13))
v13
hist(v13, breaks=15)

zd = data.frame(
y1=hard$V1,
y2=hard$V2,
y3=hard$V3,
y4=hard$V4,
y5=hard$V5,
x1=v6,
x2=v7,
x3=v8,
x4=v9,
x5=v10,
x6=v11,
x7=v12,
x8=v13)

test.lal=sample(1:209, size=50)
test.lal
test=zd[test.lal,]
test

learn=zd[-test.lal,]
learn

val.lal=sample(1:159, size=25)
val.lal
val=learn[val.lal,]
val

tr=learn[-val.lal,]
tr

library('AMORE')
net1=newff(n.neurons=c(12,15,1), learning.rate.global=0.1,
momentum.global=0.01, error.criterium="LMS",
hidden.layer="sigmoid", output.layer="sigmoid",
method="ADAPTgdwm")

is.numeric(val[,6:7])
result1=train(net=net1, P=tr[,1:12],
T=tr[,13], Pval=val[,1:12],
Tval=val[,13], error.criterium="LMS",
report=TRUE, show.step=200, n.shows=10)

test[1:15,13]

out1 = sim(result1$net, test[,1:12])
out1[1:15,]
