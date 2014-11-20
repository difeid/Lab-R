n = 1000
x1 = runif(n, min=0, max=4)
x2 = runif(n, min=0, max=2)

f1 = function(x1,x2) {
	return(exp(x1) + 1/(x2+2))
}
f2 = function(x1,x2) {
	return(2*x1^2 + x2/(x1+1))
}

y1 = f1(x1,x2)
y2 = f2(x1,x2)

y1 = y1 + 0.05*runif(n,min = min(y1),max = max(y1))
y2 = y2 + 0.05*runif(n,min = min(y2),max = max(y2))

zd = data.frame(x1,x2,y1,y2)

zdn = data.frame(x1=zd$x1, x2 = zd$x2,
y1 = ((zd$y1-min(zd$y1))/(max(zd$y1)-min(zd$y1))),
y2 = ((zd$y2-min(zd$y2))/(max(zd$y2)-min(zd$y2))))
library('AMORE')
net1 = newff(n.neurons=c(2,5,2), learning.rate.global=0.2,
momentum.global=0.01, error.criterium="LMS",
hidden.layer="sigmoid", output.layer="sigmoid",
method="ADAPTgdwm")

tr = zdn[1:500,1:4]
val = zdn[501:750,1:4]
test = zdn[751:1000,1:4]

result1 = train(net=net1, P=tr[,1:2],
T=tr[,3:4], Pval=val[,1:2],
Tval=val[,3:4], error.criterium="LMS",
report=TRUE, show.step=1, n.shows=5)


out1 = sim(result1$net, test[,1:2])

plot(test[,3], out1[,1])
plot(test[,4], out1[,2])

err1 = abs(out1[,1] - test$y1)
summary(err1)

err2 = abs(out1[,2] - test$y2)
summary(err2)

inputs = data.frame(x1=c(3.5, 1.0), x2=c(1.5, 1.3))
out = sim(result1$net, inputs)

m1 = min(zd$y1)
M1 = max(zd$y1)
Y1 = out[,1]*(M1-m1)+m1
Y1
Y1 = f1(inputs$x1,inputs$x2)
Y1

m2 = min(zd$y2)
M2 = max(zd$y2)
Y2 = out[,2]*(M2-m2)+m2
Y2
Y2 = f2(inputs$x1,inputs$x2)
Y2


