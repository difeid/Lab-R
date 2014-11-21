cancer = read.table('C:/R-Portable/cancer.csv', header=FALSE, sep = ",", dec = ".")
summary(cancer)
hist(cancer$V1, breaks=15)
hist(cancer$V2, breaks=15)
hist(cancer$V3, breaks=15)

cancer1 = cancer[,1:3]
for (i in 1:306) {
	if (cancer[i,4] == 1){
	cancer1[i,4] = 0
	cancer1[i,5] = 1
	}
	if (cancer[i,4] == 2){
	cancer1[i,4] = 1
	cancer1[i,5] = 0
	}
}

voz = (cancer1$V1-mean(cancer1$V1)) / sd(cancer1$V1)
god = (cancer1$V2-mean(cancer1$V2)) / sd(cancer1$V2)
kol = (cancer1$V3-mean(cancer1$V3)) / sd(cancer1$V3)
zd = data.frame(x1 = voz, x2 = god, x3 = kol, y1 = cancer1$V4, y2 = cancer1$V5)

test.ind = sample(1:306, size = 50)
test = zd[test.ind,]
learn = zd[-test.ind,]
val.ind = sample(1:256, size = 50)
val = learn[val.ind,]
tr = learn[-val.ind,]

library('AMORE')
net1 = newff(n.neurons=c(3,5,2), learning.rate.global=0.1,
momentum.global=0.01, error.criterium="LMS",
hidden.layer="sigmoid", output.layer="sigmoid",
method="ADAPTgdwm")

net2 = newff(n.neurons=c(3,10,2), learning.rate.global=0.1,
momentum.global=0.01, error.criterium="LMS",
hidden.layer="sigmoid", output.layer="sigmoid",
method="ADAPTgdwm")

net3 = newff(n.neurons=c(3,3,2,2), learning.rate.global=0.1,
momentum.global=0.01, error.criterium="LMS",
hidden.layer="sigmoid", output.layer="sigmoid",
method="ADAPTgdwm")

result1 = train(net=net1, P=tr[,1:3],
T=tr[,4:5], Pval=val[,1:3],
Tval=val[,4:5], error.criterium="LMS",
report=TRUE, show.step=100, n.shows=30)

result2 = train(net=net2, P=tr[,1:3],
T=tr[,4:5], Pval=val[,1:3],
Tval=val[,4:5], error.criterium="LMS",
report=TRUE, show.step=100, n.shows=30)

result3 = train(net=net3, P=tr[,1:3],
T=tr[,4:5], Pval=val[,1:3],
Tval=val[,4:5], error.criterium="LMS",
report=TRUE, show.step=100, n.shows=30)

test[1:5,4:5]

out1 = sim(result1$net, test[,1:3])
out1[1:5,1:2]

out2 = sim(result2$net, test[,1:3])
out2[1:5,1:2]

out3 = sim(result2$net, test[,1:3])
out3[1:5,1:2]
