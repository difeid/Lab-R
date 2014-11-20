za = matrix (0, nrow = 4, ncol = 4)
za [1:4, 1] = 1
za [3:4, 2] = 1
za [2,3] = 1
za [4,3] = 1
za [4,4] = 1
za [1:3,4] = -1

f1 = function (u) {
	if (u >=0) y = 1
	else y = -1
	return (y)
}

w = c(0,0,0)

f2 = function (x1, x2, x3) {
	f1 (x1*w[1] + x2*w[2] + x3*w[3])
}

obuch = function (a) {
	for (j in 1:3)
	{
		w[j] = w[j] + 0.05 * za[a,j] * za[j,4]
	}
	return (w)
}

a=0;
for(i in 1:100 )
{
	a = a + 1;
	v = f2(za[1,a], za[2,a], za[3,a])
	if (v != za[a,4]){
		w = obuch(a)
	}
	if(a==4)
	{
		a=0
	}
}
w