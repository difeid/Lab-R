fr1 = data.frame (x1, x2, y)
fr1 [,1] <- runif (10, 0, 1)
fr1 [,2] <- runif (10, 0, 1)

f1 = function (x1, x2) {
	2 * sin(x1 + x2)
}

fr1 [,3] <- f1 (fr1[,1], fr1[,2])
fr1

f2 = function (x1, x2) {
	cos(x1) / (4 + x2)
}
fr2 = data.frame (x1 = runif (10, -pi, pi),
			x2 = runif (10, -pi, pi),
			y = f2 (x1, x2))
fr2

f3 = function (x1, x2) {
	(0.5 * (x2^2) + 4 * x1 + 5) * ((x1^3) - 1)
}
fr3 = data.frame (x1 = runif (10, 1, 2),
			x2 = runif (10, 0, 2),
			y = f3 (x1, x2))
fr3

fr = data.frame ( a = fr1 , b = fr2, c = fr3)
fr

write.table(fr, file = "lab1.txt",
		sep = "|",
		row.names = FALSE)

file = read.table("lab1.txt",
		sep = "|",
		header = TRUE)
file