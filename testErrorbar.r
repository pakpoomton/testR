x = 1:10
y = 1:10
epsilon = 0.2
sdxy = runif(10)

plot (x, y, ylim=c(0, 6))
epsilon = 0.1
for(i in 1:10) {
    up = y[i] + sdxy[i]
    low = y[i] - sdxy[i]
    segments(x[i],low , x[i], up)
    segments(x[i]-epsilon, up , x[i]+epsilon, up)
    segments(x[i]-epsilon, low , x[i]+epsilon, low)
}