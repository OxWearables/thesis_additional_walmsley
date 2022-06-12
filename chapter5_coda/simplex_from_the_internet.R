# https://stackoverflow.com/questions/35867135/3d-plot-of-2d-simplex-in-r
x <- seq(0,1,0.01) 
y <- seq(0,1,0.01)
f <- function(x,y){ z <- -x - y + 1 }
z <- outer(x,y,f)
z <- ifelse(z<0,NA,z)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
plot_ly(x=x,y=y,z=z,type="surface") %>% layout(xaxis=list(range=c(0,1)), yaxis=list(range=c(0,1)), zaxis=list(range=c(0,1)))


# Answer

pp <- persp(0:1, 0:1, 
            matrix(c(1,0,0,NA), nrow=2), 
            col="green", theta=60, 
            xlab= "",
            ylab ="",
            zlab="",
            ticktype="detailed",
            nticks=1)

text(trans3d(0.5,-0.1,-0.1,pp),labels=expression(theta[1]))
