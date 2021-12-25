#Determine number of lines using max
max = 3

#vertical axes
plot(rep(0,100), seq(-max - 0.3,max + 0.3,length = 100), type = "l", lwd = 2, frame.plot = FALSE,axes=FALSE, xlab = "", ylab = "", xlim = c(-max,max), ylim = c(-max,max))
#horizontal axes
lines(seq(-max-0.3,max+0.3,length = 100), rep(0,100), type = "l", lwd = 2)


#draw gridlines
for (k in -max:max) {
  print(k)
  lines(seq(-max,max,length = 100), rep(k,100), type = "l")
  lines(rep(k,100), seq(-max,max,length = 100), type = "l")
}

#draw unit square
lines(rep(0,100), seq(0,1,length = 100), type = "l", col = "red", lwd=3)
lines(rep(1,100), seq(0,1,length = 100), type = "l", col = "red", lwd=3)
lines(seq(0,1,length = 100),rep(0,100), type = "l", col = "red", lwd=3)
lines(seq(0,1,length = 100), rep(1,100),  type = "l", col = "red", lwd=3)
  
#specify transformation
f_1 <- function(u,v){
  u^2 - v^2
}
f_2 <- function(u,v){
  2*u*v
}

#determine new axes lengths
new_max_horz <- 0
new_max_vert <- 0
for (k in (-max:max)) {
  candidate_max <- max(f_1(seq(-max,max,length = 100), rep(k,100)),
  f_1(rep(k,100), seq(-max,max,length = 100)))
  new_max_horz <- ifelse(candidate_max > new_max_horz, candidate_max, new_max_horz)
  
  candidate_max <- max(f_2(seq(-max,max,length = 100), rep(k,100)),
                       f_2(rep(k,100), seq(-max,max,length = 100)))
  new_max_vert <- ifelse(candidate_max > new_max_vert, candidate_max, new_max_vert)
}

plot(rep(0,100), seq(-new_max_vert - 0.3,new_max_vert + 0.3,length = 100), type = "l", lwd = 2, frame.plot = FALSE,axes=FALSE, xlab = "", ylab = "", xlim = c(-new_max_horz,new_max_horz), ylim = c(-new_max_vert,new_max_vert))
#horizontal axes
lines(seq(-new_max_horz-0.3,new_max_horz+0.3,length = 100), rep(0,100), type = "l", lwd = 2)

#draw transformed gridlines
for (k in (-max:max)) {
  print(k)
  lines(f_1(seq(-max,max,length = 100), rep(k,100)), f_2(seq(-max,max,length = 100), rep(k,100)), type = "l")
  lines(f_1(rep(k,100), seq(-max,max,length = 100)), f_2(rep(k,100), seq(-max,max,length = 100)), type = "l")
}

#cbind(f_1(rep(2,100), seq(-max,max,length = 100)), f_2(rep(2,100), seq(-max,max,length = 100)), f_1(seq(-max,max,length = 100), rep(1,100)), f_2(seq(-max,max,length = 100), rep(1,100)))

#draw transformed unit square
lines(f_1(rep(0,100), seq(0,1,length = 100)), f_2(rep(0,100), seq(0,1,length = 100)), type = "l", col = "red", lwd=3)
lines(f_1(rep(1,100), seq(0,1,length = 100)), f_2(rep(1,100), seq(0,1,length = 100)), type = "l", col = "red", lwd=3)
lines(f_1(seq(0,1,length = 100),rep(0,100)), f_2(seq(0,1,length = 100),rep(0,100)), type = "l", col = "red", lwd=3)
lines(f_1(seq(0,1,length = 100), rep(1,100)),  f_2(seq(0,1,length = 100), rep(1,100)), type = "l", col = "red", lwd=3)

## APROXIMATE WITH JACOBIAN 
plot(rep(0,100), seq(-new_max_vert - 0.3,new_max_vert + 0.3,length = 100), type = "l", lwd = 2, frame.plot = FALSE,axes=FALSE, xlab = "", ylab = "", xlim = c(-new_max_horz,new_max_horz), ylim = c(-new_max_vert,new_max_vert))
#horizontal axes
lines(seq(-new_max_horz-0.3,new_max_horz+0.3,length = 100), rep(0,100), type = "l", lwd = 2)

#draw transformed gridlines
for (k in (-max:max)) {
  print(k)
  lines(f_1(seq(-max,max,length = 100), rep(k,100)), f_2(seq(-max,max,length = 100), rep(k,100)), type = "l")
  lines(f_1(rep(k,100), seq(-max,max,length = 100)), f_2(rep(k,100), seq(-max,max,length = 100)), type = "l")
}



#cbind(f_1(rep(2,100), seq(-max,max,length = 100)), f_2(rep(2,100), seq(-max,max,length = 100)), f_1(seq(-max,max,length = 100), rep(1,100)), f_2(seq(-max,max,length = 100), rep(1,100)))

#draw transformed unit square
f_1 <- function(u,v){
  4*u-2*v
}
f_2 <- function(u,v){
  2*u+4*v
}

for (k in seq(from= -max, to = max, length = 13)) {
  print(k)
  lines(f_1(seq(-max,max,length = 100), rep(k,100)), f_2(seq(-max,max,length = 100), rep(k,100)), type = "l", col = "red", lwd = 0.8)
  lines(f_1(rep(k,100), seq(-max,max,length = 100)), f_2(rep(k,100), seq(-max,max,length = 100)), type = "l", col = "red", lwd = 0.8)
}

#Zoom in 

#specify transformation
f_1 <- function(u,v){
  u^2 - v^2
}
f_2 <- function(u,v){
  2*u*v
}



plot(f_1(seq(1.5,2.5,length = 100), rep(1,100)), f_2(seq(1.5,2.5,length = 100), rep(1,100)), type = "l",frame.plot = FALSE,axes=FALSE, xlab = "", ylab = "", xlim = c(2,4), ylim = c(3,5))
for(k in seq(from = .7, to = 1.3, length = 13)){
  lines(f_1(seq(1.5,2.5,length = 100), rep(k,100)), f_2(seq(1.5,2.5,length = 100), rep(k,100)), type = "l")
}
for(k in seq(from = 1.7, to = 2.3, length = 13)){
  lines(f_1(rep(k,100), seq(.5,1.5,length = 100)), f_2(rep(k,100), seq(.5,1.5,length = 100)), type = "l")
}

f_1 <- function(u,v){
  4*u-2*v
}
f_2 <- function(u,v){
  2*u+4*v
}



for(k in seq(from = 0, to = 5, length = (5)/.05 + 1)){
  lines(f_1(seq(-100,100,length = 100), rep(k,100)), f_2(seq(-100,100,length = 100), rep(k,100)), type = "l", col = "red")
}
for(k in seq(from = -100, to = 100, length = (200)/.05 + 1)){
  lines(f_1(rep(k,100), seq(0,2,length = 100)), f_2(rep(k,100), seq(0,2,length = 100)), type = "l", col = "red")
}
points(3,4
       )



