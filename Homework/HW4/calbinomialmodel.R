cpparity<-function(c,p,s,r,t,x){
  arb.chance = !((s+p)==c+x/((1+r)^t))
  if(arb.chance==TRUE){
    return(c-s+x/((1+r)^t))
  }
  else{
    return(FALSE)
  }
}

s=50
c=2
x=66
p=12
r=0.1
t=1
cpparity(c,p,s,r,t,x)
60/1.1

calc<-function(ch,cl,sh,sl,r,s){
  delta<-(ch-cl)/(sh-sl)
  B<-(sh*cl-sl*ch)/((1+r)*(sh-sl))
  return(c=delta*s+B)
}

ch<-0
cl<-57.47
sh<-478.4
sl<-20.9
r<-0.276 
s<-100
calc(ch,cl,sh,sl,r,s)
