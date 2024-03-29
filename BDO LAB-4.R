#full blind search method
fsearch=function(search, FUN, type="min",...) 
{ 		x=apply(search,1,FUN,...) 
# run FUN over all search rows 	
ib=switch(type, min=which.min(x),max=which.max(x)) 
return(list(index=ib, sol=search[ib,],eval=x[ib])) 
}

#grid search method
gsearch=function(step,lower,upper,FUN,type="min",...) 
{ 		
  D=length(step) 			# dimension 
  domain=vector("list",D)		 # domain values 
  L=vector(length=D)			 # auxiliary vector 	
  for(i in 1:D) 	
  { 	
    domain[[i]]=seq(lower[i],upper[i],by=step[i]) 				 		
    L[i]=length(domain[[i]]) 
  } 
  LS=prod(L) 
  s=matrix(ncol=D,nrow=LS) 	# set the search space 
  for(i in 1:D) 
  { 	
    if(i==1) E=1 
    else E=E*L[i-1] 			 		
    s[,i]=rep(domain[[i]],length.out=LS,each=E) 
  } 
  fsearch(s,FUN,type,...)			 # best solution 
}

#sales function
sales=function(x, A=1000,B=200,C=141, m=seq(2,length.out=length(x),by=-0.25)) 
{
  return(round(m*(A/log(x+B)-C),digits=0))
}

#cost function
cost=function(units,A=100,cpu=35-5*(1:length(units)))
{
  return(A+cpu*units)
}

#profit function
profit=function(x) 		#x-a vector of prices 
{ x=round(x,digits=0) 	# convert x into integer 
s=sales(x) 			# get the expected sales 
c=cost(s) 			# get the expected cost 
profit=sum(s*x-c)		 # compute the profit 
return(profit) 
# local variables x, s, c and profit are lost from here
}

#bag prices problem using grid dearch
PTM=proc.time() 			# start clock 
S1=gsearch(rep(100,5),rep(1,5),rep(1000,5),profit,"max") 
sec=(proc.time()-PTM)[3] 	# get seconds elapsed 
cat("gsearch best s:",S1$sol,"f:",S1$eval,"time:",sec,"s\n")

