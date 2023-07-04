# Count-Min Sketch dimensioning for Metro network telemetry. Examples.
# Jose Alberto Hernandez
# January 2023






library(sads)


nflows = 7000
npackets = round(nflows*78.5)

ss = 1.1
PDF <- dzipf(x=c(1:nflows), N=nflows, s=ss)
CDF <- pzipf(q=c(1:nflows), N=nflows, s=ss)




flowpacket = ceiling(PDF*npackets)




# CMS sketch


nhash = 11; ncols = 64

sshash = c()
for(hh in c(1:nhash)) {
  sshash = rbind(sshash,sample(x=c(1:ncols), size=nflows, replace = TRUE, prob = NULL))
}

cms = 0*matrix(0,nrow=nhash,ncol=ncols)
for (ii in c(1:nflows)) {
  for (nn in c(1:nhash)) {
    cms[nn,sshash[nn,ii]] = cms[nn,sshash[nn,ii]] + flowpacket[ii]
  }
}


# query de uno de ellos

# error in the top-20 flows

error = 0*c(0:nflows)
cms_query_3 = 0*c(0:nflows); cms_query_5 = 0*c(0:nflows); cms_query_7 = 0*c(0:nflows); cms_query_9 = 0*c(0:nflows);
for (ii in c(1:nflows)) {
  flowid = ii #sample(x=c(1:nflows), size=1)
  
  cms_aux = 0*c(1:nhash)
  for (nn in c(1:nhash)) {
    cms_aux[nn] = cms[nn,sshash[nn,flowid]]
  }
  cms_query_3[ii] = min(cms_aux[1:3])
  cms_query_5[ii] = min(cms_aux[1:5])
  cms_query_7[ii] = min(cms_aux[1:7])
  cms_query_9[ii] = min(cms_aux[1:9])
  
#  cms_query[ii] = min(c(cms[1,ss1[flowid]], cms[2,ss2[flowid]],
#                        cms[3,ss3[flowid]], cms[4,ss4[flowid]],
#                        cms[5,ss5[flowid]]))
  
  error[ii] = (flowpacket[flowid] - cms_query[ii])/flowpacket[flowid]
}
#print(summary(error))


Ntest = 2e1 # top-20 could be 50% of total traffic
print(flowpacket[1:Ntest])
print(cms_query[1:Ntest])
print(summary(error[1:Ntest]))


plot(flowpacket[1:Ntest],pch=0,col="black",xlim=c(1,20), ylim=c(0,1.2*flowpacket[1]),
     xlab="Flow-ID", ylab="Number of packets")
title("Top-20 flows (CMS estimate) ")
points(cms_query_3[1:Ntest],pch=1,col="red")
points(cms_query_5[1:Ntest],pch=2,col="green")
points(cms_query_7[1:Ntest],pch=3,col="blue")
legend(10, flowpacket[1], legend=c("Real Flow size", "Est. CMS (d=3,w=64)", "Est. CMS (d=5,w=64)", "Est. CMS (d=7,w=64)"),
       col=c("black","red", "green","blue"), pch=0:3, lty=1:2, cex=0.8)


# Experiment no. 2


nhash = 11; ncols = 256

sshash = c()
for(hh in c(1:nhash)) {
  sshash = rbind(sshash,sample(x=c(1:ncols), size=nflows, replace = TRUE, prob = NULL))
}

cms = 0*matrix(0,nrow=nhash,ncol=ncols)
for (ii in c(1:nflows)) {
  for (nn in c(1:nhash)) {
    cms[nn,sshash[nn,ii]] = cms[nn,sshash[nn,ii]] + flowpacket[ii]
  }
}


# query de uno de ellos

# error in the top-20 flows

error = 0*c(0:nflows)
cms_query_3 = 0*c(0:nflows); cms_query_5 = 0*c(0:nflows); cms_query_7 = 0*c(0:nflows); cms_query_9 = 0*c(0:nflows);
for (ii in c(1:nflows)) {
  flowid = ii #sample(x=c(1:nflows), size=1)
  
  cms_aux = 0*c(1:nhash)
  for (nn in c(1:nhash)) {
    cms_aux[nn] = cms[nn,sshash[nn,flowid]]
  }
  cms_query_3[ii] = min(cms_aux[1:3])
  cms_query_5[ii] = min(cms_aux[1:5])
  cms_query_7[ii] = min(cms_aux[1:7])
  cms_query_9[ii] = min(cms_aux[1:9])
  
  #  cms_query[ii] = min(c(cms[1,ss1[flowid]], cms[2,ss2[flowid]],
  #                        cms[3,ss3[flowid]], cms[4,ss4[flowid]],
  #                        cms[5,ss5[flowid]]))
  
  error[ii] = (flowpacket[flowid] - cms_query[ii])/flowpacket[flowid]
}
#print(summary(error))


Ntest = 2e1 # top-20 could be 50% of total traffic
print(flowpacket[1:Ntest])
print(cms_query[1:Ntest])
print(summary(error[1:Ntest]))


plot(flowpacket[1:Ntest],pch=0,col="black",xlim=c(1,20), ylim=c(0,1.2*flowpacket[1]),
     xlab="Flow-ID", ylab="Number of packets")
title("Top-20 flows (CMS estimate) ")
points(cms_query_3[1:Ntest],pch=1,col="red")
points(cms_query_5[1:Ntest],pch=2,col="green")
points(cms_query_7[1:Ntest],pch=3,col="blue")
legend(10, flowpacket[1], legend=c("Real Flow size", "Est. CMS (d=3,w=256)", "Est. CMS (d=5,w=256)", "Est. CMS (d=7,w=256)"),
       col=c("black","red", "green","blue"), pch=0:3, lty=1:2, cex=0.8)


plot(CDF, type = "b", pch = 19, 
     col = "red", xlab="Flow ID",ylab = "Accumulated traffic")
title("Flow-size Zipf distribution")
legend(2000,0.4,"Zipf dist.: alpha=1.1, N=7000")
