library(mgcv)
library(unmarked)

Logit<-function(p){log(p/(1-p));}


###simulate data

library(unmarked)

umf <- unmarked::unmarkedFrameOccu(y = detection_matrice, 
                                   siteCovs = site_info)
fm1 <- occu(~Dnst_Cultures + Dist_Littoral ~1, umf)


## A function produce the Dunn-Smyth residuals.

## Arguments:
## object, the occMod object which must contain the following:
## $data$psi$est, fitted probabilities of occupancy.
## $data$p$est, fitted probabilities of detection.
## $data$det.data, binary matrix of detections, sites in rows and visits in columns. NA for missing values.

## Value: outputs a list with two components:
## det (Dunn-Smyth residuals for detection, based on cdf of #detections).
## occ (Dunn-Smyth residuals for occupancy, based on cdf of binary detected/not).

## To do:
## Generalise to mixture models given: number of components (for a mixture model); mixture probabilities.
## This will be straightforward, almost a one-liner.

residuals.occModum<-function(object,is.detect.constant=FALSE)
{
  det.data<-object@data@y;     # Binary matrix of detections, sites in rows, visits in columns.
  nSites<-dim(det.data)[1];           # Number of sites.
  psi<-predict(object, 'state')[, 1];           # Occupancy probabilities, assuming a vector.
  
  ## Get number of visits (ni) and probability of non-detection (prob0).
  
  ni<-apply(is.na(det.data)==FALSE,1,sum);    # Number of site visits, a vector across sites.
  xi<-apply(det.data,1,sum,na.rm=TRUE);       # Number of detections.
  xiOcc<-pmin(xi,1);                          # Make it binary to study occupancy "only".
  
  pi<-matrix(predict(object, 'det')[, 1],nrow=nSites);  # Detection probabilities.
  
  ##### need to include some code to compute is.detect.constant from pi #####
  
  ## Below assumes equal probabilities of detection for each sampling time!!!
  
  if(is.detect.constant==TRUE)
  {
    pi<-predict(object, 'det')[1:nsites, 1];   # Detection probabilities, assuming equal across sites.
    prob0<-pbinom(0,ni,pi);            # Probability of no detections when present, a site vector.
    
    ## Get cdf's for detection residuals - as a function of sum of detection events
    
    xi[xi==0]<-NA;                               # Ignore sites with no detections here.
    pdet<-(pbinom(xi,ni,pi)-prob0)/(1-prob0);   # CDF for number of detections xi, positive binomial.
    pdetMinus<-(pbinom(xi-1,ni,pi)-prob0)/(1-prob0); # Previous value of the cdf of xi.
  }
  
  if(is.detect.constant==FALSE)
  {
    prob0<-apply(1-pi,1,prod);    # Probability of no detections when present, a site vector.
    
    ## Define a function to get the pdf under unequal detections.
    
    hetpdf<-function(xiSite,niSite,piSite)
    {
      ind<-combn(niSite,xiSite);
      piMat<-matrix(piSite[ind],nrow=xiSite);
      
      return(sum(apply(piMat/(1-piMat),2,prod))*prod(1-piSite));
    }
    
    hetcdf<-function(xiSite,niSite,piSite)
    {
      if(xiSite==0){cdf<-0;}
      else
      {
        detiSite<-rep(NA,xiSite);
        
        for(iX in 1:xiSite)
        {
          detiSite[iX]<-hetpdf(iX,niSite,piSite);
        }
        
        cdf<-sum(detiSite);       
      }
      
      return(cdf);
    }
    
    ## Get cdf's for detection residuals - as a function of sum of detection events.
    
    isDetected<-xi>0;
    xi[isDetected==FALSE]<-NA;  # Ignores sites with no detections.
    
    pdet=pdetMinus=rep(NA,nSites);
    
    for(iSite in which(isDetected))
    {
      xiSite<-xi[iSite];
      niSite<-ni[iSite];
      piSite<-pi[iSite,];
      pdetMinus[iSite]<-hetcdf(xiSite-1,niSite,piSite);
      pdet[iSite]<-pdetMinus[iSite]+hetpdf(xiSite,niSite,piSite);
    }
    
    pdet<-pdet/(1-prob0);   # 'CDF' for number of detections xi in heterogeneous case.
    pdetMinus<-pdetMinus/(1-prob0); # Previous value of the cdf of xi.
  }
  
  ## Get cdf's for occupancy residuals - as a function of binary detected/not.
  
  probOcc<-psi*(1-prob0);                     # Probability of occupancy.
  pOcc<-1-probOcc+xiOcc*probOcc;              # CDF for occupancy, Bernoulli variable with param probOcc.
  pOccMinus<-xiOcc*(1-probOcc);               # Previous value of the cdf of occupancy.
  
  ## Jitter and get occupancy residuals.
  
  uOcc<-runif(nSites);                             # Standard uniform value to "jitter" the cdf.
  residOcc<-qnorm(pOcc*uOcc+pOccMinus*(1-uOcc));   # Dunn-Smyth residual, standard normal if cdf correct.
  
  ## Jitter and get detection residuals.
  
  u<-runif(nSites);                             # Standard uniform value to "jitter" the cdf.
  residDet<-qnorm(pdet*u+pdetMinus*(1-u));      # Dunn-Smyth residual, standard normal if cdf correct.
  
  residuals<-list(occ=residOcc,det=residDet);  
  
  return(residuals) # Return output (i.e., a list with occupancy residuals (occ) and detection residuals (det)).
}

## A function that plots DS residuals against either fitted occupancy/detection probs.



res1<-residuals.occModum(fm1, FALSE)

m1_res_occ<-res1$occ # Stores residuals for occupancy.
m1_res_det<-res1$det[-which(is.na(res1$det))] # Stores residuals for detection (and remove NA
#values).
## Then, use residuals in diagnostic plots, e.g. obtain a normal quantile plot to check distr
#ibutional assumptions:
qqnorm(m1_res_occ,main="",col="blue",pch=16,cex.axis=1.4,ylab="",xlab="") # A QQ???plot.
abline(0,1,col="black")
mtext(text="sample quantiles",side=2,line=2.2,cex=1.2)




DS.resid.plot<-function(x,y,ylim=c(-1,1)*max(abs(y)),alpha=0.05,k=5)
{
  plot(x,y,pch=16,cex=1.2,col="blue",cex.axis=1.4,ylim=ylim,cex.main=0.9,ylab="",xlab="");
  
  lsmod<-gam(y~s(x,k=k));
  lsmod.p<-predict(lsmod,se.fit=TRUE);
  z.crit<-qnorm(1-alpha/2)
  upr<-lsmod.p$fit+(z.crit*lsmod.p$se.fit);
  lwr<-lsmod.p$fit-(z.crit*lsmod.p$se.fit);
  polygon(c(rev(sort(x)),sort(x)),c(rev(upr[order(x)]),(lwr[order(x)])),col='grey80',border=NA);
  points(x,y,pch=16,cex=1.2,col="blue");
  lines(sort(x),fitted(lsmod)[order(x)],lwd=2);
  abline(h=0,col="black",lwd=1);
}


m1_x_occ<-predict(fm1, 'state')[,1] # Stores the fitted values for occupancy probabilities.
DS.resid.plot(m1_x_occ,m1_res_occ)
title(main="Occupancy Dunn???Smyth residuals",cex.main=2)
mtext(text="fitted occupancy values",side=1,las=1,line=2.6,cex=1.2)

###have to play around with this a bit if p is constant
m1_x_det<-apply(matrix(predict(fm1, 'det')[,1],ncol=nt),1,sum,na.rm=T)[is.na(res1$det)==FALSE] # Stores
#the sum of fitted values for detection probabilities.
DS.resid.plot(m1_x_det,m1_res_det)
title(main="Detection Dunn???Smyth residuals",cex.main=2)
mtext(text=Sigma[j]~"fitted detection values",side=1,las=1,line=2.7,cex=1.2)

