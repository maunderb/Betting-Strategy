# Download your predictive data from www.football-data.co.uk and save as
# "Data.csv" (a comma seperated value file). For example you can download
# the data used in our workings at http://www.football-data.co.uk/englandm.php 
# Setting up the data:

DATA=read.csv("Data.csv",header=T) # This reads the data into R

HD=DATA[with(DATA,order(DATA[,3],DATA[,4])),] # Ordering the data by Home Team

AD=DATA[with(DATA,order(DATA[,4],DATA[,3])),] # Ordering the data by Away Team
# Naming some of the columns:
HOME1=HD[,3]

AWAY1=HD[,4]

HOME2=AD[,3]

AWAY2=AD[,4]

HOME.SCORE1=HD[,5]

AWAY.SCORE1=HD[,6]

HOME.SCORE2=AD[,5]

AWAY.SCORE2=AD[,6]

SH=sum(HD[,'FTHG']) # sums the total home scores

SA=sum(HD[,'FTAG']) # sums the total away scores

facH1=factor(HOME1) # This is useful when indexing by teams

facA1=factor(AWAY1)

facH2=factor(HOME2)

facA2=factor(AWAY2)

f1=function(x)return(sum(x[['FTHG']]))

f2=function(x)return(sum(x[['FTAG']]))

bHH1=by(HD,facH1,f1)

bHH2=by(HD,facH1,f2)

bHA1=by(HD,facA1,f1)

bAH2=by(AD,facH2,f2)

bAA2=by(AD,facA2,f2)

bAA1=by(AD,facA2,f1)

# We now estimate the parameters for Model 1:

# Initial estimates can be calculated as:

A=c(bHH1)/sqrt(SH)

B=c(by(HD,facA1,f1))/sqrt(SH)

# We then iteratively calculate the parameters for all the teams:

B1=c(bHA1)/(sum(A)-A)

A1=c(bHH1)/(sum(B1)-B1)

B2=c(bHA1)/(sum(A1)-A1)

A2=c(bHH1)/(sum(B2)-B2)

B3=c(bHA1)/(sum(A2)-A2)

A3=c(bHH1)/(sum(B3)-B3)

B4=c(bHA1)/(sum(A3)-A3)

A4=c(bHH1)/(sum(B4)-B4)

B5=c(bHA1)/(sum(A4)-A4)

A5=c(bHH1)/(sum(B5)-B5)

# Also, the same again for the GAMMA and DELTA parameters

G=c(bAH2)/sqrt(SA)

D=c(bAA2)/sqrt(SA)

D1=c(bAA2)/(sum(G)-G)

G1=c(bAH2)/(sum(D1)-D1)

D2=c(bAA2)/(sum(G1)-G1)

G2=c(bAH2)/(sum(D2)-D2)

D3=c(bAA2)/(sum(G2)-G2)

G3=c(bAH2)/(sum(D3)-D3)

D4=c(bAA2)/(sum(G3)-G3)

G4=c(bAH2)/(sum(D4)-D4)

D5=c(bAA2)/(sum(G4)-G4)

G5=c(bAH2)/(sum(D5)-D5)

R1=cbind(A5,B5,G5,D5) # This is a table of parameter estimates for Model 1


# Model 2

k2=SA/SH # This is the estimate for k2.

# As in model 1, we make initial estimates:

NAL=c(bHH1+bAA2)/((1+k2)*sqrt(SH))

NBE=c(bAA1+bHH2)/((1+k2)*sqrt(SH))

# and now we iteratively estimate the parameters for Model 2

NBE1=c(bAA1+bHH2)/((1+k2)*(sum(NAL)-NAL))

NAL1=c(bHH1+bAA2)/((1+k2)*(sum(NBE)-NBE))

NBE2=c(bAA1+bHH2)/((1+k2)*(sum(NAL1)-NAL1))

NAL2=c(bHH1+bAA2)/((1+k2)*(sum(NBE1)-NBE1))

NBE3=c(bAA1+bHH2)/((1+k2)*(sum(NAL2)-NAL2))

NAL3=c(bHH1+bAA2)/((1+k2)*(sum(NBE2)-NBE2))

NBE4=c(bAA1+bHH2)/((1+k2)*(sum(NAL3)-NAL3))

NAL4=c(bHH1+bAA2)/((1+k2)*(sum(NBE3)-NBE3))

NBE5=c(bAA1+bHH2)/((1+k2)*(sum(NAL4)-NAL4))

NAL5=c(bHH1+bAA2)/((1+k2)*(sum(NBE4)-NBE4))

R2=cbind(NAL5,NBE5) # A table of the estimated parameters for Model 2



# The following functions can be used to estimate probabilities given home and away score means, that is, for model 2, for a match between team i and team j, p = 
#^i^j and q = k^2
#^j ^ i:

# Probability of results given home and away score means

prob=function(p,q){

hs1=dpois(c(0:1000),p)

as1=dpois(c(0:1000),q)

sc=outer(hs1,as1,"*")

HOME=sum(sc[lower.tri(sc)])

DRAW=sum(diag(sc))

AWAY=sum(sc[upper.tri(sc)])

return(cbind(HOME,DRAW,AWAY))}

# Probability of scores given home and away score means

probsc=function(p,q){

hs1=dpois(c(0:10),p)

as1=dpois(c(0:10),q)

sc=outer(hs1,as1,"*")

return(sc)}

# Prob over/under 2.5 goals given home and away score means

prob2.5=function(p,q){

hs1=dpois(c(0:1000),p)

as1=dpois(c(0:1000),q)

sc=outer(hs1,as1,"*")

over=sum(sc)-(sc[1,1]+sc[1,2]+sc[2,1]

+sc[3,1]+sc[1,3]+sc[2,2])

under=1-over

return(cbind(over,under))}


# The following functions can be used to decide whether to bet on particular outcomes on a single football match or not. The variables p and q are the same as in Appendix C, h, d, a need to be EU odds for home win, draw or away win respectively and r is the discrepency level, we reccomend setting r = 1:1. Where the R output shows a 0 no bet should be placed, where the output displays a 1 a bet should be placed on this outcome.

STRATEGY1=function(p,q,h,d,a,r){

values=(prob(p,q))/c(1/h,1/d,1/a)

BET=ifelse(values > r,1,0)

return (BET)}

STRATEGY2.5=function(p,q,o,u,r){

values=(prob2.5(p,q))/c(1/o,1/u)

ODDS=cbind(o,u)

BET=ifelse(values>r,1,0)

return (BET) }

