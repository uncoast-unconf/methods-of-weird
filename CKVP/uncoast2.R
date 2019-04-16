library(tidyverse)

cbsa <- read.csv("CBSA Demo File for Practice.csv")

names(cbsa) <- tolower(names(cbsa))

#use dplyr code, and select only records with 'tt' prefix
testdat.subset <- cbsa %>% select(starts_with("tt"))

#keep only complete cases...not needed given the princomp specification, but may need it clean later.
testdat.subset[complete.cases(testdat.subset), ]


#run PCA, omit NA's, normalize and rotate factors
PC <- princomp(na.omit(testdat.subset))

#plot eigenvalues
plot(PC,type="lines")

#we still have complex factors, so we need to consider how to pare down items.
fit <- factanal(na.omit(testdat.subset), 5, scores = c("regression"), rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

#remove those with low factor loadings and those with non-simple structure
trimmed.fields <- subset(testdat.subset, select = -c(tt.eng.direct.mail,tt.eng.broadcast.cable.tv,tt.dms.recreational.shoppers,tt.conv.etail.only,tt.adch.online.video,tt.dms.novelty.seekers,tt.adch.online.streaming.tv,tt.dms.in.the.moment.shoppers,tt.eng.mobile.sms.mms,tt.eng.radio,tt.eng.traditional.newspaper,tt.conv.discount.supercenters,tt.conv.online.deal.voucher,tt.adch.broadcast.cable.tv,tt.adch.internet.radio,tt.adch.satellite.radio,tt.conv.ebid.sites,tt.conv.etailer,tt.conv.online.bid.marketplace,tt.conv.online.discount,tt.adch.mobile.video,tt.tp.it.s.all.in.the.name,tt.tp.never.show.up.empty.handed,tt.adch.online.display,tt.conv.brick.mortar) )


#run CFA on reduced item set
fit <- factanal(na.omit(trimmed.fields), 4, scores = c("regression"), rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)


#define measurment model...kind of direct effects. We are not creating regression models (we don't know the directionality between factors)
#nor correlated error terms...esp given we have so many fields.
model <- '
# measurement model
factor1=~tt.tp.buy.american+tt.dms.brand.loyalists+tt.adch.direct.mail+tt.dms.trendsetters+tt.tp.look.at.me.now+tt.tp.on.the.road.again+
tt.eng.digital.newspaper+tt.dms.mainstream.adopters+tt.dms.savvy.researchers+tt.adch.mobile.display+tt.eng.streaming.tv+
tt.eng.digital.video+tt.eng.digital.display

factor2=~tt.dms.deal.seekers+tt.dms.mainstream.adopters+tt.conv.online.mid.high+tt.adch.email.engagement+tt.conv.specialty.dept.store+
tt.conv.wholesale+tt.eng.digital.newspaper+tt.conv.mid.high.end.store+tt.conv.specialty.or.boutique

factor3=~tt.conv.specialty.dept.store+tt.conv.wholesale+tt.conv.online.mid.high+tt.tp.stop.and.smell.the.roses+tt.dms.deal.seekers+
tt.dms.mainstream.adopters+tt.tp.go.with.the.flow+tt.tp.show.me.the.money+tt.tp.no.time.like.the.present+
tt.tp.penny.saved.a.penny.earned+tt.tp.work.hard..play.hard

factor4=~tt.conv.specialty.dept.store+tt.eng.digital.newspaper+tt.conv.wholesale+tt.dms.quality.matters+tt.dms.trendsetters+
tt.dms.organic.and.natural

factor1~~factor1
factor2~~factor2
factor3~~factor3
factor4~~factor4
'

library(lavaan)
#comment out bc can;t run on full dataset
fits <- lavaan(model, na.omit(trimmed.fields))
summary(fits, standardized=TRUE)
_

