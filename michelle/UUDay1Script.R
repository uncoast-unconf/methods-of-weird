library(dplyr)
#library(lavaan)
library(ggplot2)
library(sem)

data <- read.csv("CBSADemoFilePractice.csv", header = TRUE)
names(data) <- tolower(names(data))

selected_cols <- data %>% select(starts_with("tt")) %>% filter(complete.cases(.))

data_pca <- prcomp(selected_cols, center = TRUE, scale = TRUE)

summary(data_pca)

data_pca$rotation

#plot(data_pca, ellipse=TRUE,choices=c(1,2))

plot(data_pca, TYPE = "lines")

fit3 <- factanal(selected_cols, 5, rotation="varimax")
print(fit3, digits=2, cutoff=.3, sort=TRUE)

#remove those with low factor loadings and those with non-simple structure
trimmed.fields <- subset(selected_cols, select = -c(tt.eng.direct.mail,tt.eng.broadcast.cable.tv,tt.dms.recreational.shoppers,tt.conv.etail.only,tt.adch.online.video,tt.dms.novelty.seekers,tt.adch.online.streaming.tv,tt.dms.in.the.moment.shoppers,tt.eng.mobile.sms.mms,tt.eng.radio,tt.eng.traditional.newspaper,tt.conv.discount.supercenters,tt.conv.online.deal.voucher,tt.adch.broadcast.cable.tv,tt.adch.internet.radio,tt.adch.satellite.radio,tt.conv.ebid.sites,tt.conv.etailer,tt.conv.online.bid.marketplace,tt.conv.online.discount,tt.adch.mobile.video,tt.tp.it.s.all.in.the.name,tt.tp.never.show.up.empty.handed,tt.adch.online.display,tt.conv.brick.mortar))

#run CFA on reduced item set
fit <- factanal(na.omit(trimmed.fields), 4, scores = c("regression"), rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)


model <- '
  # measurement model
factor1=~tt.tp.buy.american+tt.dms.brand.loyalists+tt.adch.direct.mail+tt.dms.trendsetters+tt.tp.look.at.me.now+tt.tp.on.the.road.again+
tt.eng.digital.newspaper+tt.dms.mainstream.adopters+tt.dms.savvy.researchers+tt.adch.mobile.display+tt.eng.streaming.tv+
tt.eng.digital.video+tt.eng.digital.display

frctor2=~tt.dms.deal.seekers+tt.dms.mainstream.adopters+tt.conv.online.mid.high+tt.adch.email.engagement+tt.conv.specialty.dept.store+
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

factor2~~factor2
factor2~~factor3
factor2~~factor4

factor3~~factor3
factor3~~factor4

factor4~~factor4
'

#comment out bc can;t run on full dataset
#fitsem <- lavaan(model, na.omit(trimmed.fields))
fitsem <- sem(model, na.omit(trimmed.fields))
summary(fitsem, standardized=TRUE)

