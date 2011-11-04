w=function(a,b) {


dt_a=a[,1]-b[,1]
dk_a=a[,2]-b[,2]
db_a=q(a)[,1]-q(b)[,1]
dd_a=q(a)[,2]-q(b)[,2]
dtr_a=q(a)[,3]-q(b)[,3]
dp_a=a[,9]-b[,9]
dr_a=a[,10]-b[,10]

dtaskai = cbind(dt_a)
dklaidos = cbind(dk_a)
dbaudos = cbind(db_a)
ddvitaskiai = cbind(dd_a)
dtritaskiai = cbind(dtr_a)
dprazangos = cbind(dp_a)
datkovoti = cbind(dr_a)

k <- data.frame(dtaskai,dklaidos,dbaudos,ddvitaskiai,dtritaskiai,dprazangos,datkovoti)

}