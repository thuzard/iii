install.packages('FinTS')
install.packages("tseries")
install.packages('fGarch')
install.packages('quantmod')
install.packages('rugarch') #GARCH模型有很多套件，我採用的是這種，可以做的調整很豐富
library(FinTS)
library(tseries)
library(fGarch)
library(quantmod)
library(rugarch)

#當原始資料不為stationary時，就有可能會產生variance隨時間改變的現象，因此需套用ARCH或GARCH系列的模型

#若不確定的華可以測試是否有出現需要用ARCH的狀況(可用ARCH的情況下大多可以用GARCH，而且GARCH更好)
#ArchTest()

#須考慮是否要對其對數建模
logts = log(tshindex)

#做出一個GARCH模型，使用ugarchspec來設定模型規格，此為設定為TGARCH的模型，藉由調整model選項可以改變所使用的GARCH家族，須注意submodel只有fGARCH才能使用
tshindexSpec = ugarchspec(variance.model = list(model = "fGARCH", 
                                                submodel = "TGARCH",
                                                garchOrder = c(2, 2)),
                                                mean.model = list(armaOrder = c(1, 1)),
                                                distribution.model = "std")
#另一個範例，為sGARCH
tshindexSpec = ugarchspec(variance.model = list(model = "sGARCH", 
                                                garchOrder = c(2, 2)),
                                                mean.model = list(armaOrder = c(1, 1)),
                                                distribution.model = "std")
#詳情請?ugarchspec

#接下來再使用建好的GARCH設定來做模型
tshindexGarch = ugarchfit(spec = tshindexSpec, data = tshindex)

#殘差圖
plot(tshindexGarch@fit$residuals, type = "l")
plot(tshindexGarch, which = 10)

#要找出最適當的GARCH模型需要一個個不斷的嘗試，一般來說GARCH(1,1)就足夠了，須注意的是平均數模型的設定，然後再比較各別的AIC跟BIC，最小的即為適合的模型

#以做好的GARCH模型來預測並畫出，此方法會透過使用bootstrap來預測，可以在method做bootstrap的調整(partial或full)，也可進行期數的調整，須注意full會很久......
tshindexPred = ugarchboot(tshindexGarch, n.ahead = 10,
                          method = "Partial")
plot(tshindexPred, which = 2)