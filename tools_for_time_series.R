#可能用到套件
install.packages('dplyr')
install.packages('zoo')
install.packages('ggplot2')
install.packages('xts')
install.packages('forecast')
install.packages("tseries")
install.packages('KernSmooth')
library(ggplot2)
library(zoo)
library(dplyr)
library(xts)
library(forecast)
library(tseries)
library(KernSmooth)
library(RMySQL)

#將資料讀進來後拆開
hindex = read.table("123.txt", sep = "|")
hindex1 = hindex[,1]  #年月
hindex2 = hindex[,2]  #台北房價指數
hindex3 = hindex[,3]  #新北房價指數
hindex4 = hindex[,4]  #大台北房價指數

#組合成時間序列
tshindex = zoo(hindex2)

#一階差分
difftshindex = diff(tshindex)

#2期移動平均
rollmean(tshindex,2)

#檢查是否有自我相關
Box.test(tshindex)

#自我相關函數，決定AR階數
acf(tshindex)

#偏自我相關函數，決定MA階數
pacf(tshindex)

#檢視兩個時間序列的延遲相關(ARDL)
#ccf(時間序列1, 時間序列2)

#移除時間序列趨勢
m = lm(coredata(tshindex)~index(tshindex))  #coredata為指數，index為時間期數
detr = zoo(resid(m), index(tshindex))  #resid為殘差

#建立ARIMA模型
?auto.arima
m = auto.arima(tshindex, max.p = 20, max.q = 20)
#m = auto.arima(tshindex, max.p = 20, max.q = 20, stationary = TRUE)
#此為自動配置適合的階數，分別為AR(p), I(d), MA(q)
#auto.arima配置不一定好，可試試自行檢驗ARIMA階層進行配置
#arima(tshindex, order=c(6,2,3))  手動配置
#目前較合適的模型為arima(tshindex, order = c(2,2,3), fixed = c(NA,NA,0,0,NA))
confint(m)  #此為ARIMA各係數的信賴區間，如果上下介裡包含0的話須考慮移除

#若信賴區間包含0，則可用fixed()將對應ARMA階層移除
ts = arima(tshindex, order = c(6,2,3), fixed = c(NA,0,0,0,0,0,0,0,NA))

#診斷ARIMA模型
tsdiag(m)
#會產生三張圖，理想狀況是SR圖沒有波動性集群，ACF沒有顯著相關，LB的P值都很大(表示殘差沒有規律)

#以ARIMA進行預測
predict(m)
predict(m, n.ahead = 10)  #預測未來十期
forecast(m)  #和上面那句一樣功能，但會加上範圍
plot(forecast(etshindex,h=10))  #將forecast的結果畫出來
#預測準確度
accuracy(forecast(m))  #只能放forcast物件

#Augmented Dickey-Fuller(ADf)檢定(可測試時間序列是否會回到長期平均值，也可檢測時間序列是否為stationary)
adf.test(tshindex)

#如果想將white noise平滑化，請參照R Cookbook P.441，KernSmooth套件
