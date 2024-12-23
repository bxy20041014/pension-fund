library(readxl)
stock <- read_excel("股指.xlsx", col_types = c("date", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))
nav <- read_excel("股指.xlsx", sheet = "单位净值", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))
cnav <- read_excel("股指.xlsx", sheet = "累计净值", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))

library(tidyverse)
library(dplyr)
df1=tibble(cnav)
df2=tibble(nav)
df3=tibble(stock)

df=inner_join(df3,df1,by = "日期")
df=inner_join(df,df2,by = "日期")

cnav.data=df[,c(1,8,10,12,14,16,18)]
nav.data=df[,c(1,8,20,22,24,26,28)]
cnav.data=cnav.data[-729,]
nav.data=nav.data[-729,]

name=c("日期","混合基准收益率","建信深证基本面60ETF","工银瑞信深证100ETF","国联安沪深300ETF","景顺长城成长之星股票A","保险主题LOF")
colnames(cnav.data)=name
colnames(nav.data)=name
cnav.name=names(cnav.data[,-1]);cnav.name
nav.name=names(nav.data[,-1]);nav.name

summary.cnav=data.frame(
    '名称'=character(),
    '均值'=numeric(),
    '中位数'=numeric(),
    '最大值'=numeric(),
    '最小值'=numeric(),
    '标准差'=numeric()
)
for (x in cnav.name) {
  summary.cnav <- rbind(summary.cnav,
                        data.frame(
                          名称 = x,
                          均值 = mean(cnav.data[[x]], na.rm = TRUE),
                          中位数 = median(cnav.data[[x]], na.rm = TRUE),
                          最大值 = max(cnav.data[[x]], na.rm = TRUE),
                          最小值 = min(cnav.data[[x]], na.rm = TRUE),
                          标准差 = sqrt(var(cnav.data[[x]], na.rm = TRUE)),
                          stringsAsFactors = FALSE
                        ))
}

summary.nav=data.frame(
  '名称'=character(),
  '均值'=numeric(),
  '中位数'=numeric(),
  '最大值'=numeric(),
  '最小值'=numeric(),
  '标准差'=numeric()
)
for (x in nav.name) {
  summary.nav <- rbind(summary.nav,
                        data.frame(
                          名称 = x,
                          均值 = mean(nav.data[[x]], na.rm = TRUE),
                          中位数 = median(nav.data[[x]], na.rm = TRUE),
                          最大值 = max(nav.data[[x]], na.rm = TRUE),
                          最小值 = min(nav.data[[x]], na.rm = TRUE),
                          标准差 = sqrt(var(nav.data[[x]], na.rm = TRUE)),
                          stringsAsFactors = FALSE
                        ))
}


#cnav单位根检验----
library(tseries)

for (x in colnames(cnav.data[-1])) {
  cat("基金累计净值增长率: ", x, "\n")
  adf_result=adf.test(cnav.data[[x]])
  print(adf_result)
  cat("\n------------------------\n")
}
#所有序列都是平稳的，可以进行格兰杰因果检验

#cnav格兰杰因果检验----

library(lmtest)
library(vars)

# 检查是否有缺失值
cnav.data <- na.omit(cnav.data)
str(cnav.data)

# 使用 VARselect 来确定最优滞后阶数
lag_selection <- VARselect(cnav.data[-1], lag.max = 10, type = "const")
best_lag <- lag_selection$selection["AIC(n)"]  # 选择AIC准则确定的最优滞后阶数
print(best_lag)

# 循环对所有基金收益列进行格兰杰因果检验
for (fund in colnames(cnav.data[,-c(1,2)])) {  
  cat("基金累计净值增长率: ", fund, "\n")
  
  # 检验基准收益是否由基金净值增长格兰杰导致
  print(grangertest(as.formula(paste("混合基准收益率 ~", fund)), order = best_lag, data = cnav.data))
  
  # 检验基金净值增长率是否由基准收益格兰杰导致
  print(grangertest(as.formula(paste(fund, "~ 混合基准收益率")), order = best_lag, data = cnav.data))
  
  cat("\n------------------------\n")
}
#建信深证基本面60ETF，工银瑞信深证100ETF，保险主题LOF 与混合基准收益间不存在格兰杰因果
#国联安沪深300ETF ，景顺长城成长之星股票A 在0.05显著性水平下存在格兰杰因果，且为双向因果

#nav单位根检验----

for (x in colnames(nav.data[,-1])) {
  cat("基金日净值增长率: ", x, "\n")
  adf_result=adf.test(cnav.data[[x]])
  print(adf_result)
  cat("\n------------------------\n")
}
#所有序列都是平稳的，可以进行格兰杰因果检验

#nav格兰杰因果----
# 检查是否有缺失值
nav.data <- na.omit(nav.data)

# 使用 VARselect 来确定最优滞后阶数
lag_selection <- VARselect(nav.data[-1], lag.max = 10, type = "const")
best_lag <- lag_selection$selection["AIC(n)"]  # 选择AIC准则确定的最优滞后阶数
print(best_lag)

# 循环对所有基金收益列进行格兰杰因果检验
for (fund in colnames(nav.data)[-1]) {  
  cat("基金日净值增长率: ", fund, "\n")
  
  # 检验基准收益是否由基金净值增长格兰杰导致
  print(grangertest(as.formula(paste("混合基准收益率 ~", fund)), order = best_lag, data = nav.data))
  
  # 检验基金净值增长率是否由基准收益格兰杰导致
  print(grangertest(as.formula(paste(fund, "~ 混合基准收益率")), order = best_lag, data = nav.data))
  
  cat("\n------------------------\n")
}
#建信深证基本面60ETF，工银瑞信深证100ETF，保险主题LOF 与混合基准收益间不存在格兰杰因果
#国联安沪深300ETF ，景顺长城成长之星股票A 在0.05显著性水平下存在格兰杰因果，且为双向因果

#cnav  Johansen协整检验
#cnav理论上是平稳的不必进行协整检验
A=data.frame(上证A指收益率=stock$上证A指收益率)
A=na.omit(A)
cnav.data=cbind(cnav.data,A)
nav.data=cbind(nav.data,A)
library(urca)

for (i in 2:7) {
  # 线性回归
  reg <- lm(cnav.data[[i]] ~ cnav.data$上证A指收益率)
  residuals <- reg$residuals
  
  # 单位根检验（残差）
  unit_root_test <- ur.df(residuals, type = "drift", selectlags = "AIC")
  #拒绝原假设意味着残差序列无单位根，残差长期平稳，具有长期关系
  cat(paste(colnames(cnav.data[i]), "与上证A指的协整检验结果：\n"))
  print(summary(unit_root_test))
}
#全部拒绝残差非平稳，说明五只基金与上证A指均存在长期关系

for (i in 2:7) {
  # 线性回归
  reg <- lm(nav.data[[i]] ~ nav.data$上证A指收益率)
  residuals <- reg$residuals
  
  # 单位根检验（残差）
  unit_root_test <- ur.df(residuals, type = "drift", selectlags = "AIC")
  #拒绝原假设意味着残差序列无单位根，残差长期平稳，具有长期关系
  cat(paste(colnames(nav.data[i]), "与上证A指的协整检验结果：\n"))
  print(summary(unit_root_test))
}

#风险调整收益----
#定义计算函数----
library(dplyr)

# 计算夏普比率(年化)----
sharpe_ratio <- function(returns, risk_free_rate = 0, scale = 252) {
  excess_returns <- returns - risk_free_rate
  mean_excess_return <- mean(excess_returns, na.rm = TRUE)
  sd_excess_return <- sd(excess_returns, na.rm = TRUE)
  
  sharpe <- (mean_excess_return / sd_excess_return) * sqrt(scale)
  return(sharpe)
}

# 计算特雷诺比率----
treynor_ratio <- function(returns, risk_free_rate = 0, beta) {
  excess_returns <- returns - risk_free_rate
  mean_excess_return <- mean(excess_returns, na.rm = TRUE)
  
  treynor <- mean_excess_return / beta
  return(treynor)
}

# 计算信息比率----
information_ratio <- function(returns, benchmark_returns) {
  excess_returns <- returns - benchmark_returns
  mean_excess_return <- mean(excess_returns, na.rm = TRUE)
  tracking_error <- sd(excess_returns, na.rm = TRUE)
  
  information <- mean_excess_return / tracking_error
  return(information)
}

# 计算M方指标----
m2_ratio <- function(fund_returns, benchmark_returns, risk_free_rate = 0) {
  beta <- cov(fund_returns, benchmark_returns) / var(benchmark_returns)
  m2=(mean(fund_returns, na.rm = TRUE) - risk_free_rate) - beta * (mean(benchmark_returns, na.rm = TRUE) - risk_free_rate)
  return(m2)
}

rf <- read_excel("rf.xlsx", col_types = c("text", 
                                          "skip", "numeric"))
rf$日期=as.Date(rf$日期, format = "%Y-%m-%d")
nav.data=inner_join(nav.data,rf,by = "日期")

results <- data.frame(Fund = character(), 
                      Sharpe = numeric(), 
                      Treynor = numeric(), 
                      Information = numeric(), 
                      M2 = numeric(), 
                      stringsAsFactors = FALSE)

for (i in 2:7) {  # 基金列从2到7
  fund_returns <- nav.data[[i]]
  market_returns <- nav.data$上证A指收益率
  risk_free_rate <- mean(nav.data$`日度化无风险利率(%)`)
  
  # 计算各项指标
  sharpe <- sharpe_ratio(fund_returns, risk_free_rate)
  beta <- cov(fund_returns, market_returns) / var(market_returns)
  treynor <- treynor_ratio(fund_returns, risk_free_rate, beta)
  information <- information_ratio(fund_returns, market_returns)
  m2 <- m2_ratio(fund_returns, market_returns, risk_free_rate)
  
  # 将结果存入数据框
  results <- rbind(results, data.frame(Fund = colnames(nav.data)[i],
                                       Sharpe = sharpe, 
                                       Treynor = treynor, 
                                       Information = information, 
                                       M2 = m2))
}


results <- results |> 
  mutate(
    排名_sharpe = rank(-Sharpe, ties.method = "first"),.after = Sharpe) |> 
  mutate(
    排名_Treynor = rank(-Treynor, ties.method = "first"),.after=Treynor) |> 
  mutate(
    排名_Information = rank(-Information, ties.method = "first"),.after=Information) |> 
  mutate(
    排名_M2 = rank(-M2, ties.method = "first"),.after=M2)

#SM模型----

nav.data=nav.data |> 
  mutate(
    混合基准收益率超额收益 = 混合基准收益率 - `日度化无风险利率(%)`,
    建信深证基本面60ETF超额收益 = 建信深证基本面60ETF - `日度化无风险利率(%)`,
    工银瑞信深证100ETF超额收益 = 工银瑞信深证100ETF - `日度化无风险利率(%)`,
    国联安沪深300ETF超额收益 = 国联安沪深300ETF - `日度化无风险利率(%)`,
    景顺长城成长之星股票A超额收益 = 景顺长城成长之星股票A - `日度化无风险利率(%)`,
    保险主题LOF超额收益 = 保险主题LOF - `日度化无风险利率(%)`,
    市场超额收益 = 上证A指收益率 - `日度化无风险利率(%)`
  )
nav.data.reg=nav.data[,c(-2:-7)]

newname=colnames(nav.data.reg[,c(-1:-3,-10)])
i=1
alpha=numeric(6)
beta=numeric(6)
t_value_alpha=numeric(6)
t_value_beta=numeric(6)
p_value_alpha=numeric(6)
p_value_beta=numeric(6)
R2=numeric(6)
for (x in newname) {
  # 使用lm进行普通回归
  fit <- lm(reformulate("市场超额收益", response = x), data = nav.data.reg)
  
  # 提取回归系数
  alpha[i] <- coef(fit)[1]
  beta[i] <- coef(fit)[2]
  
  # 提取 t 值和 p 值（截距）
  summary_fit <- summary(fit)
  t_value_alpha[i] <- summary_fit$coefficients[1, "t value"]
  p_value_alpha[i] <- round(summary_fit$coefficients[1, "Pr(>|t|)"], 2)
  
  # 计算 beta = 1 的 t 值
  beta_estimate <- coef(fit)[2]
  beta_se <- summary_fit$coefficients[2, "Std. Error"]
  t_value_beta[i] <- (beta_estimate - 1) / beta_se  # 计算 beta = 1 的 t 值
  
  # 计算 p 值
  p_value_beta[i] <- 2 * pt(-abs(t_value_beta[i]), df = summary_fit$df[2])  # 双侧检验的 p 值
  p_value_beta[i]=round(p_value_beta[i],2)
  
  # 提取 R^2
  R2[i] <- summary_fit$r.squared
  
  # 更新索引
  i <- i + 1
}
sm.results <- data.frame(
  Fund = newname,
  Alpha = alpha,
  T_Value_Alpha = t_value_alpha,
  P_Value_Alpha = p_value_alpha,
  Beta = beta,
  t_Value_Beta_1_Test = t_value_beta,
  P_Value_Beta_1_Test = p_value_beta,  # 用于检验 beta = 1 的 p 值
  R_Squared = R2
)


#双因素模型----
B=stock[-729,7]
nav.data.reg=cbind(nav.data.reg,B)
nav.data.reg=nav.data.reg |> 
  mutate(
    国债超额收益 = `中证5-10年国债活跃券收益率` - `日度化无风险利率(%)`
  ) 
i=1
alpha=numeric(6)
beta1=numeric(6)
beta2=numeric(6)
t_value_alpha=numeric(6)
t_value_beta1=numeric(6)
t_value_beta2=numeric(6)
p_value_alpha=numeric(6)
p_value_beta1=numeric(6)
p_value_beta2=numeric(6)
R2=numeric(6)
for (x in newname) {
  # 使用lm进行普通回归
  fit <- lm(reformulate(c("市场超额收益","国债超额收益"), response = x), data = nav.data.reg[,-11])
  
  # 提取回归系数
  alpha[i] <- coef(fit)[1]
  beta1[i] <- coef(fit)[2]
  beta2[i] <- coef(fit)[3]
  
  # 提取 t 值和 p 值（截距）
  summary_fit <- summary(fit)
  t_value_alpha[i] <- summary_fit$coefficients[1, "t value"]
  p_value_alpha[i] <- round(summary_fit$coefficients[1, "Pr(>|t|)"], 2)
  
  # 计算 beta1 = 1 的 t 值
  beta1_estimate <- coef(fit)[2]
  beta1_se <- summary_fit$coefficients[2, "Std. Error"]
  t_value_beta1[i] <- (beta1_estimate - 1) / beta1_se  # 计算 beta = 1 的 t 值
  
  # 计算 p 值
  p_value_beta1[i] <- 2 * pt(-abs(t_value_beta1[i]), df = summary_fit$df[2])  # 双侧检验的 p 值
  p_value_beta1[i]=round(p_value_beta1[i],2)
  
  # 计算 beta2 = 1 的 t 值
  beta2_estimate <- coef(fit)[3]
  beta2_se <- summary_fit$coefficients[3, "Std. Error"]
  t_value_beta2[i] <- (beta2_estimate ) / beta2_se  # 计算 beta = 1 的 t 值
  
  # 计算 p 值
  p_value_beta2[i] <- 2 * pt(-abs(t_value_beta2[i]), df = summary_fit$df[2])  # 双侧检验的 p 值
  p_value_beta2[i]=round(p_value_beta2[i],2)
  # 提取 R^2
  R2[i] <- summary_fit$r.squared
  
  # 更新索引
  i <- i + 1
}
double.results <- data.frame(
  Fund = newname,
  Alpha = alpha,
  T_Value_Alpha = t_value_alpha,
  P_Value_Alpha = p_value_alpha,
  Beta1 = beta1,
  t_Value_Beta1_1_Test = t_value_beta1,
  P_Value_Beta1_1_Test = p_value_beta1,
  Beta2 = beta2,
  t_Value_Beta2_1_Test = t_value_beta2,
  P_Value_Beta2_1_Test = p_value_beta2,
  R_Squared = R2
)

#fama三因素

fama <- read_excel("fama.xlsx")
fama[1]=as.Date(fama[[1]])
nav.data.reg=inner_join(nav.data.reg,fama,by = "日期")

i=1
alpha=numeric(6)
beta=numeric(6)
lambda1=numeric(6)
lambda2=numeric(6)
t_value_alpha=numeric(6)
t_value_beta=numeric(6)
t_value_lambda1=numeric(6)
t_value_lambda2=numeric(6)
p_value_alpha=numeric(6)
p_value_beta=numeric(6)
p_value_lambda1=numeric(6)
p_value_lambda2=numeric(6)
R2=numeric(6)
for (x in newname) {
  # 使用lm进行普通回归
  fit <- lm(reformulate(c("市场超额收益","市值因子","账面市值比因子"), response = x), data = nav.data.reg)
  
  # 提取回归系数
  alpha[i] <- coef(fit)[1]
  beta[i] <- coef(fit)[2]
  lambda1[i] <- coef(fit)[3]
  lambda2[i] <- coef(fit)[4]
  
  # 提取 t 值和 p 值（截距）
  summary_fit <- summary(fit)
  t_value_alpha[i] <- summary_fit$coefficients[1, "t value"]
  p_value_alpha[i] <- round(summary_fit$coefficients[1, "Pr(>|t|)"], 2)
  
  # 计算 beta1 = 1 的 t 值
  beta1_estimate <- coef(fit)[2]
  beta1_se <- summary_fit$coefficients[2, "Std. Error"]
  t_value_beta1[i] <- (beta1_estimate - 1) / beta1_se  # 计算 beta = 1 的 t 值
  
  # 计算 p 值
  p_value_beta1[i] <- 2 * pt(-abs(t_value_beta1[i]), df = summary_fit$df[2])  # 双侧检验的 p 值
  p_value_beta1[i]=round(p_value_beta1[i],2)
  
  # 提取 t 值和 p 值（市值因子）
  summary_fit <- summary(fit)
  t_value_lambda1[i] <- summary_fit$coefficients[3, "t value"]
  p_value_lambda1[i] <- round(summary_fit$coefficients[3, "Pr(>|t|)"], 2)
  # 提取 t 值和 p 值（账面市值比因子）
  summary_fit <- summary(fit)
  t_value_lambda2[i] <- summary_fit$coefficients[4, "t value"]
  p_value_lambda2[i] <- round(summary_fit$coefficients[4, "Pr(>|t|)"], 2)
  
  # 提取 R^2
  R2[i] <- summary_fit$r.squared
  
  # 更新索引
  i <- i + 1
}
fama.results <- data.frame(
  Fund = newname,
  Alpha = alpha,
  T_Value_Alpha = t_value_alpha,
  P_Value_Alpha = p_value_alpha,
  Beta1 = beta1,
  t_Value_Beta_1_Test = t_value_beta1,
  P_Value_Beta_1_Test = p_value_beta1,
  Lambda1 = lambda1,
  t_Value_lambda1 = t_value_lambda1,
  p_value_lambda1 = p_value_lambda1,
  t_Value_lambda2 = t_value_lambda2,
  p_value_lambda2 = p_value_lambda2,
  R_Squared = R2
)

cov.data=cbind(results[,c(1,2,4,6,8)],summary.nav[,2])
cov.data=cbind(cov.data,sm.results[,2])
colnames(cov.data)[6]="日净值增长"
colnames(cov.data)[7]='alpha值'

#相关性
correlation_matrix <- cor(selected_columns[,2:7])


#业绩分解

market.premium=mean(nav.data.reg[[10]])
beta.risk=sm.results$Beta
premium=summary.nav$均值-mean(rf$`日度化无风险利率(%)`)
risk.premium=beta.risk*market.premium
select.premium=sm.results$Alpha
diversifiable.premium=summary.nav$均值-mean(rf$`日度化无风险利率(%)`)-market.premium
net.selection=select.premium-diversifiable.premium
managerial.risk=risk.premium-market.premium

performance=data.frame(名称=nav.name,
                          超额收益=premium,
                          风险回报=risk.premium,
                          选择回报=select.premium,
                          可分散回报=diversifiable.premium,
                          净选择回报=net.selection,
                          经理人风险回报=managerial.risk)

performance <- performance |> 
  mutate(
    净选择回报排序 = rank(-净选择回报, ties.method = "first"),.after = 净选择回报) |> 
  mutate(
    经理人风险回报排序 = rank(-经理人风险回报, ties.method = "first"),.after=经理人风险回报)

#选择能力----
##TM 
i=1
alpha=numeric(6)
beta=numeric(6)
lambda=numeric(6)
p_alpha=numeric(6)
p_beta=numeric(6)
p_lambda=numeric(6)
R2=numeric(6)
for (x in newname) {
  # 使用lm进行普通回归
  fit <- lm(reformulate(c("市场超额收益","I(市场超额收益^2)"), response = x), data = nav.data.reg)
  
  # 提取回归系数
  alpha[i] <- coef(fit)[1]
  beta[i] <- coef(fit)[2]
  lambda[i] <- coef(fit)[3]
  
  # 提取 t 值和 p 值（截距）
  summary_fit <- summary(fit)
  p_alpha[i] <- round(summary_fit$coefficients[1, "Pr(>|t|)"], 2)
  p_beta[i] <- round(summary_fit$coefficients[2, "Pr(>|t|)"], 2)
  p_lambda[i] <- round(summary_fit$coefficients[3, "Pr(>|t|)"], 2)
  
  # 提取 R^2
  R2[i] <- summary_fit$adj.r.squared
  
  # 更新索引
  i <- i + 1
}
tm.results <- data.frame(
  Fund = newname,
  Alpha = alpha,
  P_Value_Alpha = p_value_alpha,
  Beta = beta,
  P_Value_Beta = p_value_beta,  
  Lambda=lambda,
  P_Value_Lambda=p_lambda,
  R_Squared = R2
)
#HM
D=as.numeric(nav.data.reg[[10]]>0)
nav.data.reg.HM = cbind(nav.data.reg,D)

i=1
alpha=numeric(6)
beta=numeric(6)
lambda=numeric(6)
p_alpha=numeric(6)
p_beta=numeric(6)
p_lambda=numeric(6)
R2=numeric(6)
for (x in newname) {
  # 使用lm进行普通回归
  fit <- lm(reformulate(c("市场超额收益","D"), response = x), data = nav.data.reg.HM)
  
  # 提取回归系数
  alpha[i] <- coef(fit)[1]
  beta[i] <- coef(fit)[2]
  lambda[i] <- coef(fit)[3]
  
  # 提取 t 值和 p 值（截距）
  summary_fit <- summary(fit)
  p_alpha[i] <- round(summary_fit$coefficients[1, "Pr(>|t|)"], 2)
  p_beta[i] <- round(summary_fit$coefficients[2, "Pr(>|t|)"], 2)
  p_lambda[i] <- round(summary_fit$coefficients[3, "Pr(>|t|)"], 2)
  
  # 提取 R^2
  R2[i] <- summary_fit$adj.r.squared
  
  # 更新索引
  i <- i + 1
}
hm.results <- data.frame(
  Fund = newname,
  Alpha = alpha,
  P_Value_Alpha = p_value_alpha,
  Beta = beta,
  P_Value_Beta = p_value_beta,  
  Lambda=lambda,
  P_Value_Lambda=p_lambda,
  R_Squared = R2
)


#Tm三因素
i=1
alpha=numeric(6)
beta=numeric(6)
gamma=numeric(6)
lambda1=numeric(6)
lambda2=numeric(6)
p_value_alpha=numeric(6)
p_value_beta=numeric(6)
p_value_gamma=numeric(6)
p_value_lambda1=numeric(6)
p_value_lambda2=numeric(6)
R2=numeric(6)
for (x in newname) {
  # 使用lm进行普通回归
  fit <- lm(reformulate(c("市场超额收益","I(市场超额收益^2)","市值因子","账面市值比因子"), response = x), data = nav.data.reg)
  
  # 提取回归系数
  alpha[i] <- coef(fit)[1]
  beta[i] <- coef(fit)[2]
  gamma[i] <- coef(fit)[3]
  lambda1[i] <- coef(fit)[4]
  lambda2[i] <- coef(fit)[5]
  
  # 提取 t 值和 p 值（截距）
  summary_fit <- summary(fit)
  p_value_alpha[i] <- round(summary_fit$coefficients[1, "Pr(>|t|)"], 2)
  
  summary_fit <- summary(fit)
  p_value_gamma[i] <- round(summary_fit$coefficients[3, "Pr(>|t|)"], 2)
  # 提取 t 值和 p 值（市值因子）
  summary_fit <- summary(fit)
  p_value_lambda1[i] <- round(summary_fit$coefficients[4, "Pr(>|t|)"], 2)
  # 提取 t 值和 p 值（账面市值比因子）
  summary_fit <- summary(fit)
  p_value_lambda2[i] <- round(summary_fit$coefficients[5, "Pr(>|t|)"], 2)
  
  # 提取 R^2
  R2[i] <- summary_fit$adj.r.squared
  
  # 更新索引
  i <- i + 1
}
fama.tm.results <- data.frame(
  Fund = newname,
  Alpha = alpha,
  P_Value_Alpha = p_value_alpha,
  Beta1 = beta1,
  P_Value = p_value_beta1,
  Gamma = gamma,
  p_value_gamma= p_value_gamma,
  Lambda1 = lambda1,
  p_value_lambda1 = p_value_lambda1,
  Lambda2 = lambda2,
  p_value_lambda2 = p_value_lambda2,
  adj_R_Squared = R2
)


#hm三因素
i=1
alpha=numeric(6)
beta=numeric(6)
gamma=numeric(6)
lambda1=numeric(6)
lambda2=numeric(6)
p_value_alpha=numeric(6)
p_value_beta=numeric(6)
p_value_gamma=numeric(6)
p_value_lambda1=numeric(6)
p_value_lambda2=numeric(6)
R2=numeric(6)
for (x in newname) {
  # 使用lm进行普通回归
  fit <- lm(reformulate(c("市场超额收益","D","市值因子","账面市值比因子"), response = x), data = nav.data.reg.HM)
  
  # 提取回归系数
  alpha[i] <- coef(fit)[1]
  beta[i] <- coef(fit)[2]
  gamma[i] <- coef(fit)[3]
  lambda1[i] <- coef(fit)[4]
  lambda2[i] <- coef(fit)[5]
  
  # 提取 t 值和 p 值（截距）
  summary_fit <- summary(fit)
  p_value_alpha[i] <- round(summary_fit$coefficients[1, "Pr(>|t|)"], 2)
  
  summary_fit <- summary(fit)
  p_value_gamma[i] <- round(summary_fit$coefficients[3, "Pr(>|t|)"], 2)
  # 提取 t 值和 p 值（市值因子）
  summary_fit <- summary(fit)
  p_value_lambda1[i] <- round(summary_fit$coefficients[4, "Pr(>|t|)"], 2)
  # 提取 t 值和 p 值（账面市值比因子）
  summary_fit <- summary(fit)
  p_value_lambda2[i] <- round(summary_fit$coefficients[5, "Pr(>|t|)"], 2)
  
  # 提取 R^2
  R2[i] <- summary_fit$adj.r.squared
  
  # 更新索引
  i <- i + 1
}
fama.hm.results <- data.frame(
  Fund = newname,
  Alpha = alpha,
  P_Value_Alpha = p_value_alpha,
  Beta1 = beta1,
  P_Value = p_value_beta1,
  Gamma = gamma,
  p_value_gamma= p_value_gamma,
  Lambda1 = lambda1,
  p_value_lambda1 = p_value_lambda1,
  Lambda2 = lambda2,
  p_value_lambda2 = p_value_lambda2,
  adj_R_Squared = R2
)

#计算季度平均收益率-----

# 加载必要的包
library(lubridate)

#名称转换方便计算
transname=paste0("fund_", 1:5)
data.quarter=nav.data
data.quarter=data.quarter[1:7]
data.quarter=data.quarter[-2]
colnames(data.quarter)[2:6]=transname

# 将日期转为季度
data.quarter <- data.quarter %>%
  mutate(季度 = paste(year(日期), quarter(日期), sep = "-Q"))


# 按季度计算每列基金的平均收益率
quarter.return <- data.quarter %>%
  group_by(季度) %>%
  summarize(across(starts_with("fund_"), ~ mean(.x, na.rm = TRUE), .names = "{.col}"))

# 计算每季度的中位数
data_median <- quarter.return %>%
  rowwise() %>%
  mutate(median = median(c_across(starts_with("fund_")))) %>%
  ungroup()

# 标记每只基金是否超过中位数
data_flag <- data_median %>%
  mutate(across(starts_with("fund_"), ~ . > median, .names = "flag_{col}"))

# 创建一个偏移的数据框用于比较下一季度
data_next <- data_flag[-1, 8:12]  # 删除第一行并选择第 8 到 12 列

# 删除最后一行以对齐当前季度
data_flag <- data_flag %>%
  slice(-n())

# 添加“quarter”列到对齐后的数据框
data_flag <- data_flag |> 
  select(季度, starts_with("flag_fund"))

# 计算每个季度的WW, WL, LW, LL
results <- data_flag %>%
  mutate(
    WW = rowSums(select(data_flag, starts_with("flag_fund")) & data_next),
    WL = rowSums(select(data_flag, starts_with("flag_fund")) & !data_next),
    LW = rowSums(!select(data_flag, starts_with("flag_fund")) & data_next),
    LL = rowSums(!select(data_flag, starts_with("flag_fund")) & !data_next)
  ) %>%
  select(季度, WW, WL, LW, LL)

results=results |> 
  mutate(Ta = (WW+WL)*(WW+LW)/5,
         Tb = (WW+WL)*(WL+LW)/5,
         Tc = (WW+LW)*(LL+LW)/5,
         Td = (LL+WL)*(LL+LW)/5,
         ) |> 
  mutate(x2 = (WW-Ta)^2/Ta+(WL-Tb)^2/Tb+(LW-Tc)^2/Tc+(LL-Td)^2/Td)



#回归方法
#使用单因素模型所得的alpha值，并选取月度平均数据

#；流动性
fluidity.data <- read_excel("交易.xlsx", sheet = "Sheet2")
index = names(fluidity.data[,c(3:7)])
index2 = names(fluidity.data[,c(8:11)])

fluidity.data.2021 = fluidity.data |> filter(year==2021)
fluidity.data.2022 = fluidity.data |> filter(year==2022)
fluidity.data.2023 = fluidity.data |> filter(year==2023)
fluidity.data.2024 = fluidity.data |> filter(year==2024)

result.2021 = fluidity.data.2021[,-1:-2] |> 
  pivot_longer(cols = all_of(index),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value)/10^6,
    中位数 = median(value)/10^6,
    最大值 = max(value)/10^6,
    最小值 = min(value)/10^6,
    标准差 = sqrt(var(value/10^6))
    
  )
result.2022 = fluidity.data.2022[,-1:-2] |> 
  pivot_longer(cols = all_of(index),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value/10^6),
    中位数 = median(value/10^6),
    最大值 = max(value/10^6),
    最小值 = min(value/10^6),
    标准差 = sqrt(var(value/10^6))
    
  )
result.2023 = fluidity.data.2023[,-1:-2] |> 
  pivot_longer(cols = all_of(index),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value/10^6),
    中位数 = median(value/10^6),
    最大值 = max(value/10^6),
    最小值 = min(value/10^6),
    标准差 = sqrt(var(value/10^6))
    
  )
result.2024 = fluidity.data.2024[,-1:-2] |> 
  pivot_longer(cols = all_of(index),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value/10^6),
    中位数 = median(value/10^6),
    最大值 = max(value/10^6),
    最小值 = min(value/10^6),
    标准差 = sqrt(var(value/10^6))
    
  )

result.2021 = fluidity.data.2021[,-1:-2] |> 
  pivot_longer(cols = all_of(index2),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value*100),
    中位数 = median(value*100),
    最大值 = max(value*100),
    最小值 = min(value*100),
    标准差 = sqrt(var(value*100))
    
  )
result.2022 = fluidity.data.2022[,-1:-2] |> 
  pivot_longer(cols = all_of(index2),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value*100),
    中位数 = median(value*100),
    最大值 = max(value*100),
    最小值 = min(value*100),
    标准差 = sqrt(var(value*100))
    
  )
result.2023 = fluidity.data.2023[,-1:-2] |> 
  pivot_longer(cols = all_of(index2),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value*100),
    中位数 = median(value*100),
    最大值 = max(value*100),
    最小值 = min(value*100),
    标准差 = sqrt(var(value*100))
    
  )
result.2024 = fluidity.data.2024[,-1:-2] |> 
  pivot_longer(cols = all_of(index2),names_to = "index",values_to = 'value') |> 
  group_by(index) |> 
  summarise(
    均值 = mean(value*100),
    中位数 = median(value*100),
    最大值 = max(value*100),
    最小值 = min(value*100),
    标准差 = sqrt(var(value*100))
    
  )


#流动性与基金业绩
#风险调整收益

sharpe_everyday = nav.data |> 
  mutate(建信深证基本面60ETF夏普 = 建信深证基本面60ETF超额收益/sqrt(var(建信深证基本面60ETF)),
         工银瑞信深证100ETF夏普 = 工银瑞信深证100ETF超额收益/sqrt(var(工银瑞信深证100ETF)),
         国联安沪深300ETF夏普 = 国联安沪深300ETF超额收益/sqrt(var(国联安沪深300ETF)),
         景顺长城成长之星股票A夏普 = 景顺长城成长之星股票A超额收益/sqrt(var(景顺长城成长之星股票A)),
         保险主题LOF夏普 = 保险主题LOF超额收益/sqrt(var(保险主题LOF超额收益))
         )
sharpe_everyday=sharpe_everyday[,c(1,17:21)]

sharpe_everyday=sharpe_everyday |> 
  mutate(year = year(日期),quarter = quarter(日期),id = 1:nrow(sharpe_everyday))

sharpe_quarter = aggregate(.~year+quarter,data = sharpe_everyday[,-1],FUN = mean)
sharpe_quarter=sharpe_quarter |> 
  arrange(id)
sharpe_quarter=sharpe_quarter[,-8]

#几何平均收益

return.data = nav.data[,c(1,3:7)]
return.data = return.data |> 
  mutate(year = year(日期),
         quarter=quarter(日期),
         id = 1:nrow(nav.data))
geo_mean <- function(x) {
  # (1 + 收益) 的乘积，再取 n 次方根，再减去 1
  100*(prod(1 + x/100, na.rm = TRUE)^(1 / length(x)) - 1)
}
return.quarter = aggregate(.~year+quarter,data = return.data[,-1],FUN = geo_mean)
return.quarter=return.quarter |> 
  arrange(desc(id))
return.quarter=return.quarter[,-8]


fluidity.data <- read_excel("交易.xlsx", sheet = "Sheet3", 
                            col_types = c("text", "text", "numeric", 
                                          "numeric", "numeric", "numeric"))
fluidity.data = fluidity.data |> 
  mutate(
    year = year(日期),
    quarter=quarter(日期)
  )
PR.data = fluidity.data[,c(1,3,7,8)]
PR.data=PR.data |> 
  pivot_wider(
      names_from = c("名称"),
      values_from = "PR"
  )

RR.data = fluidity.data[,c(1,4,7,8)]
RR.data=RR.data |> 
  pivot_wider(
    names_from = c("名称"),
    values_from = "RR"
  )


NAVCR.data = fluidity.data[,c(1,5,7,8)]
NAVCR.data=NAVCR.data |> 
  pivot_wider(
    names_from = c("名称"),
    values_from = "NAVCR"
  )

TR.data = fluidity.data[,c(1,6:8)]
TR.data=TR.data |> 
  pivot_wider(
    names_from = c("名称"),
    values_from = "TR"
  )

sharpe.reg <- read_excel("sharpe.xlsx")

coef=numeric(4)
t=numeric(4)
p=numeric(4)
R2=numeric(4)
dw=numeric(4)

x=1

for (i in 5:8) {
  fit = lm(sharpe~.,data = sharpe.reg[,c(3,i)])
  DW=dwtest(fit)
  coef[x] = fit$coefficients[2]
  t[x] = summary(fit)$coefficients[, "t value"][2]
  p[x] = summary(fit)$coefficients[, "Pr(>|t|)"][2]
  R2[x] = summary(fit)$r.squared
  dw[x] = DW$statistic[[1]]
  x=x+1
}
result1 = data.frame(名称 = c("PR","RR","NAVCR","TR"),
                    系数 = coef,
                    t值 = t,
                    p值 = p,
                    R2 = R2,
                    DW = dw)
coef=numeric(4)
t=numeric(4)
p=numeric(4)
R2=numeric(4)
dw=numeric(4)

x=1

for (i in 5:8) {
  fit = lm(return~.,data = sharpe.reg[,c(4,i)])
  DW=dwtest(fit)
  coef[x] = fit$coefficients[2]
  t[x] = summary(fit)$coefficients[, "t value"][2]
  p[x] = summary(fit)$coefficients[, "Pr(>|t|)"][2]
  R2[x] = summary(fit)$r.squared
  dw[x] = DW$statistic[[1]]
  x=x+1
}

result2 = data.frame(名称 = c("PR","RR","NAVCR","TR"),
                     系数 = coef,
                     t值 = t,
                     p值 = p,
                     R2 = R2,
                     DW = dw)


sharpe.reg.lag = read_excel("sharpe.xlsx", sheet = "2")

coef=numeric(4)
t=numeric(4)
p=numeric(4)
R2=numeric(4)
dw=numeric(4)

x=1

for (i in 5:8) {
  fit = lm(sharpe~.,data = sharpe.reg.lag[,c(3,i)])
  DW=dwtest(fit)
  coef[x] = fit$coefficients[2]
  t[x] = summary(fit)$coefficients[, "t value"][2]
  p[x] = summary(fit)$coefficients[, "Pr(>|t|)"][2]
  R2[x] = summary(fit)$r.squared
  dw[x] = DW$statistic[[1]]
  x=x+1
}

result3 = data.frame(名称 = c("PR","RR","NAVCR","TR"),
                     系数 = coef,
                     t值 = t,
                     p值 = p,
                     R2 = R2,
                     DW = dw)

coef=numeric(4)
t=numeric(4)
p=numeric(4)
R2=numeric(4)
dw=numeric(4)

x=1

for (i in 5:8) {
  fit = lm(return~.,data = sharpe.reg.lag[,c(4,i)])
  DW=dwtest(fit)
  coef[x] = fit$coefficients[2]
  t[x] = summary(fit)$coefficients[, "t value"][2]
  p[x] = summary(fit)$coefficients[, "Pr(>|t|)"][2]
  R2[x] = summary(fit)$r.squared
  dw[x] = DW$statistic[[1]]
  x=x+1
}

result4 = data.frame(名称 = c("PR","RR","NAVCR","TR"),
                     系数 = coef,
                     t值 = t,
                     p值 = p,
                     R2 = R2,
                     DW = dw)
