library("xts")
mydata = read.csv("feds200628.csv")
mydata$X = as.Date(mydata$X)
rownames(mydata) = mydata[,1]
mydata$X = NULL
date_axis = row.names(mydata)
mydata = as.xts(mydata)
mydata = mydata["1983-12-30/2018-06-29"]
weekly = endpoints(mydata,"weeks", k=1)
weekly = weekly[-1]

date_axis = index(mydata[weekly][-1])
x_date =  format(date_axis,"%h %d,%Y")

t10 = 9+358/365
t2 = 1 + 358/365
t_week = 7 / 365

ncol = length(mydata[1,])
initial_capital = 1000000
result = matrix(nrow = length(weekly),ncol = 15)

convexity_return = vector()
colnames(result) = c("unit 2","unit 10","price2","price10","interest","capital","weekly_return","convexity_2","convexity_10","spread_return","convexity_return","time_return","residual_return","a","b")


bond_unit = function(modified_D2,modified_D10,price2,price10,cash,margin){
  A = matrix(data=c((modified_D2 * price2), (modified_D10 * price10), -price2, price10), nrow=2, ncol=2, byrow=TRUE)
  b = matrix(data=c(0,cash /margin),nrow=2, ncol=1, byrow=FALSE)
  result = solve(A,b)
  return(result)
}

NSS_model = function(parameters,t){
  beta0 = parameters[1]
  beta1 = parameters[2]
  beta2 = parameters[3]
  beta3 = parameters[4]
  tau1 = parameters[5]
  tau2 = parameters[6]
  t1 = t/tau1
  t2 = t/tau2
  e1 = exp(-t1)
  e2 = exp(-t2)
  return(beta0 + beta1 * (1 - e1)/t1 + beta2 * ((1 - e1)/t1 - e1) + beta3 * ((1 - e2) / t2 - e2))
}

myfunction = function(initial_capital=1000000,margin=0.1,result){
  weekly_return = 0
  spread_return = 0
  convexity_return = 0
  time_return = 0
  residual_return = 0
  for (i in c(1:(length(weekly)-1))){
    if (any(is.na(mydata[weekly[i],][,c(2,10,(ncol -5):ncol)]))){
      startweek = as.vector(mydata[weekly[i]+1,])
    }
    else{
      startweek = as.vector(mydata[weekly[i],])
    }
    if (any(is.na(mydata[weekly[i+1],][,c(2,10,(ncol -5):ncol)]))){
      endweek = as.vector(mydata[weekly[i+1]+1,])
    }
    else{
      endweek = as.vector(mydata[weekly[i+1],])
    }
    start_y2 = startweek[2]/100
    start_y10 = startweek[10]/100
    modified_D2 = 2 / exp(start_y2)
    modified_D10 = 10 / exp(start_y10)
    convexity_2 = 2^2
    convexity_10 = 10^2
    start_p2 = 100 / exp(start_y2 * 2)
    start_p10 = 100 / exp(start_y10 * 10)
    bond = bond_unit(modified_D2 = modified_D2, modified_D10 = modified_D10,price2 = start_p2,price10 = start_p10, cash = initial_capital,margin = margin)
    
    ini_cash = abs(bond[1] * start_p2) - abs(bond[2] * start_p10) + initial_capital
                                               
    st_parameter = startweek[(length(startweek) - 5):length(startweek)]
    ed_parameter = endweek[(length(endweek) - 5):length(endweek)]
    
    y2 = endweek[2]/100
    y10 = endweek[10]/100
    
    t_week = 7 / 365
    y_w = NSS_model(parameters= st_parameter,t=t_week)/100
    y9_358_st = NSS_model(parameters= st_parameter,t=t10)/100
    y1_358_st = NSS_model(parameters = st_parameter, t = t2)/100
    
    y1_358_ed = NSS_model(parameters = ed_parameter,t=t2)/100
    y9_358_ed = NSS_model(parameters = ed_parameter, t=t10)/100
    
    
    price10 = 100 / exp(y9_358_ed * t10)
    price2 = 100 / exp(y1_358_ed * t2)
    interest = ini_cash * exp(y_w * t_week) - ini_cash
    end_capital = ini_cash + interest + bond[2] * price10 + bond[1]* price2
    weekly_return = end_capital - initial_capital
    delta_y2 = y2 - start_y2
    delta_y10 = y10 - start_y10
    spread_delta_p2 = -modified_D2 * bond[1] * start_p2 * delta_y2
    spread_delta_p10 = -modified_D10 * bond[2] * start_p10 * delta_y10
    spread_return = spread_delta_p2 + spread_delta_p10
    
    convexity_delta_p2 = bond[1] * start_p2 * 0.5 * convexity_2 * delta_y2^2
    convexity_delta_p10 = bond[2] * start_p10 * 0.5 * convexity_10 * delta_y10^2
    convexity_return = convexity_delta_p2 + convexity_delta_p10
    
    time_delta_p2 = 100/exp(y1_358_st * t2) - start_p2
    time_delta_p10 = 100/exp(y9_358_st * t10) - start_p10
    time_return = bond[1] * time_delta_p2 + bond[2] * time_delta_p10 + interest
    initial_capital = end_capital
    
    residual_return = weekly_return - spread_return - convexity_return - time_return
    result[i,] = c(bond,price2,price10,interest,initial_capital,weekly_return,convexity_2,convexity_10,spread_return,convexity_return,time_return,residual_return,delta_y2,delta_y10)
    if (initial_capital <= 0){
      break
    }
    
  }
  return(result)
}



convexity_risk = function(result){
  for (i in c(1:(length(weekly)-1))){
    if (any(is.na(mydata[weekly[i],][,c(2,10,(ncol -5):ncol)]))){
      startweek = as.vector(mydata[weekly[i]+1,])
    }
    else{
      startweek = as.vector(mydata[weekly[i],])
    }
    if (any(is.na(mydata[weekly[i+1],][,c(2,10,(ncol -5):ncol)]))){
      endweek = as.vector(mydata[weekly[i+1]+1,])
    }
    else{
      endweek = as.vector(mydata[weekly[i+1],])
    }
    start_y2 = startweek[2]/100
    start_y10 = startweek[10]/100
    modified_D2 = 2 / exp(start_y2)
    modified_D10 = 10 / exp(start_y10)
    convexity_2 = 2^2
    convexity_10 = 10^2
    start_p2 = 100 / exp(start_y2 * 2)
    start_p10 = 100 / exp(start_y10 * 10)
    #bond = bond_unit(modified_D2 = modified_D2, modified_D10 = modified_D10,price2 = start_p2,price10 = start_p10, cash = initial_capital,margin = margin)
    # 1M/100 * start_p10 * modified_D10 + unit_2 * start_p2 * modified_D2 = 0
    unit_2 = -(10000 * start_p10 * modified_D10) / start_p2 / modified_D2
    delta_y = 0.001
    initial_capital = (unit_2 * start_p2 + 10000 * start_p10) * 0.1
    
    
    
    convexity_delta_p2 = unit_2 * start_p2 * 0.5 * convexity_2 * delta_y^2
    convexity_delta_p10 = 10000 *  start_p10 * 0.5 * convexity_10 * delta_y^2
    convexity_return = (convexity_delta_p2 + convexity_delta_p10)
    #cash = end_capital
    result[i] = convexity_return
    
  }
  return(result)
}


convexity_return = convexity_risk(convexity_return)

plot(x = date_axis,convexity_return,type = "l",col = "red",xaxt="n", xlab="date", ylab = "cumulative return ($)", main = "Convexity Risk While Fixing Long Position")
axis(side = 1, at = date_axis, labels = x_date)
grid(NULL,NULL,col="lightgrey",lty="dotted")

a = myfunction(1000000,margin = 0.1, result = result)[-1801,]
cum_return = cumsum(a[,c(7,10,11,12,13)])

myplot = function(a){
  plot(x = date_axis,cumsum(a[,7]),type = "l",col = "red",ylim = c(-5e+05,4.1e+05),xaxt="n", xlab="date", ylab = "cumulative return ($)", main = "Return Analytical Plot")
  axis(side = 1, at = date_axis, labels = x_date)
  lines(x = date_axis,cumsum(a[,10]),type = "l",col="blue")
  lines(x = date_axis,cumsum(a[,11]),type = "l",col = "orange")
  lines(x = date_axis,cumsum(a[,12]),type = "l",col = "green")
  lines(x = date_axis,cumsum(a[,13]),type = "l",col = "black")
  legend("topright", legend = c("total return","spread return","convexity return","time return","residual return"), col = c("red","blue","orange","green","black"),lty=1,cex=0.6)
  grid(NULL,NULL,col="lightgrey",lty="dotted")
}


myplot_b = function(a,b){
  plot(x = date_axis,cumsum(a[,7]),type = "l",col = "blue", xlab = "date", ylab = "cumulative return ($)", xaxt = "n", main = "Return Comparision With Different Margin")
  axis(side = 1, at = date_axis, labels = x_date)
  lines(x = date_axis,cumsum(b[,7]),type = "l",col="red")
  
  legend("topright", legend = c("10% margin","2% margin"), col = c("red","blue"),lty=1)
  grid(NULL,NULL,col="lightgrey",lty="dotted")
}

myplot(a=a)

b = myfunction(1000000,margin = 0.02, result = result)[-1801,]
myplot_b(b,a)
