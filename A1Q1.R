# clear screen and console
cat("\014")  
rm(list=ls())

# history data
x=c(-0.08955, -0.116457, 0.081953, 0.089772, 0.051721, 0.000196, 0.071522, 0.033009, 0.0351, -0.01996, 0.055779, 0.017615, -0.037675, 0.028115, 0.057133, 0.014651, -0.085532, -0.055388, 0.066516, -0.048612, 0.083928, 0.036193, -0.002293, 0.063257, 0.022393, 0.031457, -0.001048, 0.028097, -0.013593, -0.018426, -0.021708, -0.058467, -0.074467, 0.102307, -0.005071, 0.008497, 0.04266, 0.039787, 0.030852, -0.007526, -0.064699, 0.038793, 0.012519, 0.019571, 0.023947, -0.019988, 0.002843, 0.007043)
y=c(-0.039286, 0.120337, -0.032261, 0.109334, 0.102978, -0.011429, -0.089453, 0.020049, 0.198054, -0.113441, -0.060271, 0.11414, -0.111471, 0.01979, 0.076671, 0, -0.074713, -0.102901, 0.002886, -0.057074, 0.077752, 0.151627, -0.041479, 0.049933, -0.031148, 0.017221, 0.016929, 0.059354, -0.067179, -0.095673, -0.138101, 0.038193, -0.032863, 0.17189, 0.004466, 0.026383, -0.041778, -0.042251, 0.025958, 0.020807, -0.019494, 0.037983, 0.000632, -0.078098, 0.086898, 0.052419, 0.108503, 0.05846)
# load xnext from xnext to predict ynext
xnext=c(0.049198, 0.011, 0.035355, 0.017924, 0.02055, -0.015113, 0.048278, -0.031798, 0.029316, 0.04363)

# build model
Model<-lm(y~x) # regression line
summary<-summary(Model) # find b0 b1

# find t critical value for 95% 
t_critical <- qt(0.975, length(x)-2)

# predict ynext from existing xnet
# x at 0.049198
x<-0.049198 
p_1<-predict(Model,x, interval='prediction')

# x at 0.04363
x<-0.04363
p_2<-predict(Model,x, interval='prediction')

# last question, find the predicted interval for all the xnext
ynext=c(-0.013661, 0.082117, 0.0991, 0.049741, 0.061552, -0.045507, 0.111351, -0.035142, 0.201374, -0.006958)
answer <- 0
count <- 1
for (each_x in xnext){
  x<-each_x
  predicted_each_ynext = predict.lm(Model, x, interval='prediction')
  # predicted output looks like
  # fit->1 lwr->2 upr->3 
  lower_bond = predicted_each_ynext[2] # lwr of p
  upper_bond = predicted_each_ynext[3] # upr of p
  true_value <- ynext[count]
  if ((true_value >= lower_bond) && (true_value <= upper_bond) ){
    answer <- answer + 1
  }
  count <- count + 1
}





