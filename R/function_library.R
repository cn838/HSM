load_9_35_site_functions= function(){
  Nexpected_b = function(P_b,O_b,w) (P_b*w+(1-w)*O_b) #(9A.1-1)

  Nexpected_a = function(Nexpected_b,adj) (Nexpected_b*adj) #(9A.1-4)

  OR.i = function(O_a,Nexp_a) (O_a)/(Nexp_a) #(9A.1-5) # Lines 72-82 Edit: Removed sum function from denominator and numerator 1/24

  VAR.=function(adj,Nexp_b,w) (adj^(2)*Nexp_b*(1-w)) #(9A.1-9)

  OR.=function(OR.i,VAR.,Nexp_a) (OR.i)/(1+VAR.)/Nexp_a^2 #(9A.1-8)

  SE.=function(OR.) (100*(1-OR.)) # (9A.1-10)

  VarOR.=function(OR.i,O_a,VAR.,Nexp_a) (OR.i)^(2)*(1/(O_a)+(VAR.)/(Nexp_a)^(2)/(1+(VAR.)/(Nexp_a)^2)) #(9A.1-11)

  Std.Err.OR.=function(VarOR.) ((VarOR.)^(1/2)) #(9A.1-12)

  Std.Err.SE.=function(Std.Err.OR.) (100*Std.Err.OR.) #(9A.1-13)

  test_statistic.=function(SE.,Std.Err.SE.)  (abs(SE./Std.Err.SE.)) #Step 14 (a)

  Significance.test=function(test_statistic) ifelse(test_statistic <1.7, "N.S.",
                                                    ifelse(test_statistic < 2 & test_statistic >= 1.7 ,"90%",
                                                           ifelse(test_statistic < Inf & test_statistic>= 2.0, "95%",
                                                                  ifelse(test_statistic==Inf,"Inf",""))))
  }

