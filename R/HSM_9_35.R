

#' 2010 HSM Section 9-35 Site Specific Empirical Bayes Analysis
#'
#' @param data Input data set. Requires user to specify the safety performance function, observed crashes, and AADT. Safety Performance Function title names are specified according to the titles provided in the FHWA "The Calibrator".
#' @param group Input parameter for Step 14. No default is specified. User specifies TRUE or FALSE for Step 14, respectively.
#' @param segment Input parameter for segments. No default is specified. User specifies TRUE for Segments or FALSE. Note if segments are used the length (L) column must be populated (units are in miles).
#'
#' @return A tibble
#' @export
#'
#' @examples
#' HSM_9_35(data=X9_10_seg,group=FALSE,segment = TRUE) # Only the HSM example 9.10 SPF HSM-RUR2U-KABCO is available with version 1.0.
#'
#' HSM_9_35(data=X9_10_seg,group=TRUE,segment = TRUE)
#'
#' HSM_9_35(data=X9_10_int,group=FALSE,segment = FALSE)# All FI and Total crash intersection SPF are available with version 1.0.
#'
#' HSM_9_35(data=X9_10_int,group=TRUE,segment = FALSE)
#'
HSM_9_35 = function(data,group,segment){{
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

  data |>
    dplyr::select(starts_with(c("Site_No.","Yrs","Base","L","C","CMF","Before","After"))) |>
    pivot_longer(cols = -c(Site_No.:C,CMF,Yrs_Before,Yrs_After), names_to = c(".value", "Year"), names_sep = "_", values_drop_na = TRUE) |>
    fill(L, .direction = "downup") |>
    drop_na(Year)|> rowwise() |>
    dplyr::mutate(
      P.T.B= if(segment==TRUE) {C*CMF*spf(AADTMAJ=Before.AADT.Major,AADTMIN=NULL,L=L,Base_Condition=Base_Past,Provide_Overdispersion_Factor=FALSE,Segment=TRUE)} else
      {C*CMF*spf(AADTMAJ=Before.AADT.Major,AADTMIN=Before.AADT.Minor,L=NULL,Base_Condition=Base_Past,Provide_Overdispersion_Factor=FALSE,Segment=FALSE)},
      P.T.A= if(segment==TRUE) {
        C*CMF*spf(AADTMAJ=After.AADT.Major,AADTMIN=NULL,L=L,Base_Condition=Base_Past,Provide_Overdispersion_Factor=FALSE,Segment=TRUE)} else
        {C*CMF*spf(AADTMAJ=After.AADT.Major,AADTMIN=After.AADT.Minor,L=NULL,Base_Condition=Base_Past,Provide_Overdispersion_Factor=FALSE,Segment=FALSE)})|>
    pivot_wider(
      names_from = Year,
      names_sep = "_",
      values_from = c(Before.AADT.Major,Before.AADT.Minor, After.AADT.Major,After.AADT.Minor, Before.Yr,After.Yr, P.T.B,P.T.A ))%>% rowwise() |> dplyr::mutate( # Begin Table Layout and Empirical Bayes Testing Procedure

        O_b=ifelse(
          Yrs_Before == 1, rowSums(across(Before.Yr_1)),
          ifelse(
            Yrs_Before == 2, rowSums(across(Before.Yr_1:Before.Yr_2)),
            ifelse(
              Yrs_Before == 3, rowSums(across(Before.Yr_1:Before.Yr_3)),
              ifelse(
                Yrs_Before == 4, rowSums(across(Before.Yr_1:Before.Yr_4)),
                ifelse(
                  Yrs_Before == 5, rowSums(across(Before.Yr_1:Before.Yr_5)),
                  ifelse(
                    Yrs_Before == 6, rowSums(across(Before.Yr_1:Before.Yr_6)),
                    ifelse(
                      Yrs_Before == 7, rowSums(across(Before.Yr_1:Before.Yr_7)),
                      ifelse(
                        Yrs_Before == 8, rowSums(across(Before.Yr_1:Before.Yr_8)),
                        ifelse(
                          Yrs_Before == 9 ~ rowSums(across(Before.Yr_1:Before.Yr_9)),
                          ifelse(
                            Yrs_Before == 10 ~ rowSums(across(Before.Yr_1:Before.Yr_10),""))))))))))),

        Avg_AADTmaj_b=ifelse(
          Yrs_Before == 1, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_1)),
          ifelse(
            Yrs_Before == 2, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_2)),
            ifelse(
              Yrs_Before == 3, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_3)),
              ifelse(
                Yrs_Before == 4, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_4)),
                ifelse(
                  Yrs_Before == 5, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_5)),
                  ifelse(
                    Yrs_Before == 6, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_6)),
                    ifelse(
                      Yrs_Before == 7, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_7)),
                      ifelse(
                        Yrs_Before == 8, rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_8)),
                        ifelse(
                          Yrs_Before == 9 ~ rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_9)),
                          ifelse(
                            Yrs_Before == 10 ~ rowMeans(across(Before.AADT.Major_1:Before.AADT.Major_10),""))))))))))),

        Avg_AADTmin_b=ifelse(
          Yrs_Before == 1, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_1)),
          ifelse(
            Yrs_Before == 2, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_2)),
            ifelse(
              Yrs_Before == 3, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_3)),
              ifelse(
                Yrs_Before == 4, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_4)),
                ifelse(
                  Yrs_Before == 5, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_5)),
                  ifelse(
                    Yrs_Before == 6, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_6)),
                    ifelse(
                      Yrs_Before == 7, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_7)),
                      ifelse(
                        Yrs_Before == 8, rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_8)),
                        ifelse(
                          Yrs_Before == 9 ~ rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_9)),
                          ifelse(
                            Yrs_Before == 10 ~ rowMeans(across(Before.AADT.Minor_1:Before.AADT.Minor_10)),"")))))))))),

        P_b=ifelse(
          Yrs_Before == 1, rowSums(across(P.T.B_1:P.T.B_1)),
          ifelse(
            Yrs_Before == 2, rowSums(across(P.T.B_1:P.T.B_2)),
            ifelse(
              Yrs_Before == 3, rowSums(across(P.T.B_1:P.T.B_3)),
              ifelse(
                Yrs_Before == 4, rowSums(across(P.T.B_1:P.T.B_4)),
                ifelse(
                  Yrs_Before == 5, rowSums(across(P.T.B_1:P.T.B_5)),
                  ifelse(
                    Yrs_Before == 6, rowSums(across(P.T.B_1:P.T.B_6)),
                    ifelse(
                      Yrs_Before == 7, rowSums(across(P.T.B_1:P.T.B_7)),
                      ifelse(
                        Yrs_Before == 8, rowSums(across(P.T.B_1:P.T.B_8)),
                        ifelse(
                          Yrs_Before == 9 ~ rowSums(across(P.T.B_1:P.T.B_9)),
                          ifelse(
                            Yrs_Before == 10 ~ rowSums(across(P.T.B_1:P.T.B_10),""))))))))))),


        Avg_AADTmaj_a=ifelse(
          Yrs_After == 1, rowMeans(across(After.AADT.Major_1:After.AADT.Major_1)),
          ifelse(
            Yrs_After == 2, rowMeans(across(After.AADT.Major_1:After.AADT.Major_2)),
            ifelse(
              Yrs_After == 3, rowMeans(across(After.AADT.Major_1:After.AADT.Major_3)),
              ifelse(
                Yrs_After == 4, rowMeans(across(After.AADT.Major_1:After.AADT.Major_4)),
                ifelse(
                  Yrs_After == 5, rowMeans(across(After.AADT.Major_1:After.AADT.Major_5)),
                  ifelse(
                    Yrs_After == 6, rowMeans(across(After.AADT.Major_1:After.AADT.Major_6)),
                    ifelse(
                      Yrs_After == 7, rowMeans(across(After.AADT.Major_1:After.AADT.Major_7)),
                      ifelse(
                        Yrs_After == 8, rowMeans(across(After.AADT.Major_1:After.AADT.Major_8)),
                        ifelse(
                          Yrs_After == 9 ~ rowMeans(across(After.AADT.Major_1:After.AADT.Major_9)),
                          ifelse(
                            Yrs_After == 10 ~ rowMeans(across(After.AADT.Major_1:After.AADT.Major_10)),"")))))))))),

        Avg_AADTmin_a=ifelse(
          Yrs_After == 1, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_1)),
          ifelse(
            Yrs_After == 2, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_2)),
            ifelse(
              Yrs_After == 3, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_3)),
              ifelse(
                Yrs_After == 4, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_4)),
                ifelse(
                  Yrs_After == 5, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_5)),
                  ifelse(
                    Yrs_After == 6, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_6)),
                    ifelse(
                      Yrs_After == 7, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_7)),
                      ifelse(
                        Yrs_After == 8, rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_8)),
                        ifelse(
                          Yrs_After == 9 ~ rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_9)),
                          ifelse(
                            Yrs_After == 10 ~ rowMeans(across(After.AADT.Minor_1:After.AADT.Minor_10)),"")))))))))),
        P_a=ifelse(
          Yrs_After == 1, rowSums(across(P.T.A_1:P.T.A_1)),
          ifelse(
            Yrs_After == 2, rowSums(across(P.T.A_1:P.T.A_2)),
            ifelse(
              Yrs_After == 3, rowSums(across(P.T.A_1:P.T.A_3)),
              ifelse(
                Yrs_After == 4, rowSums(across(P.T.A_1:P.T.A_4)),
                ifelse(
                  Yrs_After == 5, rowSums(across(P.T.A_1:P.T.A_5)),
                  ifelse(
                    Yrs_After == 6, rowSums(across(P.T.A_1:P.T.A_6)),
                    ifelse(
                      Yrs_After == 7, rowSums(across(P.T.A_1:P.T.A_7)),
                      ifelse(
                        Yrs_After == 8, rowSums(across(P.T.A_1:P.T.A_8)),
                        ifelse(
                          Yrs_After == 9 ~ rowSums(across(P.T.A_1:P.T.A_9)),
                          ifelse(
                            Yrs_After == 10 ~ rowSums(across(P.T.A_1:P.T.A_10),""))))))))))),

        k=if(segment==TRUE) {
          spf(L=L,Base_Condition=Base_Past,Provide_Overdispersion_Factor=TRUE,Segment=TRUE)} else
          {
            spf(AADTMAJ=NULL,AADTMIN=NULL,L=NULL,Base_Condition=Base_Past,Provide_Overdispersion_Factor=TRUE,Segment=FALSE)},

        w=(1/(1+k*P_b)), # (9A.1-2)

        adj=(P_a/P_b),  # (9A.1-3)

        Nexp_b= Nexpected_b(P_b,O_b,w), # Nexp,b=Predicted*w+(1-w)O_bs,b; HSM (9A.1-4) #bayes estimates

        Nexp_a=Nexpected_a(Nexp_b,adj), # (9A.1-4)

        O_a=ifelse(
          Yrs_After == 1, rowSums(across(After.Yr_1)),
          ifelse(
            Yrs_After == 2, rowSums(across(After.Yr_1:After.Yr_2)),
            ifelse(
              Yrs_After == 3, rowSums(across(After.Yr_1:After.Yr_3)),
              ifelse(
                Yrs_After == 4, rowSums(across(After.Yr_1:After.Yr_4)),
                ifelse(
                  Yrs_After == 5, rowSums(across(After.Yr_1:After.Yr_5)),
                  ifelse(
                    Yrs_After == 6, rowSums(across(After.Yr_1:After.Yr_6)),
                    ifelse(
                      Yrs_After == 7, rowSums(across(After.Yr_1:After.Yr_7)),
                      ifelse(
                        Yrs_After == 8, rowSums(across(After.Yr_1:After.Yr_8)),
                        ifelse(
                          Yrs_After == 9 ~ rowSums(across(After.Yr_1:After.Yr_9)),
                          ifelse(
                            Yrs_After == 10 ~ rowSums(across(After.Yr_1:After.Yr_10),""))))))))))),

        ORi= OR.i(O_a,Nexp_a),#Observed,a/Expected,a; HSM (9A.1-5)

        Var.=VAR.(adj,Nexp_b,w),#(9A.1-9)

        OR=OR.(ORi,Var.,Nexp_a), #(9A.1-8)

        SE=SE.(ORi), #correction from OR to ORi

        VarOR=VarOR.(ORi,O_a,Var.,Nexp_a),#9A.1-11

        Std.Err_OR=Std.Err.OR.(VarOR),#9A.1-12

        Std.Err.SE=(Std.Err_OR*100),#9A.1-13

        test_statistic=abs(SE/Std.Err.SE), #Step 14 (a) & (b)
        Significance=Significance.test(test_statistic)) %>% dplyr::select(Site_No.:C,CMF,O_b:Significance)|> mutate_if(is.numeric, round, 3) -> data

  }

if(group==TRUE){

  #pooling or grouping functions (grp)
  OR.grp.i = function(O_a,Nexp_a) sum(O_a)/sum(Nexp_a) #(9A.1-7)

  VAR.grp=function(adj,Nexp_b,w) sum(adj^(2)*Nexp_b*(1-w)) #(9A.1-9) #edited. summation over entire function

  OR.grp=function(OR.grp.i,VAR.grp,Nexp_a) sum(OR.grp.i)/(1+sum(VAR.grp)/sum(Nexp_a)^2) # (9A.1-8) #edited switched sum(Nexp_a^2) to sum(Nexp_a)^2

  SE.grp=function(OR.grp) 100*(1-sum(OR.grp)) #(9A.1-10)

  VarOR.grp=function(OR.grp.i,O_a,VAR.grp,Nexp_a) sum(OR.grp.i)^(2)*(1/sum(O_a)+sum(VAR.grp)/sum(Nexp_a)^(2)/(1+sum(VAR.grp)/sum(Nexp_a)^2)) #(9A.1-11)

  Std.Err.OR.grp=function(VarOR.grp) sum(VarOR.grp)^(1/2) #(9A.1-12)

  Std.Err.SE.grp=function(Std.Err.OR.grp) 100*sum(Std.Err.OR.grp) #(9A.1-13)

  test_statistic.grp=function(SE.grp,Std.Err.SE.grp)  abs(sum(SE.grp)/sum(Std.Err.SE.grp))  #Step 14 (a)


    data %>%
      group_by(Base_Past) |>
      dplyr::summarise(No.Sites=n(),#count sites
                       Yrs_Before=sum(Yrs_Before), #edit moved
                       Yrs_After=sum(Yrs_After), #edit moved
                       O_b.grp.=sum(O_b),
                       O_a.grp.=sum(O_a),
                       P_b=sum(P_b),
                       Nexp_b.grp.=sum(Nexp_b), #(9A.1-1)
                       P_a=sum(P_a), #(9A.1-1)
                       Nexp_a.grp.=sum(Nexp_a), #(9A.1-4)
                       Avg_AADTmaj_b=mean(Avg_AADTmaj_b),
                       Avg_AADTmaj_a=mean(Avg_AADTmaj_a),
                       OR.grp.i.=OR.grp.i(O_a,Nexp_a), #(9A.1-7)
                       VAR.grp.=VAR.grp(adj,Nexp_b,w), #(9A.1-9)
                       OR.grp.=OR.grp(OR.grp.i.,VAR.grp.,Nexp_a), #(9A.1-8)
                       VarOR.grp.=VarOR.grp(OR.grp.i.,O_a,VAR.grp.,Nexp_a), #(9A.1-11)
                       SE.grp.=SE.grp(OR.grp.), #(9A.1-10)
                       Std.Err.OR.grp.=Std.Err.OR.grp(VarOR.grp.), #(9A.1-12)
                       Std.Err.SE.grp.=Std.Err.SE.grp(Std.Err.OR.grp.), #(9A.1-13)
                       test_statistic.grp.=test_statistic.grp(SE.grp.,Std.Err.SE.grp.), #Step 14 (a)
                       Significance.test.grp.=Significance.test(test_statistic.grp.)) |>
                         mutate_if(is.numeric, round, 3) |>
                         rename(
                         "Total Sites"="No.Sites",
                         "Past Base Condition"="Base_Past",
                         "Years in the Before Period"="Yrs_Before",
                         "Average AADT in the Before Period"="Avg_AADTmaj_b",
                         "Total Predicted Crashes in the Before Period"="P_b",
                         "Total Observed Crashes in the Before Periods"="O_b.grp.",
                         "Expected Crashes in the Before Period (9A.1-2)"="Nexp_b.grp.",
                         "Average AADT in the After Period"="Avg_AADTmaj_a",
                         "Total Predicted Crashes in the After Period"="P_a",
                         "Total Years in the After Period"="Yrs_After",
                         "Total Expected Crashes in the After Period (9A.1-4)"="Nexp_a.grp.",
                         "Total Observed Crashes in the After Period"="O_a.grp.",
                         "Odds Ratio (9A.1-5)"="OR.grp.i.",
                         "Variance (9A.1-9)"="VAR.grp.",
                         "Unbiased Odds Ratio (9A.1-8)"="OR.grp.",
                         "Variance Odds Ratio (9A.1-11)"="VarOR.grp.",
                         "Standard Error Odds Ratio (9A.1-12)"="Std.Err.OR.grp.",
                         "Safety Effectiveness (9A.1-10)"="SE.grp.",
                         "Standard Error of Safety Effectiveness (9A.1-13)"="Std.Err.SE.grp.",
                         "Test Statistic (Step 14)"="test_statistic.grp.",
                         "Significance Level (Step 14)"="Significance.test.grp.")

  }

else{
    data |> rename(
      "Past Base Condition"="Base_Past",
             "Current Base Condition"="Base_Current",
             "Years in the Before Period"="Yrs_Before",
             "Calibration Factor"="C",
             "Crash Modification Factor"="CMF",
             "Segment Length"="L",
             "Average AADT in the Before Period"="Avg_AADTmaj_b",
             "Predicted Crashes in the Before Period"="P_b",
             "Observed Crashes in the Before Periods"="O_b",
             "Overdisperion Factor"="k",
             "Weight (9A.1-2)"="w",
             "Expected Crashes in the Before Period (9A.1-2)"="Nexp_b",
             "Average AADT in the After Period"="Avg_AADTmaj_a",
             "Predicted Crashes in the After Period"="P_a",
             "Years in the After Period"="Yrs_After",
             "Adjustment Factor (9A.1-3)"="adj",
             "Expected Crashes in the After Period (9A.1-4)"="Nexp_a",
             "Observed Crashes in the After Period"="O_a",
             "Odds Ratio (9A.1-5)"="ORi",
             "Variance (9A.1-9)"="Var.",
             "Unbiased Odds Ratio (9A.1-8)"="OR",
             "Variance Odds Ratio (9A.1-11)"="VarOR",
             "Standard Error Odds Ratio (9A.1-12)"="Std.Err_OR",
             "Safety Effectiveness (9A.1-10)"="SE",
             "Standard Error of Safety Effectiveness (9A.1-13)"="Std.Err.SE",
             "Test Statistic (Step 14)"="test_statistic",
             "Significance Level (Step 14)"="Significance")
  }

}

#' spf
#'
#' @param AADTMAJ value for major volume AADT.
#' @param AADTMIN value for minor volume AADT.
#' @param L Length specified in miles. Applies only to segments.
#' @param Base_Condition Corresponding base condition or the title of the SPF.
#' @param Provide_Overdispersion_Factor TRUE or FALSE Parameter. Specify TRUE for overdispersion parameter k.
#' @param Segment TRUE or FALSE parameter. Specify TRUE for segment SPF. Depends on Major AADT and Length.
#'
#' @return
#' @export
#'
#' @examples
#'
#' spf(AADTMAJ=100,AADTMIN=NULL,L=100,Base_Condition= "HSM-RUR2U-KABCO",Provide_Overdispersion_Factor=FALSE,Segment=TRUE)
#'
#' spf(AADTMAJ=100,AADTMIN=100,L=NULL,Base_Condition= "HSM-RUR2-3ST-KABCO",Provide_Overdispersion_Factor=FALSE,Segment=FALSE)
#'
spf=function(AADTMAJ,AADTMIN,L,Base_Condition,Provide_Overdispersion_Factor,Segment) {

  #REF: Harwood, D. W., F. M. Council, E. Hauer, W. E. Hughes, and A. Vogt. Prediction of the Expected Safety
  #Performance of Rural Two-Lane Highways, Report No. FHWA-RD-99-207. Federal Highway Administration,
  #U.S. Department of Transportation, Washington, DC, December 2000.
  #NOTE: SPFs are only applicable to varied range of Traffic Volumes.
  #NOTE: USE AADTMIN=NULL for segments

  if (Provide_Overdispersion_Factor==TRUE & Segment==TRUE){ #Suggest changing Segment==TRUE to is.numeric(L)==TRUE
    ##Begin Coding Segments
    #Rural
    if(Base_Condition=="HSM-RUR2U-KABCO") {k=0.236/L}
    k
  }

  else if (Provide_Overdispersion_Factor==TRUE & Segment==FALSE){    ##Begin Coding Intersections
    #Rural
    if(Base_Condition=="HSM-RUR2-3ST-KABCO") {k=0.54} #(10-8)
    if(Base_Condition=="HSM-RUR2-4ST-KABCO") {k=0.24} #(10-9)
    if(Base_Condition=="HSM-RUR2-4SG-KABCO") {k=0.11} #(10-10)
    if(Base_Condition=="HSM-RUR4-3ST-KABCO") {k=0.460} #(table 11-7)
    if(Base_Condition=="HSM-RUR4-4ST-KABCO") {k=0.494} #(table 11-7)
    if(Base_Condition=="HSM-RUR4-4SG-KABCO") {k=0.277} #(table 11-8)

    if(Base_Condition=="HSM-RUR4-3ST-KABC") {k=0.569} #(table 11-7)
    if(Base_Condition=="HSM-RUR4-4ST-KABC") {k=0.742} #(table 11-7)
    if(Base_Condition=="HSM-RUR4-4SG-KABC") {k=0.218} #(table 11-8)

    if(Base_Condition=="HSM-RUR4-3ST-KAB") {k=0.566} #(table 11-7b)
    if(Base_Condition=="HSM-RUR4-4ST-KAB") {k=0.655} #(table 11-7b)
    if(Base_Condition=="HSM-RUR4-4SG-KAB") {k=0.566} #(table 11-8b)


    #Urban
    if(Base_Condition=="HSM-URB-3ST-MV-KABCO") {k=0.80} #(table 12-10)
    if(Base_Condition=="HSM-URB-3SG-MV-KABCO") {k=0.33} #(table 12-10)
    if(Base_Condition=="HSM-URB-4ST-MV-KABCO") {k=0.40} #(table 12-10)
    if(Base_Condition=="HSM-URB-4SG-MV-KABCO") {k=0.39} #(table 12-10)
    if(Base_Condition=="HSM-URB-3ST-SV-KABCO") {k=1.14} #(table 12-12)
    if(Base_Condition=="HSM-URB-3SG-SV-KABCO") {k=0.36} #(table 12-12)
    if(Base_Condition=="HSM-URB-4ST-SV-KABCO") {k=0.65} #(table 12-12)
    if(Base_Condition=="HSM-URB-4SG-SV-KABCO") {k=0.36} #(table 12-12)

    if(Base_Condition=="HSM-URB-3ST-MV-KABC") {k=0.69} #(table 12-10)
    if(Base_Condition=="HSM-URB-3SG-MV-KABC") {k=0.30} #(table 12-10)
    if(Base_Condition=="HSM-URB-4ST-MV-KABC") {k=0.48} #(table 12-10)
    if(Base_Condition=="HSM-URB-4SG-MV-KABC") {k=0.33} #(table 12-10)
    if(Base_Condition=="HSM-URB-3SG-SV-KABC") {k=0.24} #(table 12-10)
    if(Base_Condition=="HSM-URB-4SG-SV-KABC") {k=0.09} #(table 12-10)

    k
  }

  else if(Segment==FALSE & Provide_Overdispersion_Factor==FALSE ) {#Begin Coding intersection SPF here

    if(Base_Condition=="HSM-RUR2-3ST-KABCO") {a=-9.86;b=0.79;c=0.49}
    if(Base_Condition=="HSM-RUR2-4ST-KABCO") {a=-8.56;b=0.60;c=0.61}
    if(Base_Condition=="HSM-RUR2-4SG-KABCO") {a=-5.13;b=0.60;c=0.20}
    if(Base_Condition=="HSM-RUR4-3ST-KABCO") {a=-12.526;b=1.204;c=0.236}
    if(Base_Condition=="HSM-RUR4-4ST-KABCO") {a=-10.008;b=0.848;c=0.448}
    if(Base_Condition=="HSM-RUR4-4SG-KABCO") {a=-7.182;b=0.722;c=0.337}

    if(Base_Condition=="HSM-RUR4-3ST-KABC") {a=-12.664;b=1.107;c=0.272}
    if(Base_Condition=="HSM-RUR4-4ST-KABC") {a=-1.554;b=0.888;c=0.525}
    if(Base_Condition=="HSM-RUR4-4SG-KABC") {a=-6.393;b=0.638;c=0.232}

    if(Base_Condition=="HSM-RUR4-3ST-KAB") {a=-11.989;b=1.013;c=0.228}
    if(Base_Condition=="HSM-RUR4-4ST-KAB") {a=-10.734;b=0.828;c=0.412}
    if(Base_Condition=="HSM-RUR4-4SG-KAB") {a=-12.011;b=1;c=1.279} #verify b=1

    #Urban
    if(Base_Condition=="HSM-URB-3ST-MV-KABCO") {a=-13.36;b=1.11;c=0.41}
    if(Base_Condition=="HSM-URB-3SG-MV-KABCO") {a=-12.13;b=1.11;c=0.26}
    if(Base_Condition=="HSM-URB-4ST-MV-KABCO") {a=-8.90;b=0.82;c=0.25}
    if(Base_Condition=="HSM-URB-4SG-MV-KABCO") {a=-10.99;b=1.07;c=0.23}
    if(Base_Condition=="HSM-URB-3ST-SV-KABCO") {a=-6.81;b=0.16;c=0.51}
    if(Base_Condition=="HSM-URB-3SG-SV-KABCO") {a=-9.02;b=0.42;c=0.40}
    if(Base_Condition=="HSM-URB-4ST-SV-KABCO") {a=-5.33;b=0.33;c=0.12}
    if(Base_Condition=="HSM-URB-4SG-SV-KABCO") {a=-10.21;b=0.68;c=0.27}

    if(Base_Condition=="HSM-URB-3ST-MV-KABC") {a=-14.01;b=1.16;c=0.30}
    if(Base_Condition=="HSM-URB-3SG-MV-KABC") {a=-11.58;b=1.02;c=0.17}
    if(Base_Condition=="HSM-URB-4ST-MV-KABC") {a=-11.13;b=0.93;c=0.28}
    if(Base_Condition=="HSM-URB-4SG-MV-KABC") {a=-13.14;b=1.18;c=0.22}
    if(Base_Condition=="HSM-URB-3SG-SV-KABC") {a=-9.75;b=0.27;c=0.51} #(table 12-10)
    if(Base_Condition=="HSM-URB-4SG-SV-KABC") {a=-9.25;b=0.43;c=0.29} #(table 12-10)

    #exp(a+b*log(AADTMAJ)+c*log(AADTMIN))
    exp(a)*((AADTMAJ)^(b)*(AADTMIN)^(c))

  }

  else if(Segment==TRUE & Provide_Overdispersion_Factor==FALSE){ #Begin coding segment SPF here
    if(Base_Condition=="HSM-RUR2U-KABCO") {a=-0.312} #HSM Example 9.10
    AADTMAJ*L*365*10^(-6)*exp(a)
  }

}

