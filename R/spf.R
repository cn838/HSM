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

