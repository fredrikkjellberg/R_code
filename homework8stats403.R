# HOMEWORK 8

########################################################################################################################################
# 1.  Write out a flowchart demonstrating the sequence of model comparisons that you will make in the process of arriving at a model for 
#     shame as a function of non-manipulable factors in the experiment (sex and trait). Do this on a clean sheet of paper, and either 
#     scan it or take a legible photograph of it.
########################################################################################################################################

1[  Shame~1  ]
      |
      |
2[ Shame~sex ] ------------->3[ Shame~trait]

      |
      |
4[ Shame~sex:trait ] 
# test interaction term
      |
      |
5[ Shame~sex+trait] 
# test each factor and interaction effect



########################################################################################################################################
#  2. Describe a single model that involves the manipulable factor (condition) in addition to non-manipulable factors that you will compare 
#     the model emerging from step A in order to test whether the manipulable factor had a causal effect on the outcome. It's okay
#     for this model to depend on the result of step A, but then you should describe precisely how will depend on the result.
########################################################################################################################################

attach(selfobj)
tapply(shame, sex, mean)

shame (outcome variable)
sex (non-experimental factor) = non-manipulable factors
trait (non-experimental factor) = non-manipulable factors
condition (experimental factor, and the “focal factor” of the study)  = manipulable factor 

anova(lm(model without experimentalfactors),lm(models inclduing chosen experimental factors))
# this test will give you more power than just doing anova on one regressions

# In neither case does the non-experimental factor we add seem to improve
# the model in a statistically significant way. Therefore if we follow the
# procedure on the flowchart, we would include trait factor, and
# run a one-way ANOVA including our experimental and trait factor:

anova(lm(shame~condition, data=selfobj),lm(shame~condition+trait, data=selfobj))

Model 1: shame ~ condition
Model 2: shame ~ condition + trait
Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
1     80 490.89                              
2     79 454.96  1     35.93 6.2389 0.01457 *
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



########################################################################################################################################
#  3. Follow your flowchart to determine your “best” model in terms of the nonexperimental factors. Show us the R code and output 
#     for just one of the comparisons, so that we can see what you did. Report your results (F-statistic and corresponding p-value) 
#     for that comparison, and what your decision was (e.g. which branch you followed) for that one comparison.
########################################################################################################################################

# sex is not signigicant
# trait is not really significant

anova(lm(shame~sex, data=selfobj))
# F-value = 0.2486
# P-value = 0.6195
#  not very significant, so move over to the left

anova(lm(shame~trait, data=selfobj))
# F-value = 5.7492
# P-value = 0.01883 yes! 
#  significant at a 5% level, so lets go ahead and move down
#  the flowchart an test the interaction level

anova(lm(shame~sex:trait, data=selfobj))
# F-value = 2.5493
# P-value = 0.06176 yes! 
#  it is significant by itself but we cant really tell before we add both to the model

anova(lm(shame~sex*trait, data=selfobj))
#           Df Sum Sq Mean Sq F value  Pr(>F)
# sex        1   1.66   1.660  0.2653 0.60796 
# trait      1  35.58  35.583  5.6870 0.01952
# sex:trait  1  10.61  10.609  1.6956 0.19670 
# adding only the significant level does not make the interaciton effect significant
#  we therefore go back and sue only trait in our model!

final model (with nonexperimental factors):
  
  anova(lm(shame~trait, data=selfobj))

final model (with experimental and nonexperimental factors):
anova(lm(shame~condition+trait, data=selfobj))

Analysis of Variance Table

Response: shame
Df Sum Sq Mean Sq F value   Pr(>F)   
condition  1  45.01  45.007  7.8150 0.006502 **
  trait      1  35.93  35.930  6.2389 0.014575 * 
  Residuals 79 454.96   5.759                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


########################################################################################################################################
#  4.  Test for the presence of a treatment effect using the comparison that you pre-specified in part B. (To present your result: tell us the 
#                                                                                                   main conclusion first; then show us 
#                                                                                                    the code that you used to arrive at it.)
########################################################################################################################################

the best way to 