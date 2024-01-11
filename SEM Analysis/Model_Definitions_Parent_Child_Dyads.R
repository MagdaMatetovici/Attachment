#Model Definitions Attachment and Problem Behavior
#Author: Magda Matetovici

#-------------------------------------------------------------------------------
#Models Internalising
#-------------------------------------------------------------------------------

model_internalising_secure <- '
                     secure_attachment =~ C_ari6_recoded + C_ari11_recoded + C_ari17_recoded + C_ari30_recoded + C_ari37_recoded + 
                              C_ari42_recoded + C_ari46_recoded + C_ari47_recoded + C_ari50_recoded + C_ari54_recoded + 
                                C_ari59_recoded + C_ari61_recoded + C_ari64_recoded

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ secure_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'
model_internalising_secure_equal <- '
                  secure_attachment =~ C_ari6_recoded + C_ari11_recoded + C_ari17_recoded + C_ari30_recoded + C_ari37_recoded + 
                              C_ari42_recoded + C_ari46_recoded + C_ari47_recoded + C_ari50_recoded + C_ari54_recoded + 
                                C_ari59_recoded + C_ari61_recoded + C_ari64_recoded
                    
                 

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ c(beta1, beta1)*secure_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'

model_internalising_avoidant <- '
                     avoidant_attachment =~ C_ari3_recoded + C_ari14_recoded + C_ari23_recoded + C_ari25_recoded + C_ari26_recoded + 
                                C_ari31_recoded + C_ari33_recoded + C_ari34_recoded + C_ari36_recoded + C_ari57_recoded + 
                                  C_ari66_recoded

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ avoidant_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'
model_internalising_avoidant_equal <- '
                     avoidant_attachment =~ C_ari3_recoded + C_ari14_recoded + C_ari23_recoded + C_ari25_recoded + C_ari26_recoded + 
                                C_ari31_recoded + C_ari33_recoded + C_ari34_recoded + C_ari36_recoded + C_ari57_recoded + 
                                  C_ari66_recoded

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ c(beta1, beta1)*avoidant_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'

model_internalising_ambivalent <- '
                  ambivalent_attachment =~ C_ari12_recoded + C_ari15_recoded + C_ari16_recoded + C_ari19_recoded + C_ari38_recoded + 
                                  C_ari39_recoded + C_ari51_recoded + C_ari53_recoded + C_ari56_recoded + C_ari58_recoded + 
                                    C_ari63_recoded
                     
                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ ambivalent_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'
model_internalising_ambivalent_equal <- '
                  ambivalent_attachment =~ C_ari12_recoded + C_ari15_recoded + C_ari16_recoded + C_ari19_recoded + C_ari38_recoded + 
                                  C_ari39_recoded + C_ari51_recoded + C_ari53_recoded + C_ari56_recoded + C_ari58_recoded + 
                                    C_ari63_recoded
                                

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ c(beta1, beta1)*ambivalent_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'

model_internalising_disorganised <- '
                 disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded 

                     
                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                  
                                      
'
model_internalising_disorganised_removed <- '
                 disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded

                     
                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                  
                                      
'
#+ C_SDQ8.2_13_recoded 
model_internalising_disorganised_equal <- '
                  disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded
                    

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ c(beta1, beta1)*disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'

model_internalising_disorganised_equal_removed <- '
                  disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded
                    

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ c(beta1, beta1)*disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
                  
'
model_internalising_disorganised_children <- '
                 disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded + C_SDQ8.2_13_recoded

                     
                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                  
'
model_internalising_disorganised_equal_children <- '
                  disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded + C_SDQ8.2_13_recoded
                    

                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   internalising ~ c(beta1, beta1)*disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  internalising ~~ internalising
                                      
'
#Models Externalising

model_externalising_secure <- '
                     secure_attachment =~ C_ari6_recoded + C_ari11_recoded + C_ari17_recoded + C_ari30_recoded + C_ari37_recoded + 
                              C_ari42_recoded + C_ari46_recoded + C_ari47_recoded + C_ari50_recoded + C_ari54_recoded + 
                                C_ari59_recoded + C_ari61_recoded + C_ari64_recoded

                    externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ secure_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'
model_externalising_secure_equal <- '
                  secure_attachment =~ C_ari6_recoded + C_ari11_recoded + C_ari17_recoded + C_ari30_recoded + C_ari37_recoded + 
                              C_ari42_recoded + C_ari46_recoded + C_ari47_recoded + C_ari50_recoded + C_ari54_recoded + 
                                C_ari59_recoded + C_ari61_recoded + C_ari64_recoded
                    
                 
                   externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                
                                    
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ c(beta1, beta1)*secure_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'

model_externalising_avoidant <- '
                     avoidant_attachment =~ C_ari3_recoded + C_ari14_recoded + C_ari23_recoded + C_ari25_recoded + C_ari26_recoded + 
                                C_ari31_recoded + C_ari33_recoded + C_ari34_recoded + C_ari36_recoded + C_ari57_recoded + 
                                  C_ari66_recoded

                   externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ avoidant_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'
model_externalising_avoidant_equal <- '
                     avoidant_attachment =~ C_ari3_recoded + C_ari14_recoded + C_ari23_recoded + C_ari25_recoded + C_ari26_recoded + 
                                C_ari31_recoded + C_ari33_recoded + C_ari34_recoded + C_ari36_recoded + C_ari57_recoded + 
                                  C_ari66_recoded

                   externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ c(beta1, beta1)*avoidant_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'

model_externalising_ambivalent <- '
                  ambivalent_attachment =~ C_ari12_recoded + C_ari15_recoded + C_ari16_recoded + C_ari19_recoded + C_ari38_recoded + 
                                  C_ari39_recoded + C_ari51_recoded + C_ari53_recoded + C_ari56_recoded + C_ari58_recoded + 
                                    C_ari63_recoded
                     
                   externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ ambivalent_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'
model_externalising_ambivalent_equal <- '
                  ambivalent_attachment =~ C_ari12_recoded + C_ari15_recoded + C_ari16_recoded + C_ari19_recoded + C_ari38_recoded + 
                                  C_ari39_recoded + C_ari51_recoded + C_ari53_recoded + C_ari56_recoded + C_ari58_recoded + 
                                    C_ari63_recoded
                                

                   externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ c(beta1, beta1)*ambivalent_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'

model_externalising_disorganised <- '
                 disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded

                     
                   externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'
model_externalising_disorganised_equal <- '
                  disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded

                                
                    

                   externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
                                    
                  #Specify the regression model with labelled parameters
                  
                   externalising ~ c(beta1, beta1)*disorganised_attachment 
                   
                  #Speficy the residual variance
                  
                  externalising ~~ externalising
                                      
'
