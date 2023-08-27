# ************************************************************************
# KOF Economic Forecasts -----
# URL: https://kof.ethz.ch/en/forecasts-and-indicators/forecasts/kof-economic-forecast.html
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger, 2023 (matthias.spichiger@bluewin.ch)
# ************************************************************************

## Load packages ----
library(tidyverse)
library(tsbox)
library(kofdata)
library(ggtext)
library(readxl)

## Access the data ----

kof_ec_fcst_url <- "https://datenservice.kof.ethz.ch/api/v1/public/sets/vja_public?mime=xlsx"
download.file(url = kof_ec_fcst_url, destfile = "KOF_Economic_Forecasts/KOF_Economic_Forecasts.xlsx", method = "curl")
kof_ec_fcst <- readxl::read_excel(path = "KOF_Economic_Forecasts/KOF_Economic_Forecasts.xlsx")

names(kof_ec_fcst)
# date"                                                             
# ch.kof.vja.bau.mfh_nom                                         
# ch.kof.vja.bau.mfh_real                                        
# ch.kof.vja.fx.chf_reer                                          
# ch.kof.vja.koma.consg                                        
# ch.kof.vja.koma.consp                                          
# ch.kof.vja.koma.constot                                         
# ch.kof.vja.koma.domdem                                         
# ch.kof.vja.koma.domdem2                                        
# ch.kof.vja.koma.domdemoi                                       
# ch.kof.vja.koma.ebsc                                          
# ch.kof.vja.koma.ebsp                                           
# ch.kof.vja.koma.et                                            
# ch.kof.vja.koma.exc1                                           
# ch.kof.vja.koma.exc2                                           
# ch.kof.vja.koma.excm                                           
# ch.kof.vja.koma.exs                                            
# ch.kof.vja.koma.exst                                           
# ch.kof.vja.koma.ext                                           
# ch.kof.vja.koma.extot1                                       
# ch.kof.vja.koma.extot2                                         
# ch.kof.vja.koma.gdp
# ch.kof.vja.koma.gdpos
# ch.kof.vja.koma.icinfrar
# ch.kof.vja.koma.icnstr
# ch.kof.vja.koma.ictraf               
# ch.kof.vja.koma.ifix                                      
# ch.kof.vja.koma.iinv1                                         
# ch.kof.vja.koma.imc1                                             
# ch.kof.vja.koma.imc2                                            
# ch.kof.vja.koma.ime                                             
# ch.kof.vja.koma.ims                                              
# ch.kof.vja.koma.imst                                            
# ch.kof.vja.koma.imt                                             
# ch.kof.vja.koma.imtot1                                          
# ch.kof.vja.koma.imtot2                                          
# ch.kof.vja.koma.lfpot                                           
# ch.kof.vja.koma.lprodh                                          
# ch.kof.vja.koma.lprodos                                        
# ch.kof.vja.koma.ltot                                            
# ch.kof.vja.koma.ltoth                                           
# ch.kof.vja.koma.ltotv                                           
# ch.kof.vja.koma.nconsg                                          
# ch.kof.vja.koma.nconsp                                          
#ch.kof.vja.koma.nconstot                                        
# ch.kof.vja.koma.ndomdem                                         
# ch.kof.vja.koma.ndomdem2                                         
# ch.kof.vja.koma.ndomdemoi                                       
# ch.kof.vja.koma.nexc1                                          
# ch.kof.vja.koma.nexc2                                            
# ch.kof.vja.koma.nexcm                                           
# ch.kof.vja.koma.nexs                                            
# ch.kof.vja.koma.nexst                                            
# ch.kof.vja.koma.next                                            
# ch.kof.vja.koma.nextot1                                        
# ch.kof.vja.koma.nextot2                                         
# ch.kof.vja.koma.ngdp                                           
# ch.kof.vja.koma.ngdpos                                         
# ch.kof.vja.koma.nicnstr                                          
# ch.kof.vja.koma.nifix                                            
# ch.kof.vja.koma.niinv1                                          
# ch.kof.vja.koma.niival                                           
# ch.kof.vja.koma.nimc1                                           
# ch.kof.vja.koma.nimc2                                           
# ch.kof.vja.koma.nime                                            
# ch.kof.vja.koma.nims                                            
# ch.kof.vja.koma.nimst                                           
# ch.kof.vja.koma.nimt                                            
# ch.kof.vja.koma.nimtot1                                          
# ch.kof.vja.koma.nimtot2                                          
# ch.kof.vja.koma.ntotdem                                          
# ch.kof.vja.koma.ntotdem2                                        
# ch.kof.vja.koma.pconsg                                           
# ch.kof.vja.koma.pconsp                                           
# ch.kof.vja.koma.pconstot                                         
# ch.kof.vja.koma.pdomdem                                          
# ch.kof.vja.koma.pdomdem2                                         
# ch.kof.vja.koma.pdomdemoi                                        
# ch.kof.vja.koma.pexc1                                            
# ch.kof.vja.koma.pexc2                                            
# ch.kof.vja.koma.pexcm                                            
# ch.kof.vja.koma.pexs                                             
# ch.kof.vja.koma.pexst                                           
# ch.kof.vja.koma.pext                                            
# ch.kof.vja.koma.pextot1                                         
# ch.kof.vja.koma.pextot2                                         
# ch.kof.vja.koma.pgdp                                            
# ch.kof.vja.koma.pgdpos                                          
# ch.kof.vja.koma.picnstr                                          
# ch.kof.vja.koma.pifix                                            
# ch.kof.vja.koma.pimc1                                            
# ch.kof.vja.koma.pimc2                                           
# ch.kof.vja.koma.pime                                            
# ch.kof.vja.koma.pims                                          
# ch.kof.vja.koma.pimst                                           
# ch.kof.vja.koma.pimt                                            
# ch.kof.vja.koma.pimtot1                                         
# ch.kof.vja.koma.pimtot2                                         
# ch.kof.vja.koma.pop                                             
# ch.kof.vja.koma.ptotdem                                        
# ch.kof.vja.koma.ptotdem2                                       
# ch.kof.vja.koma.savpfund                                        
# ch.kof.vja.koma.totdem                                         
# ch.kof.vja.koma.totdem2                                         
# ch.kof.vja.koma.urilo                                          
# ch.kof.vja.koma.uroff                                           
# ch.kof.vja.koma.vasport                                        
# ch.kof.vja.koma.wage                                             
# ch.kof.vja.koma.winc                                            
# [110] "ch.kof.vja.koma.ydispb                                           
# [111] "ch.kof.vja.konsum.p31fc10_unterrichtswesen                       
# [112] "ch.kof.vja.konsum.p31fc11_restaurants_und_hotels                 
# [113] "ch.kof.vja.konsum.p31fc12_sonstige_waren_und_dienstleistungen    
# [114] "ch.kof.vja.konsum.p31fc1_p31fc2_nahrungsmittel_und_getraenke     
# [115] "ch.kof.vja.konsum.p31fc3_bekleidung_und_schuhe                  
# [116] "ch.kof.vja.konsum.p31fc4_wohnung_wasser_strom_gas               
# [117] "ch.kof.vja.konsum.p31fc5_moebel_innenausstattung_haushaltsgeraete
# ch.kof.vja.konsum.p31fc6_gesundheitspflege                      
# ch.kof.vja.konsum.p31fc7_verkehr                               
# ch.kof.vja.konsum.p31fc8_nachrichtenuebermittlung             
# ch.kof.vja.konsum.p31fc9_freizeit_und_kultur                  
# ch.kof.vja.lik.cpi                                              
# ch.kof.vja.lik.cpi_dom                                          
# ch.kof.vja.lik.cpi_domg                                         
# ch.kof.vja.lik.cpi_doms                                         
# ch.kof.vja.lik.cpi_domspri                                      
# ch.kof.vja.lik.cpi_domspub                                      
# ch.kof.vja.lik.cpi_domsren                                   
# ch.kof.vja.lik.cpi_im                                          
# ch.kof.vja.lik.lrate                                            
# ch.kof.vja.lik.srate                                          
# ch.kof.vja.saldi.saldo_arbeitseinkommen                         
# ch.kof.vja.saldi.saldo_kapitaleinkommen                       
# ch.kof.vja.saldi.sekundaereinkommen 

# END