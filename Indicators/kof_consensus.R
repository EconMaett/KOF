# KOF Consensus forecast -----

# kof_consensus_forecast        KOF Consensus Forecast
# kof_consensus_forecast_count  KOF Consensus Forecast Count
# kof_consensus_forecast_dates  KOF Consensus Forecast Dates
# kof_consensus_forecast_max    KOF Consensus Forecast Maximum
# kof_consensus_forecast_mean   KOF Consensus Forecast Mean
# kof_consensus_forecast_median KOF Consensus Forecast Median
# kof_consensus_forecast_min    KOF Consensus Forecast Minimum
# kof_consensus_forecast_prob   KOF Consensus Forecast Probabilites
# kof_consensus_forecast_stdev  KOF Consensus Forecast Standard Deviation

list_keys_in_collection(collectionname = "kof_consensus_forecast")

## Prices ----
# ch.kof.consensus.q_qn_prices_[cy,ny,5y].[count,max,mean,median,mean,stdev]

# ch.kof.consensus.q_qn_prices_prob_[cy,ny,5y]_[1-17].[count,max,mean,median,mean,stdev]


## Financial conditions ----

### 10-year bond yields ----
# ch.kof.consensus.q_qn_10yyield_[3m,12m].[count,max,mean,median,min,stdev]

### 3-month interest rates ----
# ch.kof.consensus.q_qn_3minterest_[3m,12m].[count,max,mean,median,mean,stdev]

## Swiss Performance Index ----
# ch.kof.consensus.q_qn_spi_[3m,12m].[count,max,mean,median,mean,stdev]


## Unemployment rate ----
# ch.kof.consensus.q_qn_unemp_[cy,ny,5y].[count,max,mean,median,mean,stdev]

# ch.kof.consensus.q_qn_unemp_prob_[cy,ny,5y]_[1-17].[count,max,mean,median,mean,stdev]


## Exchange rates ----

## EUR-CHF ----
# ch.kof.consensus.q_qn_eurchf_[3m,12m].[count,max,mean,median,mean,stdev]

### USD - CHF ----
# ch.kof.consensus.q_qn_usdchf_[3m,12m].[count,max,mean,median,mean,stdev]


## GDP ----

### Real GDP ----
# ch.kof.consensus.q_qn_realgdp_[cy,ny,5y].[count,max,mean,median,mean,stdev]

# ch.kof.consensus.q_qn_realgdp_prob_[cy,ny,5y]_[1-17].[count,max,mean,median,mean,stdev]

### Exports ----
# ch.kof.consensus.q_qn_exports_[cy,ny].[count,max,mean,median,mean,stdev]

### Investment ----
# ch.kof.consensus.q_qn_invest_cy.[count,max,mean,median,mean,stdev]

#### Construction investment ----
# ch.kof.consensus.q_qn_investbau_[cy,ny].[count,max,mean,median,mean,stdev]

#### Equipment investment ----
# ch.kof.consensus.q_qn_investequ_[cy,ny].[count,max,mean,median,mean,stdev]


## Meta information ----

### KOF media release day ----
# ch.kof.consensus.media_release_day

### KOF questionnaire ----
# ch.kof.consensus.questionnaire_end_day     
# ch.kof.consensus.questionnaire_start_day