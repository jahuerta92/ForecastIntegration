# Forecast Integration
Repository for code in article:  "A short-term solar radiation forecasting system for the Iberian Peninsula. Part 2: Model blending approaches based on machine learning"

# How to run
Using R the code can be run as an script. Data in .rds format is necessary. Data must be a list of stations and each station data-set must contain these columns:

"date": Date for t. (minutes)
"date_expected": Date for t+h. (minutes)
"horizon": Predictive horizon h. (minutes)
"IRRADIANCE_MODEL": Forecast made by MODEL (any physical model) for IRRADIANCE (ghi, dni, ghiKt or dniKt) measure.
"IRRADIANCE_present": Current value for IRRADIANCE (ghi, dni, ghiKt or dniKt) measure.   
"IRRADIANCE_expected": Desired value for IRRADIANCE (ghi, dni, ghiKt or dniKt) measure.          
"zenit": Zenithal angle.
"wTypeF": Weather type.

There are three scripts:
-ModeloIntegrador.R (General model)
-ModeloIntegradorMultiHorizonte.R (Horizon Model)
-ModeloIntegradorTipoDeTiempo.R (Time type Model)

Regional predictions can be built ad-hoc with this code and an extended dataset with fields such as "STATION_IRRADIANCE_MODEL".

What columns are used (or ignored) can be chosen for each Script within the code.

