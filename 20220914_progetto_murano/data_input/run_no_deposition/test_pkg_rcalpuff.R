library(rcalpuff)

mapview_calpost_grd('./rank(0)_cd_2952hr_conc_cd.grd', 
                    name_of_map_layer = 'Cd [ngm-3]<br>AVG period',
                    string_filename = 'cd_avg_periodo',
                    export = TRUE)

mapview_calpost_grd('./rank(60)_cd_1hr_conc_cd.grd', 
                    name_of_map_layer = 'Cd [ngm-3]<br>P098 period',
                    string_filename = 'cd_p098_periodo',
                    export = TRUE)

mapview_calpost_grd('./rank(1)_cd_24hr_conc_cd.grd', 
                    name_of_map_layer = 'Cd [ngm-3]<br>P100 period',
                    string_filename = 'cd_p100_periodo',
                    export = TRUE)
