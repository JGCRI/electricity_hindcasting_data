$title GAMS code to read-in heat-rate data

*       Read in heatrate data
parameter       heatrate(oc,fuel,utility,plant,generator,t) /
$ondelim
$offlisting
$include data/heatrate.csv
$onlisting
$offdelim
/;

*       Read in summer capacity data
parameter       summer_cap(oc,fuel,utility,plant,generator,t) /
$ondelim
$offlisting
$include data/summer4hr.csv
$onlisting
$offdelim
/;

*       Define indices for generator and technology
set             gen(utility,plant,generator)    index for generator,
                tech(oc,fuel)                   index for technology;

tech(oc,fuel) = yes$sum((utility,plant,generator,t), heatrate(%i%));
tech(oc,"na") = no;
tech("na",fuel) = no;
gen(utility,plant,generator) = yes$sum((tech(oc,fuel),t), heatrate(%i%));

alias(gen,gen_);

*       Compute summerweight at generator level
parameter       summer_weight_g(oc,fuel,utility,plant,generator,t);

summer_weight_g(tech(oc,fuel),gen(utility,plant,generator),t)$summer_cap(tech,gen,t) =
                        summer_cap(tech,gen,t)/
                        sum(gen_(utility_,plant_,generator_),
                                        summer_cap(tech,utility_,plant_,generator_,t));

*       Check that all weights some to unity
parameter       check;
check(tech,t) = sum(gen(utility,plant,generator), summer_weight_g(tech,utility,plant,generator,t));
display check;

*       Compute average heatrate levels for each technology
parameter       avghr(oc,fuel,t);

avghr(tech(oc,fuel),t)$sum(gen(utility,plant,generator), heatrate(%i%)) =
                        sum(gen(utility,plant,generator), summer_weight_g(%i%)*heatrate(%i%));

$ontext
Results from weighted heatrate using summer_weights as weights

                                      1990        1991        1992        1993        1994

biomass           .biomass       14198.788   14391.794   14445.453   15306.274   14249.287
coal              .coal          11068.828   11090.220   10982.534   11020.649   10992.895
combined_cycle    .natural_gas   11459.803   11503.592   11425.106   11248.288   10864.078
combined_cycle    .oil           12823.406   13024.092   13018.194   14246.104   13304.551
combustion_turbine.natural_gas   12766.489   12560.515   12462.734   12446.366   12572.469
combustion_turbine.oil           12105.493   12218.632   11864.175   11874.670   12051.788
geothermal        .geothermal    16200.000   16200.000   19869.281   20008.798   26236.935
hydro             .water          5220.499               12780.468   10765.940
nuclear           .uranium       10789.434   10709.381   10691.688   10615.445   10545.568
steam_turbine     .natural_gas   12145.112   12048.647   12039.719   12111.836   11866.701
steam_turbine     .oil           11964.953   11935.121   11835.197   11924.898   11858.935
$offtext

*       AER2012 Approximate heat rate growth for years 1995-2007


table   growth(t,*)
        fossil          nuclear
1996    0.002715283     -0.000380699
1997    -0.012282398    -0.000856898
1998    -0.001566631    -0.000285878
1999    0.002843974     -0.003908112
2000    -0.002444749    -0.002009569
2001    0.012939908     0.001342411
2002    -0.01548437     -9.57579E-05
2003    0.006684361     -0.002011109
2004    -0.02138463     0.00057576
2005    -0.002294951    0.000863144
2006    -0.0080008      0
2007    -0.003528582    0.004695286;


*       Read in heatrates for years 2005 - 2015 from AEO data

table   eiareports(oc,fuel,t)
                                2007    2008    2009    2010    2011    2012    2013    2014    2015
biomass           .biomass                              13500   13500   13500   13500   13500   13500
coal              .coal         10158   10138   10150   10142   10128   10107   10089   10080   10059
combined_cycle    .natural_gas  7577    7642    7605    7619    7603    7615    7667    7658    7655
combined_cycle    .oil          10970   10985   10715   10474   10650   10195   9937    9924    9676
combustion_turbine.natural_gas  11632   11576   11560   11590   11569   11499   11371   11378   11302
combustion_turbine.oil          13217   13311   13326   13386   13637   13622   13555   13457   13550
nuclear           .uranium      10489   10452   10459   10452   10464   10479   10449   10459   10458
steam_turbine     .natural_gas  10440   10377   10427   10416   10414   10385   10354   10408   10372
steam_turbine     .oil          10398   10356   10349   10249   10414   10359   10334   10156   10197
;

avghr(oc,fuel,t)$(t.val ge 2007) = eiareports(oc,fuel,t);

*       Fill in data between 1996 and 2006

variable        alpha   weight applied to stochastic variation,
                HRVAR   heatrate variable,
                OBJ     objective;

equation        fit_coal,fit_gas,fit_oil,fit_nuclear, objdef;
fit_coal(oc,t)$(t.val le 2007 and t.val gt 1995)..
                HRVAR(oc,"coal",t) =E= avghr(oc,"coal","1995") *
                        prod(t_$(t_.val le t.val and t_.val ge 1996), 1+alpha(oc,"coal") * growth(t_,"fossil"));

fit_gas(oc,t)$(t.val le 2007 and t.val gt 1995)..
                HRVAR(oc,"natural_gas",t) =E= avghr(oc,"natural_gas","1995") *
                        prod(t_$(t_.val le t.val and t_.val ge 1996), 1+alpha(oc,"natural_gas") * growth(t_,"fossil"));

fit_oil(oc,t)$(t.val le 2007 and t.val gt 1995)..
                HRVAR(oc,"oil",t) =E= avghr(oc,"oil","1995") *
                        prod(t_$(t_.val le t.val and t_.val ge 1996), 1+alpha(oc,"oil") * growth(t_,"fossil"));

fit_nuclear(oc,t)$(t.val le 2007 and t.val gt 1995)..
                HRVAR(oc,"uranium",t) =E= avghr(oc,"uranium","1995") *
                        prod(t_$(t_.val le t.val and t_.val ge 1996), 1+alpha(oc,"uranium") * growth(t_,"nuclear"));

objdef..        OBJ =E=
                sum(oc,
                                sqr(HRVAR(oc,"coal","2007") - eiareports(oc,"coal","2007")) +
                                sqr(HRVAR(oc,"oil","2007") - eiareports(oc,"oil","2007")) +
                                sqr(HRVAR(oc,"natural_gas","2007") - eiareports(oc,"natural_gas","2007")) +
                                sqr(HRVAR(oc,"uranium","2007") - eiareports(oc,"uranium","2007"))
                                );

model findalpha /all/;
solve findalpha min OBJ using NLP;
display alpha.l;

loop(t$(t.val lt 2007 and t.val gt 1995),
        avghr(oc,"coal",t) = HRVAR.L(oc,"coal",t);
        avghr(oc,"natural_gas",t) = HRVAR.L(oc,"natural_gas",t);
        avghr(oc,"oil",t) = HRVAR.L(oc,"oil",t);
        avghr(oc,"uranium",t) = HRVAR.L(oc,"uranium",t);
);

*       Read in heatrates for noncombustible renewables from Annual Energy Review 2012

parameter       renew_aer2012(t) /

        1990    10402
        1991    10436
        1992    10342
        1993    10309
        1994    10316
        1995    10312
        1996    10340
        1997    10213
        1998    10197
        1999    10226
        2000    10201
        2001    10333
        2002    10173
        2003    10241
        2004    10022
        2005    9999
        2006    9919
        2007    9884
        2008    9854
        2009    9760
        2010    9756

*       Considering the efficiency decrease for 1980-2010 average rate decrease is linear at 0.206 %
*       We interpolate for 2011-2015
        2011    9735.867812
        2012    9715.777169
        2013    9695.727983
        2014    9675.720171
        2015    9655.753646
        /;

avghr("geothermal","geothermal",t) = renew_aer2012(t);
avghr("hydro","water",t) = renew_aer2012(t);
avghr("photovoltaic","solar",t) = renew_aer2012(t);
avghr("solar_thermal","solar",t) = renew_aer2012(t);
avghr("solar_thermal","solar",t) = renew_aer2012(t);
avghr("wind","wind",t) = renew_aer2012(t);

positive variable       DECAY;
variable                BIO_EST;
equation                biomass_est,obj_biomass;

biomass_est(t)..        BIO_EST(t) =E= avghr("biomass","biomass","1995") * power((1-DECAY),t.val-1995);
obj_biomass..           OBJ =E= sqr(BIO_EST("1995") - avghr("biomass","biomass","1995")) +
                                sqr(BIO_EST("2010") - avghr("biomass","biomass","2010")) +
                                sqr(BIO_EST("2015") - avghr("biomass","biomass","2015"));

model biomass_estimate  /biomass_est, obj_biomass/;
solve biomass_estimate using NLP minimizing OBJ;

loop(t$(t.val gt 1995),
        avghr("biomass","biomass",t) = BIO_EST.L(t);
);

execute_unload 'avghr.gdx', avghr;
execute 'gdxdump avghr.gdx output=avghr.csv delim=comma Format=csv symb=avghr';




