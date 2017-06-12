PRO WIDTH_SII_HA, WHALPH, WSII, V_NON_THERM, TEMP

    V_NON_THERM = [ [ (WHALPH/21.4D)^2.0D - (WSII/3.78D)^2.0D - 0.070D ] / [ (1/12.8D)^2.0D - (1/2.27D)^2.0D ] ]^(.5D)

    TEMP = 1.0E4 * [ (WHALPH/21.4D )^2.0D - (V_NON_THERM/12.8D)^2.0D - 0.070D  ]

END
 
