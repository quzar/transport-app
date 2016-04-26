(in-package :polyforms)

;; Distance, Area, Volume Units (Imperial)
(defconstant +ft/yard+ 3.0)
(defconstant +yard/mile+ 1760)
(defconstant +acre/mile2+ 640)
(defconstant +acre/ha+ 2.5)

;; Distance, Area, Volume Units (Metric)
(defconstant +L/gallon+ 4.546)
(defconstant +L/us-gallon+ 3.7843)
(defconstant +km/mile+ 1.6093)
(defconstant +m/ft+ 0.3048)
(defconstant +m2/ft2+ 0.092903)
(defconstant +m3/ft3+ 0.028317)

;; Mass Units (Metric)
(defconstant +kg/lb+ 0.45359)
(defconstant +tonnes/kg+ 1000)

;; Energy Units (Metric)
(defconstant +kJ/kcal+ 4.184)
(defconstant +BTU/kcal+ 3.966)
(defconstant +MET/kcal-kg-h+ 1)

;; Energy Use (Food)
(defconstant +MET-idle+ 1.0)
(defconstant +MET-driving+ 2.0)
(defconstant +MET-walking+ 4.0)
(defconstant +MET-cycling+ 8.0)
(defconstant +idle-kJ/h+ 609)

;; Energy Content
(defconstant +kcal/g-fat+ 9)
(defconstant +kcal/g-alcohol+ 7)
(defconstant +kcal/g-proteins+ 4)
(defconstant +kcal/g-carbohydrates+ 4)
(defconstant +kcal/g-organic-acids+ 3)
(defconstant +kcal/g-sweeteners+ 2.4)
(defconstant +kJ/L-diesel+ 35500)
(defconstant +kJ/L-gasoline+ 31600)

;; Food Waste
(defconstant +food-production-waste+ 10.36)
(defconstant +dairy-production-waste+ 21.60)
(defconstant +beef-production-waste+ 64.80)

;; Energy Waste
(defconstant +diesel-engine-efficiency+ 0.45)
(defconstant +gasoline-engine-efficiency+ 0.3)
(defconstant +automobile-lifecycle-cost+ 1.25)

;; Emissions Sources
(defconstant +kg-carbon/kg-CO2+ 0.2727)
(defconstant +kg-CO2/L-gasoline+ 2.34)
(defconstant +kg-CO2/kWh-electricity+ 0.882)
(defconstant +kg-CO2/km-travelled+ 0.1996)

;; Pontiac Vibe Properties
(defconstant +vibe-cargo-seats-up-L+ 547)
(defconstant +vibe-cargo-seats-down-L+ 1533)
(defconstant +vibe-fuel-tank-L+ 50)
(defconstant +vibe-curb-weight-kg+ 1225)

;; Vibe Fuel Efficiency
(defconstant +vibe-highway-L/km+ 0.0856) ; was 6.4 L/100km
(defconstant +vibe-city-L/km+ 0.1009) ; was 8.3 L/100km
(defconstant +vibe-rush-hour-L/km+ 0.1261)

;; Other Fuel Efficiency
(defconstant +average-L/km+ 0.1165)
(defconstant +bus-L/km+ 0.01165) ; 10x better than cars
(defconstant +via-rail-L/km+ 0.0233) ; 5x better than cars
(defconstant +airplane-L/km+ 0.0480) ; 2.5x better (???)

;; Environmental Costs
(defconstant +CO2-$/kg+ 0.02889)

;; Driving Costs (per km)
(defconstant +gasoline-$/L+ 0.9950)
(defconstant +car-tires-$/km+ 0.0179)
(defconstant +car-maintenance-$/km+ 0.0249)
(defconstant +car-related-fines-$/km+ 0.0083)
(defconstant +car-extra-depreciation-$/km+ 0.1389)

;; Driving Costs (paid annually)
(defconstant +annual-driving-km+ 18000)
(defconstant +car-annual-depreciation-$/km+ 0.2365)
(defconstant +car-license-registration-$/km+ 0.0065)
(defconstant +car-insurance-$/km+ 0.0652)
(defconstant +car-parking-$/km+ 0.05396)

;; Cycling Costs (per km)
(defconstant +annual-cycling-km+ 3400)
(defconstant +bike-maintenance-$/km+ 0.0310)
(defconstant +bike-extra-depreciation-$/km+ 0.0166)
(defconstant +bike-annual-depreciation-$/km+ 0.0284)

;; Speed Constants
(defconstant +transit-km/h+ 19.3333)
(defconstant +two-feet-km/h+ 6.6666)
(defconstant +bicycle-km/h+ 20.0000)
(defconstant +vibe-highway-km/h+ 105)
(defconstant +vibe-city-km/h+ 66.66)
(defconstant +vibe-rush-hour-km/h+ 26.5)
(defconstant +greyhound-km/h+ 79.4213)
(defconstant +shuttle-km/h+ 28.8764)
(defconstant +red-arrow-km/h+ 99.2767)
(defconstant +coach-km/h+ 99.2767)
(defconstant +via-rail-km/h+ 60.9960)
(defconstant +airplane-km/h+ 743.615)

;; Transportation Time
(defconstant +bicycle-extra-h+ 0.0583)
(defconstant +bus-stop-extra-h+ 0.0833)
(defconstant +parking-lot-extra-h+ 0.10)
(defconstant +rental-car-extra-h+ 0.25)
(defconstant +red-arrow-extra-h+ 0.25)
(defconstant +greyhound-extra-h+ 0.50)
(defconstant +via-rail-extra-h+ 0.67)
(defconstant +airport-extra-h+ 1.00)
(defconstant +airport-parking-extra-h+ 0.25)
(defconstant +shuttle-extra-h+ 0.1667)
(defconstant +taxi-extra-h+ 0.1667)

;; Transportation Costs
(defconstant +transit-cost-$+ 2.10)
(defconstant +shuttle-cost-$+ 12.50)
(defconstant +rental-cost-$+ 53.00)
(defconstant +greyhound-$/km+ 0.1085)
(defconstant +red-arrow-$/km+ 0.1929)
(defconstant +coach-$/km+ 0.1494)
(defconstant +via-rail-$/km+ 0.1675)
(defconstant +airplane-$/km+ 0.37263)

;; Hotel Costs
(defconstant +fairmont-hotel-cost-%+ 0.60)
(defconstant +hostel-cost-$/person+ 33.75)
(defconstant +campground-cost-$+ 30.00)

;; Food Costs
(defconstant +food-$/kJ+ 1.4575e-3) ; 7.2875e-4)

(defconstant +crackers-$/100g+ 1.105)
(defconstant +hummus-$/100g+ 1.316)
(defconstant +fish-$/100g+ 2.343)

(defconstant +crackers-kJ/100g+ 1785)
(defconstant +hummus-kJ/100g+ 832)
(defconstant +fish-kJ/100g+ 651)

;; Other Costs
(defconstant +park-pass-annual-$+ 123.80)
(defconstant +park-pass-daily-$+ 17.80)
(defconstant +used-goods-rate-%+ 0.85)
(defconstant +tip-rate-%+ 1.15)
(defconstant +road-distance-error-%+ 1.15)

;; Aeroplan Stuff
(defconstant +aeroplan-use-hotel-mile/$+ 102.67)
(defconstant +aeroplan-use-flight-mile/$+ 50.53)
(defconstant +aeroplan-fly-mile/$+ 2.26)
(defconstant +aeroplan-buy-mile/$+ 33.33)
(defconstant +aeroplan-amex-mile/$+ 175.00)

;; Wages & Pay
(defconstant +working-wage-$/h+ 29.566)
(defconstant +free-time-$/h+ 14.783)
(defconstant +winter-$/h+ 5.00)

;; Miscellaneous
(defconstant +city-size-km+ 29)
(defconstant +baggage-$/km+ 0.11023)
(defconstant +MET-climb-factor+ 28.57143)
(defconstant +MET-baggage-factor+ 0.22567)

;; Dates
(defconstant +summer-length-days+ 238)
(defconstant +winter-length-days+ 127)
