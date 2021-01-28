****data from Kakkar and Sivanathan (2017)

clear
set more off
use "................................................./DataStudy1DominantLeaders.dta"

*** start of code by Kakkar and Sivanathan (2017))
sort startdate
duplicates drop ipadd, force   // dropping those with duplicate IP address
duplicates drop worker, force  // dropping those with duplicate MTurk ID
rename q19 eng
drop if eng == 2

sort populationdensity

g density = 1 if populationdensity == "Low"
replace density = 2 if populationdensity == "Medium"
replace density = 3 if populationdensity == "High"
replace density = 4 if populationdensity == "Very High"


recode vote (2=0) //voting for Hillary recoded as 0 
recode vote(4=2) // voting for neither coded as 2
alpha poverty ofadultsnotinwork housingvacancyrate, g(econuncert)

*** Our code

*** CI set to 89% 

set level 89

*** COMPARING A NULL FIXED INTERCETP MODEL WITH A RANDOM INTERCEPT MODEL

***Null Model with fixed intercept (AIC = 1541.978)
gsem (0.vote <- ) (1.vote <- ), mlogit 

estat ic


***Null Model with random intercept (AIC = 1544.296)
gsem (0.vote <- M1[state]) (1.vote <- M1[state]), mlogit latent(M1) nocapslatent

estat ic

*** Fit of null model with fixed intercept is better than the fit of null model with random intercept


*** FIXED INTERCEPT REGRESSION MODELS (MAIN ANALYSIS)

***Control Model (AIC = 1518.949)

mlogit vote gender age income, base(2)

estat ic

***Economic Uncertainty Model (separated measures) (AIC = 1516.061)

mlogit vote gender age income poverty ofadultsnotinwork housingvacancyrate, base(2)

estat ic




***Ideology Model
mlogit vote gender age income polo, base(2)

estat ic


***Full Model (separated measures) (AIC = 1516.061)
mlogit vote gender age income polo poverty ofadultsnotinwork housingvacancyrate, base(2)

estat ic


*** ALTERNATIE ANALYSIS 1 (fixed intercepts with economic uncertainty as a composite variable )
***Economic Uncertainty Model (composite measurement)

mlogit vote gender age income econunce, base(2)

estat ic


***Full Model (economic uncertainty = composite measurement)
mlogit vote gender age income polo econunce, base(2)

estat ic

*** ALTERNATIE ANALYSIS 2 (random intercepts with separated economic uncertainty variables)

***Control Model
gsem (0.vote <- gender age income M1[state]) (1.vote <- gender age income M1[state]), mlogit latent(M1) nocapslatent

estat ic

***Economic Uncertainty Model
gsem (0.vote <- gender age income poverty ofadultsnotinwork housingvacancyrate M1[state]) (1.vote <- gender age income poverty ofadultsnotinwork housingvacancyrate M1[state]), mlogit latent(M1) nocapslatent

estat ic

***poverty  Model
gsem (0.vote <- gender age income poverty M1[state]) (1.vote <- gender age income poverty M1[state]), mlogit latent(M1) nocapslatent

estat ic

***Unemployment Model
gsem (0.vote <- gender age income ofadultsnotinwork M1[state]) (1.vote <- gender age income ofadultsnotinwork M1[state]), mlogit latent(M1) nocapslatent

estat ic

***Housing vacancy Model
gsem (0.vote <- gender age income housingvacancyrate M1[state]) (1.vote <- gender age income housingvacancyrate M1[state]), mlogit latent(M1) nocapslatent

estat ic

***Ideology Model
gsem (0.vote <- gender age income polo M1[state]) (1.vote <- gender age income polo M1[state]), mlogit latent(M1) nocapslatent

estat ic

***Full Model
gsem (0.vote <- gender age income polo poverty ofadultsnotinwork housingvacancyrate M1[state]) (1.vote <- gender age income polo poverty ofadultsnotinwork housingvacancyrate M1[state]), mlogit latent(M1) nocapslatent

estat ic

*** ALTERNATIE ANALYSIS 3 (random intercepts with composite economic uncertainty variable)

***Economic Uncertainty Model
gsem (0.vote <- gender age income econunce M1[state]) (1.vote <- gender age income econunce M1[state]), mlogit latent(M1) nocapslatent

estat ic


***Full Model
gsem (0.vote <- gender age income polo econunce M1[state]) (1.vote <- gender age income polo econunce M1[state]), mlogit latent(M1) nocapslatent

estat ic


*** ALTERNATIE ANALYSIS 4 (single full model standardized variables)

*Standardization of variables
egen Zage = std(age)

egen Zincome =std(income)

egen Zlivezip = std(livezip)

egen Zpolo = std(polo)


egen Ztotalpop = std(totalpop)

egen Zdensity = std(density)

egen Znumberofzipsincounty = std(numberofzipsincounty)

egen Zeconunce = std(econunce)

gsem (0.vote <- gender Zage Zincome Zlivezip Zpolo Ztotalpop Zdensity Znumberofzipsincounty Zeconunce) (1.vote <- gender Zage Zincome Zlivezip Zpolo Ztotalpop Zdensity Znumberofzipsincounty Zeconunce), mlogit nocapslatent

*** ALTERNATIE ANALYSIS 5 (single full model standardized variables and random intercepts)

gsem (0.vote <- gender Zage Zincome Zlivezip Zpolo Ztotalpop Zdensity Znumberofzipsincounty Zeconunce M1[state]) (1.vote <- gender Zage Zincome Zlivezip Zpolo Ztotalpop Zdensity Znumberofzipsincounty Zeconunce M1[state]), mlogit latent(M1) nocapslatent
