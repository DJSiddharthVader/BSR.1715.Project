# Religiosity 
## make list of religions
# Ones im like erm . do we include bc cultural vs actural organized religion? 
# NATIVE AMERICAN TRADITIONS, WICCA, HUMANIST, 
RELIGIONS_LABELS <- 
    c( 
        "AFRICAN DIASPORA TRADITIONS",
        "BAHÁ'Í",
        "BAPTIST",
        "BUDDHIST",
        "CATHOLIC",
        "CHRISTIAN",
        "EVANGELICAL",
        "HINDU",
        "JAIN",
        "JEHOVAH WITNESS",
        "JEWISH",
        "LATTER-DAY SAINTS/MORMON",
        "MUSLIM",
        "NATIVE AMERICAN TRADITIONS",
        "ORTHODOX CHRISTIAN",
        "PAGAN/WICCAN",
        "PENTECOSTAL",
        "PROTESTANT",
        "SEVENTH DAY ADVENTIST",
        "SHINTO",
        "SIKH"
    )
NON_RELIGIOS_LABELS <- 
    c(
        "UNAFFILIATED/NONE",
        "None",
        "ATHEIST",
        "HUMANIST",
        "SPIRITUAL",
        "NOT RELIGIOUS"
    )
UNKNOWN_LABELS <- 
    c(
        "UNKNOWN/OTHER",
        "UNKNOWN/UNABLE TO OBTAIN",
        "PATIENT DECLINED",
        "OTHER",
        "Unknown",
        "Other",
        "PT Declined"
    )
# Marital Status
# lists of marital statuses
# here we could shift because we have like divorced, widowed to add a
# component of change but like we dk when that happened saur 
#sticking to either unknown, married, relationship, or single for everyone else
MARRIED_LABELS <- c("MARRIED/CIVIL UNION")
SIGNIFICANT_OTHER_LABELS <- 
    c(
        "MARRIED/CIVIL UNION",
        "SIGNIFICANT OTHER/LIFE PARTNER"
    )
