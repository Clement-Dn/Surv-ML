# Libraries
library(timereg)


# Data loading & cleaning
df = data(melanoma)
#no a numeric vector. Patient code.
#status a numeric vector code. Survival status. 1: dead from melanoma, 2: alive, 3: dead from other cause.
#days a numeric vector. Survival time.
#ulc a numeric vector code. Ulceration, 1: present, 0: absent.
#thick a numeric vector. Tumour thickness (1/100 mm).
#sex a numeric vector code. 0: female, 1: male.
