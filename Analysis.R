# What is Banknifty ?
#Nifty Bank, or Bank Nifty, is an index of the most liquid and large capitalised Indian banking stocks. It provides investors with a benchmark that captures the capital market performance of Indian bank stocks. The index has 12 stocks from the banking sector.

#What is in the DataSet ?
#  This dataset comprises of historical daily chart of BankNifty movement from 2000 to all the way to 2022.

#Data Description
#The data has 9 columns (excluding the index):
  
 # Time: date
#Open: Open price for the day.
#High: Highest price for the day.
#Low: lowest price for the day.
#Close: Close price for the day.
#Weekday: 0 means Monday and 5 means Friday. This column represents which day was it.
#range_HL: Market range from high to low.
##range_OC: Market range from open to close.
#type: Market type (bullish = close>open), (bearish = close)

# Work flow: 

# 1) Load libraries 
# 2) Clean data (change the date into the right format)
# 3) Understand differences in the market when its bullish and bearish
# 4) Stats difference (t-tests maybe anova)
# 5) pairs plot for visualisation
# 5) Make Market type a response (try and understand how thing change when the markets are bulish and bearish)
# 6) 




