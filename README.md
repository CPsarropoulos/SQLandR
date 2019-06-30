# SQLandR
R script that connects to a SQL database extracts data then connects to a flat file merges and wrangles the data from both sources and creates an xlsx based report


## Objective

You are a data analyst for an advertising technology company called "Advert". The company manages advertiser campaigns and the people responsible for that are the account managers. A need for a new report has emerged from the account managers and you are asked to help with that.

## Context

* Each account manager handles multiple advertising campaigns and the goal of a campaign is to acquire new users for the advertiser’s web digital services.

* We track when user sign-up for a digital services and we call this event conversion.
* The advertiser pays us a fixed amount (called payout) for every campaign conversion.
* The total amount an advertiser pays is called spend.
* Some conversions might be rejected because of fraudulent activity, expired events or any other reason so these conversions should NOT be used in our calculations.
* Some conversions might be duplicates and should NOT be used in our calculations.




| Data                 | Description                                          | Format   |
|----------------------|:----------------------------------------------------:|---------:|
| Rejected Conversions |  A list of conversion ids that are rejected for various reasons |      cvc |
| Conversion Events  |The set of conversion events. Each event consists of the conversion id (which uniquely identifies a conversion) and the campaign id which identifies for which campaigns this conversion belongs to.|      txt |
| Campaign Info  | It contains info about the campaign: the unique campaign id , the payout for each conversion ,the account manager responsible for the campaign |        db|




## Reports Needed:

1. A spreadsheet report displaying the total spend for each campaign per account manager.
2. A. speadsheet report displaying the daily difference in spend for each campaign.
