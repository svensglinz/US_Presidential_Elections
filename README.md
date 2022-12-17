# :us: US Presidential Elections 

## :moneybag: Data

The analyzed data consists of all direct, itemized contributions which were downloaded from the [FEC Bulk Downloader](https://www.fec.gov/data/browse-data/?tab=bulk-data). This also includes all donations made theough third party platforms such as Winred or Actblue.

I further include the share of donations collected through Joint Fundraising Comittees that were passed on to the presidential campains. This data is also available through the FEC bulk download under *any transaction from one commitee to another*

## :bar_chart: Analysis

Wherever the number of donors are analyzed, donations are first gruped by Name, Profession, Zip Code and Recipient Canidate in order to aggregate multiple donations by the same individual. 

## :woman:Gender Mapping:man:

A gender is assigned to each name based on data from the SSA which contains data on each baby born in the US, the name and its gender. If a name is used as Male / Female in more than 75% of cases, I determine it as exclusively Male / Female. 

## :warning: Limitations

By law, only donations made through third party providers (such as ActBlue or Winred)
or those which cumulatively exceed 200 USD per cycle must be reported 

eg. If somebody donates 5 * 50, only the last 50 USD are reported (once it exceeds
the 200 USD threshold)
However, if somebdoy donates 250 USD at once, the entire donation is recorded. 
For more on this, see eg: [The Hidden Donor Problem](https://www.liebertpub.com/doi/10.1089/elj.2019.0593)

Further, I do not exclude mistakes made when analyzing the data as the donation maze as well as how the data is recorded is quite complex, so feel free to point out any mistakes or methodological shortcomings should you see them :smile: