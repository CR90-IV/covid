# How much COVID-19 is there in the UK?

Using UKHSA data (coronavirus.gov.uk) and corrections for underreporting and delays.

## By region

![Prevalence by region](/plots/prevalence_by_region.png)

## By local authority (UTLA)
WIP

# Method
This uses a method from the [microCOVID project](https://www.microcovid.org/paper/all#detailed-steps-for-basic-method).

We estimate the true number of new cases as

```
[estimated true new cases] = [reported new cases] * [underreporting factor] * [delay factor]
```

## `reported new cases`
We use the seven-day average of new cases by specimen date from the [GOV.UK COVID-19 dashboard API](https://coronavirus.data.gov.uk/).

## `underreporting factor`
Not all cases are tested or reported. The positivity rate can be used to estimate the extent of this, in a method from [COVID-19 Projections](https://covid19-projections.com/estimating-true-infections-revisited/).

```
[underreporting factor] = 1500/(day_i + 10) * positivityRate**0.5 + 2
```
where `day_i` is the number of days since 12th February 2020, 14 days prior to [known community transmission in Edinburgh on 26th February](https://en.wikipedia.org/wiki/Timeline_of_the_COVID-19_pandemic_in_Scotland_(2020)#February_2020).

## `delay factor`
Case data is incomplete, usually for the last 5 days.



