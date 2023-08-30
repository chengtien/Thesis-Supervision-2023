## Basket of goods

The basket of goods is a list of items that are used to calculate the Consumer Price Index (CPI). The CPI is a measure of the average change over time in the prices paid by urban consumers for a market basket of consumer goods and services. The CPI is calculated by taking price changes for each item in the predetermined basket of goods and averaging them; the goods are weighted according to their importance. Changes in CPI are used to assess price changes associated with the cost of living; the CPI is one of the most frequently used statistics for identifying periods of inflation or deflation.

***

`persistent_popularItems_count` gives you the frequency table of various persistent popular item counts. Your own algorithm shoul produce the same result as the one below.

|persistent_popularItems_count |  Freq|
|:-----------------------------|-----:|
|0                             |  5557|
|1                             |   835|
|2                             |  1644|
|3                             |  2274|
|4                             |  3491|
|5                             |  6366|
|6                             | 17710|


I modified your code and wrapped it in a function called `construct_popularItems_common_across_threePeriods`. If you encounter algorithm coding program, you should take a look. In my code, I use a lot of `shopCode` indexing to save time by avoiding the need to do data merging.

