Importance is calculated as `weight/time + crunch + noise`

Given that every item has an `rt.timescale` (at least one hour), take that timescale in hours as h.
Then, time is `ln(h)+1` which has a domain roughly in `[1, 11]`.

Weight is given directly from `rt.weight` which is in the range `[1,11]`
The values are givven by the powers of the `n`th root of 11 where `n` is the number of distinct weights.

Crunch is calculated from `rt.timescale` and `deadline`.
When deadline does not exist, `crunch = 0`.
First, calculate the number of timescale intervals remaining: `intervals = deadline - current_date / rt.timescale`
Then, we've tuned a sigmoid function: `sig(x) = atan(x/1.618 + pi) / pi + 0.5`
    * the `+ pi` is to adjust zero intervals to be worth 10 crunch
    * the `/1.618` is to adjust ~10 intervals to about ~3 crunch
    * the `/pi + 0.5` is to adjust the range to [0, 1]
Finally, crunch is `11*sig(-intervals)`.

Noise is calculated simply as a random number between 0 and 1.