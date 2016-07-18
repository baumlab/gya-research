## GYA - survey data treatment
#### Rules for aggregating data

1. Research type % 

We have % fundamental, use-inspired and applied. These should sum to 100%.

Problem: some surveys have > 100% research, some surveys have blanks (which should be zeroes), some didn't answer the question.

1. If every category = blank, turn to NA
2. If some categories have answers but others are blank, turn blank to 0.
3. If research total > 100%, divide each % by the summed research total and multiply by 100

