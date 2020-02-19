
# Statistical Process Charts

This repo contains code to analyse time series datasets using statistical process charts (SPC).

I have used control charts in the past for data such as delayed transfers of care using standard deviations for control limits.

Recently (December 2019) I became aware of the #plotthedots initiative from NHS improvement and was inspired to return to my previous analysis and improve my SPC charts.

This uses the xmR (moving average) rather than standard deviations as control limits.

### A brilliant post I found after a few hours trying to figure out the difference between SD and sigma. https://r-bar.net/xmr-control-chart-tutorial-examples/ this explains the constant value 1.128

### This is also a very very useful guide from NHS improvement (https://improvement.nhs.uk/documents/5478/MAKING_DATA_COUNT_PART_2_-_FINAL_1.pdf) for when to use SPCs, which rules to apply and sensible tips for when to recalculate limits or produce multiple process SPCs.

The text below is a collection from my readings. It is a series of notes and also some questions you should pose yourself when using an SPC. The take home message is that you need to use the context of the data to decide which parts of the template to use.

The standard deviation, represents the total variation in our data. Total variation is comprised of random variation and systematic variation. When we make an XmR chart, our control limits should represent the random component to the variation in our process. We get this though the mR derived sequential deviation discussed above. The benefit of plotting the random variation window of our process (3 * sequential deviation), is that we can detect when systemic variation creeps into our process. And, hopefully do some corrective action to remove it

What value rule should take precedent? A single value might be a special cause of concern as well as in a run of above process mean and in a trend of increasing values.

NHS Improvement use icons to signify overall performance of the whole data series. There are five outcomes -
1. common cause variation (random/natural variation only).
2. special cause variation in relation to high values (e.g. values above upper control limit, or consistent consecutive pattern of values above process mean/target).
3. special cause variation in relation to low values (e.g. values below lower control limit, or consistent consecutive pattern of values below process mean/target indicative of a shift).
4. special cause variation in relation to upward trends indicative of improvement (e.g. consecutive values increasing over time (usually 6 or 7 points)).
5. special cause variation in relation to downward trends indicative of improvement (e.g. consecutive values decreasing over time).

Whether outcome 4 or 5 is relevant will depend on the polarity of the outcomes (is an increase good (e.g. service uptake) or decrease good (non-elective admissions down)).

What constitutes special cause and common cause variation and improvement will depend on polarity of the data in question. Further, the rules applied to the dataset will also depend on what you are trying to identify. You may want to apply different rules to different tasks. For example, you could use 1 and 2 sigma limits to show areas that may be of concern.

For assessing the likelihood of meeting a target you should expect values to consistently stay above the target. There are three outcomes -
* process control chart indicates values consistently achieve target.
* process control chart indicates values consistently do not achieve target.
* process control chart indicates that the target is within common cause variation of values (e.g. values fall below and above target over time).

SPC charts can also enable you to see if a particular process can meet a specific target. The calculation used will determine the process capability.
The calculation used to determine process capability is:
Capability = Target - Average / (3 * standard deviation)

A value of 1 means the process is 100% capable of achieving the target. A negative figure means more than 50% of patients will not meet a given target.

The procedure for calculating process capability is therefore:
 * Test for stability by plotting a control chart first
 * If unstable, gain control by identifying and controlling the main factors that affect the situation
 * Only if it is stable, calculate a process capability to determine if it is capable of meeting the target

### Recalculation of limits part way through data when interventions or events occur that meaningfully change the dataset.

I would try adding a new variable showing two or more distinct processes/interventions (e.g. time_1 may be 2017/18 - 2018/19 and time_2 may be 2019/20 onwards). By creating this and using group_by, you should be able to set mR and sigma limits for each process set.

If you have several processes going on you may wish to split the dataset into those processes and produce an SPC for each one. An example of this can be daily admissions to hospital. Monday admissions may be very different to the rest of the working week and different still to weekend admissions. You may consider plotting a monday, a tuesday to friday and a saturday/sunday SPC.

## In this repo

The R script calculates values for the figure as well as rules for patterns in the values (such as runs above and below the process mean, drifts and trends in consecutive values). SPC rules must be carefully decided based on context of the data and what we are looking for (understand variation or assurance). Rules also depend on polarity (where higher or lower is better).

Script name | Comment
------------| -------------
spc_template.r | This helps to build the analysis and create ggplot spc charts as well as export the data to a json file for use in web applications
