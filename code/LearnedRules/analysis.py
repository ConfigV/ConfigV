import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.style.use('ggplot')
#plt.xkcd()

def show_yes_and_no_dist(df, title_add=''):
    print("number of observations: " + str(df.size))
    print("mean number of yes " + str(np.mean(df['yes'])))
    print("median number of yes " + str(np.median(df['yes'])))
    print("mean number of no " + str(np.mean(df['no'])))
    print("median number of no " + str(np.median(df['no'])))
    plt.title('Distribution of Yes' + ' ' + title_add)
    plt.ylabel('Frequency')
    plt.xlabel('Number of Observed Yes\'s')
    plt.hist(np.array(df['yes']), bins=100)
    plt.show()
    plt.title('Distribution of No' + ' ' + title_add)
    plt.ylabel('Frequency')
    plt.xlabel('Number of Observed No\'s')
    plt.hist(np.array(df['no']), bins=100)
    plt.show()

'''
For x-y plot of probability vs. number of observations
'''

df1 = pd.read_csv('bigMissingP.psv', delimiter='|')
df2 = pd.read_csv('bigOrderP.psv', delimiter='|')

print(df1['yes'] / (df1['yes'] + df1['no']))

df1['prob'] = df1['yes'] / (df1['yes'] + df1['no'])
#df2['prob'] = df2['yes'] / (df2['yes'] + df2['no'])

df1['obs'] = df1['yes'] + df1['no']
#df2['obs'] = df2['yes'] + df2['no']

plt.title('Number of Observations and Probability of Each Rule')
plt.ylabel('Probability')
plt.xlabel('Observations')
plt.scatter(np.array(df1['obs']), np.array(df1['prob']))
plt.xlim([0, 1.1*max(np.array(df1['obs']))])
plt.ylim([0, 1.25])
plt.show()

# now a two-series plot
dft = df1[df1['valid']]
dff = df1[~df1['valid']]
plt.title('Number of Observations and Probability of Each Rule')
plt.ylabel('Probability')
plt.xlabel('Observations')
false_rules = plt.scatter(np.array(dff['obs']), np.array(dff['prob']), color='blue')
true_rules = plt.scatter(np.array(dft['obs']), np.array(dft['prob']), color='red')
plt.xlim([0, 1.1*max(np.array(df1['obs']))])
plt.ylim([0, 1.25])
plt.legend([true_rules, false_rules], ['Rules found by complete system', 'Rules NOT found by complete system'])
plt.show()

# what about a histogram of yes - no counts?
df1['diff'] = df1['yes'] - df1['no']
plt.title('Distribution of Difference Between Yes and No Observations')
plt.ylabel('Frequency')
plt.xlabel('Difference')
plt.hist(np.array(df1['diff']), bins=100)
plt.show()

'''
It looks like for Missing value rules, since most of them carry perfect
100% probabilities (mostly 1 yes 0 no), the ones that count are the ones with 
an abnormally high observation counts (way tail of the distribution).

This benchmark too the top three with the highest number of observations, but
it still remains to be seen how accurate the other high-observation rules are.
'''


df = pd.read_csv('bigMissingP.psv', delimiter='|')
show_yes_and_no_dist(df, 'for MissingP Rules')
# Probability distributions?
df['prob'] = df['yes'] / (df['yes'] + df['no'])
plt.title('Distribution of Observed Probabilities for the MissingP Rule')
plt.ylabel('Frequency')
plt.xlabel('Probability')
plt.hist(np.array(df['prob']), bins=100)
plt.show()
# This seems like a likely cutoff for the data?
plt.title('Distribution of Observed Probabilities for the MissingP Rule (P > 0.8)')
plt.ylabel('Frequency')
plt.xlabel('Probability')
plt.hist(np.array(df[df['prob'] > 0.8]['prob']))
plt.show()
print(df[df['prob'] > 0.8].sort_values('prob'))
show_yes_and_no_dist(df[df['prob'] > 0.8], 'for MissingP Rules (P > 0.8)')
# Should we do this culling?
culled = df[df['prob'] > 0.8][df['yes'] > 10]
print(culled.sort_values('prob'))
print(culled.size)

'''
For the ordering, we have that most rules will, again be at a 100% accuracy,
with a bunch even appearing in the non-probabilistic set of learned rules.
(Even at a count of 1 yes and 0 no.) Since most rules that are caught by the 
probabilistic case are also presented by the non-probabilistic learner, it is
more interesting to analyze which rules are rejected by the non-probabilistic 
system.

Of the rules not caught by the non-probabilistic system, we have rules with a 
huge number of observations, and probabilities at either 33% or 67%. As a
matter of fact, rules that have more than 1 observation are most likely not
learned through the non-probabilistic system.
'''


df = pd.read_csv('OrderP.psv', delimiter='|')

# See where the probabilities and the non-100% probabilities are
df['prob'] = df['yes'] / (df['yes'] + df['no'])
print(df.sort_values('prob')) # see some top and low probabilities
plt.title('Distribution of Observed Probabilities for the OrderP Rule')
plt.ylabel('Frequency')
plt.xlabel('Probability')
plt.hist(df['prob'], bins=100)
plt.show()
plt.title('Distribution of Observed Probabilities for the OrderP Rule (P < 1.0)')
plt.ylabel('Frequency')
plt.xlabel('Probability')
plt.hist(np.array((df[df['prob'] < 1.0])['prob']), bins=100)
plt.show()

# See where the total number of observations are for rules marked valid or not
df['observations'] = df['yes'] + df['no']
plt.title('Distribution of Number of Observations for OrderP Rules Found by Non-Probabilistic System')
plt.ylabel('Frequency')
plt.xlabel('Observations')
plt.hist(np.array(df[df['valid']]['observations']), bins=100)
plt.show()
plt.title('Distribution of Number of Observations for OrderP Rules NOT Found by Non-Probabilistic System')
plt.ylabel('Frequency')
plt.xlabel('Observations')
plt.hist(np.array(df[~df['valid']]['observations']), bins=100)
plt.show()
# and maybe the validity of rules with more than 1 observation?
plt.title('Distribution of Number of Observations for OrderP Rules Found by Non-Probabilistic System (with more than one observation)')
plt.ylabel('Frequency')
plt.xlabel('Observations')
plt.hist(np.array(df[df['observations'] > 1]['valid']), bins=100)
plt.show() # so it is entirely false... if we have more than 1 observation...

# what are the distributions of no's and yes's
show_yes_and_no_dist(df, 'for OrderP Rules')
print("Distribution for valid rules")
show_yes_and_no_dist(df[df['valid']], 'for OrderP Rules from Non-Probabilistic System')
print("Distribution for invalid rules")
print(df[~np.array(df['valid'])]['yes'])
show_yes_and_no_dist(df[~np.array(df['valid'])], 'for OrderP Rules NOT from Non-Probabilistic System')


'''
For IntRelP, we should start looking through how many are marked as undecided
by the non-probabilistic learner.
'''

df = pd.read_csv('bigIntRelP.psv', delimiter='|')
df['observations'] = df['less_than'] + df['equals'] + df['greater_than']
print("Number of probabilistic rules: " + str(df.size))
print("Number of inconclusive probabilistic rules: "
 + str(df[df['answer'] == 'Nothing'].size))
# see how the observations and probabilities of inconclusive rules line up
print(df[df['answer'] == 'Nothing'].sort_values('observations', ascending=False))
# distribution of observations
plt.title('Distribution of Number of Observations for IntRelP Rules')
plt.xlabel('Observations')
plt.ylabel('Frequency')
plt.hist(np.array(df['observations']), bins=100)
plt.show()
# distribution of observations for inconclusive rules
plt.title('Distribution of Number of Observations for Inconclusive IntRelP Rules')
plt.xlabel('Observations (log)')
plt.ylabel('Frequency (log)')
plt.xscale('log')
plt.yscale('log')
plt.hist(np.array(df[df['answer'] == 'Nothing']['observations']), bins=100)
plt.show() # seems to have a higher mean (around 8) and more centered?
# distribution of observations with conclusive results
plt.title('Distribution of Number of Observations for Conclusive IntRelP Rules')
plt.xlabel('Observations (log)')
plt.ylabel('Frequency (log)')
plt.xscale('log')
plt.yscale('log')
plt.hist(np.array(df[df['answer'] != 'Nothing']['observations']), bins=100)
plt.show() # definitely left-skewed, mean is around 5