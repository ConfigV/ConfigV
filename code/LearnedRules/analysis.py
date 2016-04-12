import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def show_yes_and_no_dist(df):
    print("number of observations: " + str(df.size))
    print("mean number of yes " + str(np.mean(df['yes'])))
    print("median number of yes " + str(np.median(df['yes'])))
    print("mean number of no " + str(np.mean(df['no'])))
    print("median number of no " + str(np.median(df['no'])))
    plt.hist(np.array(df['yes']), bins=100)
    plt.show()
    plt.hist(np.array(df['no']), bins=100)
    plt.show()

'''
It looks like for Missing value rules, since most of them carry perfect
100% probabilities (mostly 1 yes 0 no), the ones that count are the ones with 
an abnormally high observation counts (way tail of the distribution).

This benchmark too the top three with the highest number of observations, but
it still remains to be seen how accurate the other high-observation rules are.
'''

'''
df = pd.read_csv('MissingP.psv', delimiter='|')
show_yes_and_no_dist(df)
# Looking at the distribution of yes counts past the 'peak' in the middle
print("Distribution of rules with more than 5 observations")
df = df[df['yes'] > 5]
show_yes_and_no_dist(df)
'''

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

'''
df = pd.read_csv('OrderP.psv', delimiter='|')

# See where the probabilities and the non-100% probabilities are
df['prob'] = df['yes'] / (df['yes'] + df['no'])
plt.hist(df['prob'], bins=100)
plt.show()
plt.hist(np.array((df[df['prob'] < 1.0])['prob']), bins=100)
plt.show()

# See where the total number of observations are for rules marked valid or not
df['observations'] = df['yes'] + df['no']
plt.hist(np.array(df[df['valid']]['observations']), bins=100)
plt.show()
plt.hist(np.array(df[~df['valid']]['observations']), bins=100)
plt.show()
# and maybe the validity of rules with more than 1 observation?
plt.hist(np.array(df[df['observations'] > 1]['valid']), bins=100)
plt.show() # so it is entirely false... if we have more than 1 observation...

# what are the distributions of no's and yes's
show_yes_and_no_dist(df)
print("Distribution for valid rules")
show_yes_and_no_dist(df[df['valid']])
print("Distribution for invalid rules")
print(df[~np.array(df['valid'])]['yes'])
show_yes_and_no_dist(df[~np.array(df['valid'])])
'''