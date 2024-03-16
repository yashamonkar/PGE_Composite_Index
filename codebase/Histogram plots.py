# -*- coding: utf-8 -*-
"""
Created on Mon Jan 15 18:28:52 2024

@author: Yash Amonkar
"""

#Import Libraries
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random


#Set the working directory. 
directory_path = 'C:/Users/Yash Amonkar/Documents/GitHub/PGE_Composite_Index'
os.chdir(directory_path)


#-----------------------------------------------------------------------------#
#Input the revenue and pauout slides
revenues = pd.read_csv('sims/All_revenues_no_tax.csv')
payouts = pd.read_csv('sims/All_payouts_no_tax.csv')


#_____________________________________________________________________________#
#Histograms of Revenues

fig = plt.figure(constrained_layout=True, figsize = (15,8))
gs = fig.add_gridspec(1, 2, height_ratios = [1], width_ratios=[1,1], hspace=0.1,wspace=0.05)

ax1 = fig.add_subplot(gs[0,0])
ax2 = fig.add_subplot(gs[0,1])


n, bins, _ =ax1.hist(revenues['Unmanaged'], bins=20, color='tab:blue', label='Unmanaged Net Revenue')
ax1.hist(revenues['Composite_index'], alpha=1.0, bins = bins ,label='Composite Index Revenue',linestyle='dashed', linewidth=2, edgecolor='k', hatch='///',histtype='step')
ax1.axvline(revenues['Unmanaged'].min())
ax1.axvline(revenues['Composite_index'].min(), color = 'k')
ax1.tick_params(labelsize=16)
ax1.set_xlabel('Revenues ($B)', fontsize=18)
ax1.set_ylabel('Frequency', fontsize=18)
ax1.legend(fontsize=18, loc='upper center', bbox_to_anchor=(0.5, -0.15), ncol=2)
ax1.text(-0.1, 1.05, 'A', transform=ax1.transAxes, size=20, weight='bold')


ax2.hist(revenues['Unmanaged'], bins=bins, color='tab:blue', label='Unmanaged Net Revenue')
ax2.hist(revenues['Portfolio'], alpha=1.0, bins = bins,label='Portfolio of Contracts Revenue' ,linestyle='dashed', linewidth=2, edgecolor='k', hatch='///',histtype='step')
ax1.axvline(revenues['Unmanaged'].min())
ax2.axvline(revenues['Portfolio'].min(), color = 'k')
ax2.tick_params(labelsize=16)
ax2.set_xlabel('Revenues ($B)', fontsize=18)
ax2.set_ylabel('Frequency', fontsize=18)
ax2.legend(fontsize=18)
ax2.text(-0.1, 1.05, 'B', transform=ax2.transAxes, size=20, weight='bold')


#%%
#Histograms of Payouts

d = .015

fig, axs = plt.subplots(2, 2, sharex=True, figsize = (12,8))
fig.text(0.5, -0.02, 'Payouts ($ billion)', ha='center', fontsize=20)
fig.text(-0.01, 0.4, 'Frequency (Years)',rotation=90,fontsize=20)


n, bins, _ = axs[0,0].hist(payouts['Unmanaged'], bins=20, color='tab:blue', label='Unmanaged Losses')
bin_width = bins[1] - bins[0]
width = bin_width

axs[0,0].hist(payouts['Composite_index'], alpha=1.0, bins=np.arange(min(payouts['Composite_index']), max(payouts['Composite_index']) + width, width),color='darkorange',label='Composite Payout',linestyle='dashed', linewidth=2, edgecolor='k', hatch='///',histtype='step')
axs[0,0].tick_params(labelsize=16)
axs[0,0].set_ylim(300, 500)
axs[0,0].spines['bottom'].set_visible(False)
axs[0,0].xaxis.tick_top()
axs[0,0].tick_params(labeltop=False)  # don't put tick labels at the top
kwargs = dict(transform=axs[0,0].transAxes, color='k', clip_on=False)
axs[0,0].plot((-d, +d), (-d, +d), **kwargs)  # top-left diagonal
axs[0,0].plot((1 - d, 1 + d), (-d, +d), **kwargs)  # top-right diagonal
# axs[0,0].set_ylim(800,825)


axs[1,0].hist(payouts['Unmanaged'], bins=20, color='tab:blue')
axs[1,0].hist(payouts['Composite_index'], alpha=1.0, bins=np.arange(min(payouts['Composite_index']), max(payouts['Composite_index']) + width, width),color='darkorange',linestyle='dashed', linewidth=2, edgecolor='k', hatch='///',histtype='step')
# axs[1,0].set_ylim(0, 625)
axs[1,0].set_ylim(0, 100)
axs[1,0].tick_params(labelsize=16)

axs[1,0].spines['top'].set_visible(False)

axs[1,0].xaxis.tick_bottom()
  # how big to make the diagonal lines in axes coordinates
# arguments to pass to plot, just so we don't keep repeating them
kwargs.update(transform=axs[1,0].transAxes)  # switch to the bottom axes
axs[1,0].plot((-d, +d), (1 - d, 1 + d), **kwargs)  # bottom-left diagonal
axs[1,0].plot((1 - d, 1 + d), (1 - d, 1 + d), **kwargs)  # bottom-right diagonal
# ax.set_ylim(.78, 1.)  # outliers only
# ax2.set_ylim(0, .22)  # most of the data

n, bins, _ = axs[0, 1].hist(payouts['Unmanaged'], bins=20)
bin_width = bins[1] - bins[0]
width = bin_width
axs[0,1].tick_params(labelsize=16)
axs[0,1].hist(payouts['Portfolio'], alpha=1.0, bins=np.arange(min(payouts['Portfolio']), max(payouts['Portfolio']) + width, width),color='brown',label='Portfolio Payout', linestyle='dashed', linewidth=2, edgecolor='k',hatch='...',histtype='step')
axs[1,1].hist(payouts['Unmanaged'], bins=20)
axs[1,1].hist(payouts['Portfolio'], alpha=1.0,bins=np.arange(min(payouts['Portfolio']),max(payouts['Portfolio']) + width, width), color='brown', linestyle='dashed', linewidth=2, edgecolor='k',hatch='...',histtype='step')
# axs[1,1].set_ylim(0, 625)
axs[1, 1].set_ylim(0, 100)
axs[1,1].tick_params(labelsize=16)
# axs[0,1].set_ylim(800,825)
axs[0,1].set_ylim(300, 500)
# hide the spines between ax and ax2
axs[0,1].spines['bottom'].set_visible(False)
axs[1,1].spines['top'].set_visible(False)
axs[0,1].xaxis.tick_top()
axs[0,1].tick_params(labeltop=False)  # don't put tick labels at the top
axs[1,1].xaxis.tick_bottom()
d = .015  # how big to make the diagonal lines in axes coordinates
# arguments to pass to plot, just so we don't keep repeating them
kwargs = dict(transform=axs[0,1].transAxes, color='k', clip_on=False)
axs[0,1].plot((-d, +d), (-d, +d), **kwargs)  # top-left diagonal
axs[0,1].plot((1 - d, 1 + d), (-d, +d), **kwargs)  # top-right diagonal

kwargs.update(transform=axs[1,1].transAxes)  # switch to the bottom axes
axs[1,1].plot((-d, +d), (1 - d, 1 + d), **kwargs)  # bottom-left diagonal
axs[1,1].plot((1 - d, 1 + d), (1 - d, 1 + d), **kwargs)  # bottom-right diagonal

lines_labels = [ax.get_legend_handles_labels() for ax in fig.axes]
lines, labels = [sum(lol, []) for lol in zip(*lines_labels)]
axs[0,1].legend(lines, labels,prop={'size': 18},loc='upper right')

#Add Labels
axs[0,0].text(0.05, 0.9, 'A', transform=axs[0,0].transAxes, size=24, weight='bold')
axs[0,1].text(0.05, 0.9, 'B', transform=axs[0,1].transAxes, size=24, weight='bold')


axs[1,1].annotate('Overpayments', 
                  xy=(0.1, 20), xycoords='data',
                  xytext=(0.2, 100), textcoords='data',
                  arrowprops=dict(arrowstyle="->", linewidth=2,
                                  connectionstyle="arc3"),
                  fontsize=18, 
                  bbox=dict(boxstyle="round,pad=0.3", edgecolor='black', facecolor='white'))

# Ensure that the added annotation does not cut off
#plt.tight_layout()

plt.tight_layout()
#plt.savefig('Plots/Losses.png' , bbox_inches='tight',dpi=250)
plt.show()




#%%
#Time Series -- Just Portfolio and Composite -- All years

actual_loss_array = payouts['Unmanaged']
yearly_payouts = payouts['Composite_index']
individual_instruments_separated = payouts['Portfolio']

#Selected sub-plot
#ns = [222, 21, 209, 188, 401, 261, 213, 296, 109, 497, 225, 263, 155, 176, 17, 290, 285, 80, 436, 172]
#ns = [x - 1 for x in ns]

#actual_loss_array = actual_loss_array[ns]
#yearly_payouts = yearly_payouts[ns]
#individual_instruments_separated = individual_instruments_separated[ns]

fig, ax1 = plt.subplots(figsize = (20,8))
# lower_bound = 240
# upper_bound = 300
lower_bound = 0
upper_bound = (len(individual_instruments_separated)-1)
years = np.arange(len(individual_instruments_separated))

plt.title("Net Revenue Loss and Instrument Payouts", fontsize=18)
color = 'tab:blue'
ax1.set_xlabel('years',fontsize=16)
ax1.set_ylabel('Net Revenue Loss (billions $)', fontsize=16)
ax1.tick_params(axis='both', which='major', labelsize=12)
ax1.tick_params(axis='both', which='minor', labelsize=12)
ax1.plot(years, -actual_loss_array,'-.', color=color, label='Loss',linewidth=2)
ax1.tick_params(axis='y')
ax1.set_ylim([-0.5, 0.5])

# individual_instruments_separated = payout

ax2 = ax1.twinx()  # instantiate a second axes that shares the same x-axis
ax2.set_ylabel('Index Loss (billions $)', rotation=270, labelpad=20,fontsize=16)
ax2.tick_params(axis='both', which='major', labelsize=12)
ax2.bar(years + 0.25 , yearly_payouts, color='brown', label='Composite Index Loss', width = 1, alpha = 0.75)  # LW = 2
# ax2.bar(years + 0.25, (individual_instruments_separated['CDD']), label='CDD Index loss', color='gray', hatch='///', width = 0.5, alpha = 0.75)  # change these to individual instruments
ax2.bar(years - 0.25, individual_instruments_separated, label='Portfolio of Indices loss', color='gray', hatch='///', width = 1, alpha = 0.75)
ax2.tick_params(axis='y')
ax2.set_ylim([-0.2, 0.2])

fig.tight_layout()
h1, l1 = ax1.get_legend_handles_labels()
h2, l2 = ax2.get_legend_handles_labels()
plt.legend(h1+h2, l1+l2, loc=2, prop={'size': 12})
plt.show()



#%%
#Time Series of Payouts

actual_loss_array = payouts['Unmanaged']
yearly_payouts = payouts['Composite_index']
individual_instruments_separated = payouts['Portfolio']
streamflow_payouts = payouts['Streamflow']
CDD_payouts = payouts['CDD']
NG_Payouts = payouts['NG']

#Selected sub-plot
ns = random.sample(range(1, 500), 30)


actual_loss_array = actual_loss_array[ns]
yearly_payouts = yearly_payouts[ns]
individual_instruments_separated = individual_instruments_separated[ns]
streamflow_payouts = streamflow_payouts[ns]
CDD_payouts = CDD_payouts[ns]
NG_Payouts = NG_Payouts[ns]

fig, ax1 = plt.subplots(figsize = (20,8))
# lower_bound = 240
# upper_bound = 300
lower_bound = 0
upper_bound = (len(individual_instruments_separated)-1)
years = np.arange(len(individual_instruments_separated))

plt.title("Net Revenue Loss and Instrument Payouts", fontsize=18)
color = 'tab:blue'
ax1.set_xlabel('years',fontsize=16)
ax1.set_ylabel('Net Revenue Loss (billions $)', fontsize=16)
ax1.tick_params(axis='both', which='major', labelsize=12)
ax1.tick_params(axis='both', which='minor', labelsize=12)
ax1.plot(years, -actual_loss_array,'-.', color=color, label='Loss',linewidth=2)
ax1.tick_params(axis='y')
ax1.set_ylim([-0.5, 0.5])
#ax1.set_xlim([0, 100])

# individual_instruments_separated = payout

ax2 = ax1.twinx()  # instantiate a second axes that shares the same x-axis
ax2.set_ylabel('Index Loss (billions $)', rotation=270, labelpad=20,fontsize=16)
ax2.tick_params(axis='both', which='major', labelsize=12)
ax2.bar(years, yearly_payouts, color='brown', label='Composite Index Loss', width = 0.25, alpha = 0.75)  # LW = 2
ax2.bar(years - 0.25, streamflow_payouts, label='Streamflow Payout', color='blue', hatch='///', width = 0.25, alpha = 0.75)
ax2.bar(years + 0.25, CDD_payouts, label='CDD Payout', color='gray', hatch='///', width = 0.25, alpha = 0.75)
ax2.bar(years - 0.5, NG_Payouts, label='Natural Payout', color='green', hatch='///', width = 0.25, alpha = 0.75)

ax2.tick_params(axis='y')
ax2.set_ylim([-0.5, 0.5])

fig.tight_layout()
h1, l1 = ax1.get_legend_handles_labels()
h2, l2 = ax2.get_legend_handles_labels()
plt.legend(h1+h2, l1+l2, loc=2, prop={'size': 12})
plt.show()




