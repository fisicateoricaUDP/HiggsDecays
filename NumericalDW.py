#!/usr/bin/python3
from numpy import append,log,pi, linspace
from cmath import sqrt
from HZYDecaywidth import HZADwidth

import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import scienceplots


BRHZA = HZADwidth(125.2)/0.0037
print("BR of H to ZA in SM at LO=",BRHZA)
BR= []

#Data

MH = linspace(93, 150, 1000)
for i in range(len(MH)):
 BR = append(BR, HZADwidth(MH[i])/0.0037 )

 
with plt.style.context(['science', 'high-vis']):

    fig= plt.figure(figsize = (8,6), dpi = 100)
    ax = fig.gca()
    plt.title(r'$\text{BR}(H \rightarrow Z\gamma) \text{ in SM at LO}$',fontsize = 19)
    plt.ylabel(r'BR', fontsize = 19)
    plt.xlabel(r'$M_H \ [\text{GeV}]$', fontsize = 19)

    plt.xlim(min(MH), max(MH))
    #plt.ylim(0.0, 1)
    
    plt.yscale("log")
    plt.plot(MH,BR, color='red')
    #plt.show()
    plt.savefig('brvsmh.pdf')
