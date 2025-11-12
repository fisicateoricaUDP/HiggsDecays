from numpy import log,pi
from cmath import sqrt
from sympy.parsing.mathematica import mathematica
I = 0+1j
def HZADwidth(mh):

    DW= mathematica("(3.7723593084608705*10^-13*((1.931416686334768*10^9 - 3.594324067634323*10^-7*I)*mh^2 + (29494.182177401926 + 9.767737830978035*10^-12*I)*mh^4 + 1*mh^6 + 8313.792400000002*(85414.87890890405 + 1*mh^2)*Sqrt[-25830.9184*mh^2 + mh^4]*Log[0.00007742659277650772*(12915.4592 - mh^2 + Sqrt[-25830.9184*mh^2 + mh^4])] + (9.60335200640064*10^8*mh^2 - 42707.439454452026*mh^4)*Log[0.00007742659277650772*(12915.4592 - mh^2 + Sqrt[-25830.9184*mh^2 + mh^4])]^2)^2)/(mh^7*(-8313.792400000002 + mh^2))")
    DW2 = eval(str(DW)).real
    return DW2  

