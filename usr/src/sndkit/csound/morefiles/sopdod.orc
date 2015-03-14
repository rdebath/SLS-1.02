; chowning soprano from the dodge collection

sr = 16000
kr = 1000
ksmps = 16
nchnls = 1

instr 1
        i7=cpspch(p4)
        i2=p6
        i2=(i2/i7)+.5
        i2=int(i2)
        i3=p7
        i4=p8
        i5=p9*.4
        i6=p10
        i8=i5/i4
        i9=exp(1.5*log(p5/32767))
        i10=.00311*log(i7)
        i11=sqrt(p5/32767)
        i12=(1-i3)*i11
        i13=i4*i7
        i14=i9*i3

                a1 linen i12,.1,p3,.08
                a2 oscil i13,i7,3
                a8=i7+a2
                a4 linen i10,.6,p3,.1
                a4 oscil a4,i6,3
                a5 randi i10,15
                a6 linseg -.03,.07,.03,.03,0,p3-.1,0
                a6=a4+a5+a6+1.
                a1 oscili a1,(a8+a2)*a6,3
                a7=(a2*i8)+i2
                a3 linseg 0,.07,.1,.03,1.,p3-.18,1,.02,.1,.06,0
                a3=a3*(i9*i3)
                a3 oscili a3,a7*a6,3

                out (a1+a3)*p5

endin
