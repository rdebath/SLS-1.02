; Generate 240 seconds of endless glissando with a 120 second cycle time
; Function 03 reads soundin.6 the bell shaped curve specified by Risset in Dodge

f01     0       512     10      1
f02     0       513     7       0       512     1
f03	0	2049	1	6	0
f04     0       513     5       1       512     .0009766

i01	0	240	10000	5000	120

e
