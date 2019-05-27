#!/usr/bin/env python
# reducer.py

import sys
 
# associates URLs with to the term frequency counts
frequencyCount = {}
 
# input comes from STDIN
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
 
    try:
        # parse the input we got from mapper.py
        # url, col1, col2 = line.split(',')

        url, iphone, samsunggalaxy, sonyxperia, nokialumina, htcphone, ios, googleandroid, iphonecampos, samsungcampos, sonycampos, nokiacampos, htccampos, iphonecamneg, samsungcamneg, sonycamneg, nokiacamneg, htccamneg, iphonecamunc, samsungcamunc, sonycamunc, nokiacamunc, htccamunc, iphonedispos, samsungdispos, sonydispos, nokiadispos, htcdispos, iphonedisneg, samsungdisneg, sonydisneg, nokiadisneg, htcdisneg, iphonedisunc, samsungdisunc, sonydisunc, nokiadisunc, htcdisunc, iphoneperpos, samsungperpos, sonyperpos, nokiaperpos, htcperpos, iphoneperneg, samsungperneg, sonyperneg, nokiaperneg, htcperneg, iphoneperunc, samsungperunc, sonyperunc, nokiaperunc, htcperunc, iosperpos, googleperpos, iosperneg, googleperneg, iosperunc, googleperunc = line.split(',')

        # convert columns (currently strings) to ints
        iphone = int(iphone)
        samsunggalaxy = int(samsunggalaxy)
        sonyxperia = int(sonyxperia)
        nokialumina = int(nokialumina)
        htcphone = int(htcphone)
        ios = int(ios)
        googleandroid = int(googleandroid)
        iphonecampos = int(iphonecampos)
        samsungcampos = int(samsungcampos)
        sonycampos = int(sonycampos)
        nokiacampos = int(nokiacampos)
        htccampos = int(htccampos)
        iphonecamneg = int(iphonecamneg)
        samsungcamneg = int(samsungcamneg)
        sonycamneg = int(sonycamneg)
        nokiacamneg = int(nokiacamneg)
        htccamneg = int(htccamneg)
        iphonecamunc = int(iphonecamunc)
        samsungcamunc = int(samsungcamunc)
        sonycamunc = int(sonycamunc)
        nokiacamunc = int(nokiacamunc)
        htccamunc = int(htccamunc)
        iphonedispos = int(iphonedispos)
        samsungdispos = int(samsungdispos)
        sonydispos = int(sonydispos)
        nokiadispos = int(nokiadispos)
        htcdispos = int(htcdispos)
        iphonedisneg = int(iphonedisneg)
        samsungdisneg = int(samsungdisneg)
        sonydisneg = int(sonydisneg)
        nokiadisneg = int(nokiadisneg)
        htcdisneg = int(htcdisneg)
        iphonedisunc = int(iphonedisunc)
        samsungdisunc = int(samsungdisunc)
        sonydisunc = int(sonydisunc)
        nokiadisunc = int(nokiadisunc)
        htcdisunc = int(htcdisunc)
        iphoneperpos = int(iphoneperpos)
        samsungperpos = int(samsungperpos)
        sonyperpos = int(sonyperpos)
        nokiaperpos = int(nokiaperpos)
        htcperpos = int(htcperpos)
        iphoneperneg = int(iphoneperneg)
        samsungperneg = int(samsungperneg)
        sonyperneg = int(sonyperneg)
        nokiaperneg = int(nokiaperneg)
        htcperneg = int(htcperneg)
        iphoneperunc = int(iphoneperunc)
        samsungperunc = int(samsungperunc)
        sonyperunc = int(sonyperunc)
        nokiaperunc = int(nokiaperunc)
        htcperunc = int(htcperunc)
        iosperpos = int(iosperpos)
        googleperpos = int(googleperpos)
        iosperneg = int(iosperneg)
        googleperneg = int(googleperneg)
        iosperunc = int(iosperunc)
        googleperunc = int(googleperunc)
     
        columnTuple = (iphone, samsunggalaxy, sonyxperia, nokialumina, htcphone, ios, googleandroid, iphonecampos, samsungcampos, sonycampos, nokiacampos, htccampos, iphonecamneg, samsungcamneg, sonycamneg, nokiacamneg, htccamneg, iphonecamunc, samsungcamunc, sonycamunc, nokiacamunc, htccamunc, iphonedispos, samsungdispos, sonydispos, nokiadispos, htcdispos, iphonedisneg, samsungdisneg, sonydisneg, nokiadisneg, htcdisneg, iphonedisunc, samsungdisunc, sonydisunc, nokiadisunc, htcdisunc, iphoneperpos, samsungperpos, sonyperpos, nokiaperpos, htcperpos, iphoneperneg, samsungperneg, sonyperneg, nokiaperneg, htcperneg, iphoneperunc, samsungperunc, sonyperunc, nokiaperunc, htcperunc, iosperpos, googleperpos, iosperneg, googleperneg, iosperunc, googleperunc)

        if url in frequencyCount:
            frequencyCount[url] = tuple(sum(t) for t in zip(frequencyCount[url], columnTuple))
        else:
            frequencyCount[url] = columnTuple

    except Exception as e:
        continue

# write the tuples to stdout
# Note: they are unsorted
for url in frequencyCount.keys():
    print '%s, %s' % (url, str(frequencyCount[url]).rstrip('\)').lstrip('\('))