#!/usr/bin/python

import sys

print sys.argv[1]




f = open(sys.argv[1])

line_num = 1
line = f.readline()

while line:

    line = line.rstrip()
    line_out = '%(number) 3d: ' %    { "number": line_num}
    line_out += line
    print line_out

    if (line_num % 55) == 0:
        # give some indication that a page break goes here:
        print ''
        print ''
        print ''
        print ''
        print ''

    line_num += 1
    line = f.readline()


f.close()



