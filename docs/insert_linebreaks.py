#!/usr/bin/python

f = open('word2010.xml')
slurped_string = f.read()
f.close()

IGNORE_01 = '<w:t>'
IGNORE_02 = '<w:t xml:space="preserve">'
IGNORE_03 = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
IGNORE_04 = '\r\n<?mso-application progid="Word.Document"?>'

last_b = 0
loc_a = slurped_string.find('<', last_b)
loc_b = slurped_string.find('>', last_b)
result = ''

while loc_b > 0:

    curr_token = slurped_string[loc_a:loc_b+1]

    if curr_token == IGNORE_01 or curr_token == IGNORE_02 or curr_token == IGNORE_03 or curr_token == IGNORE_04:
        result += curr_token
    else:
        result += curr_token + '\r\n'

    loc_a = loc_b+1
    last_b = loc_b+1
    loc_b = slurped_string.find('>', last_b)


if ord(result[len(result)-1]) == 10:
    result = result[0:len(result)-1]

print result
