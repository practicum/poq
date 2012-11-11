#!/usr/bin/python

"""
      <div class="element">
        <h3>My Table Title</h3>
        <table>
          <thead>
            <tr>
              <th>s_id</th>
              <th>s_name</th>
              <th>d_id</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td>1</td>
              <td>Alice</td>
              <td>2</td>
            </tr>
            <tr>
              <td>2</td>
              <td>Bob</td>
              <td>3</td>
            </tr>
            <tr>
              <td>3</td>
              <td>Miss. Carol Sissippi</td>
              <td>2</td>
            </tr>
          </tbody>
        </table>
      </div>



+----------+----------------+------------------+
| order | product        | quantity |
+----------+----------------+------------------+
|        1 | brush     |                1 |
|        1 | soap       |                3 |
|        2 | book     |                2 |
|        2 | pencil |                1 |
|        3 | pen    |                4 |
|        4 | hat    |                2 |
+----------+----------------+------------------+

"""
state = 'none'
final_out = ''


def none_state( textline ):
    global state, final_out
    #print 'none: ' + textline.rstrip()
    if len(textline) > 0 and textline[0] == '+':
        state = 'header'
        final_out += """      <div class="element">
        <h3>My Table Title</h3>
        <table>
          <thead>
            <tr>
"""


def header_state( textline ):
    global state, final_out
    #print 'header: ' + textline.rstrip()
    if len(textline) > 0 and textline[0] == '+':
        state = 'body'
    else:
        words = textline.split('|')
        for w in words:
            if w.strip() != '':
                final_out +="              <th>"
                final_out += w.strip()
                final_out += "</th>\n"

        final_out += """            </tr>
          </thead>
          <tbody>
"""


def body_state( textline ):
    global state, final_out
    #print 'body: ' + textline.rstrip()
    if len(textline) > 0 and textline[0] == '+':
        state = 'none'
        final_out += """          </tbody>
        </table>
      </div>
"""
    else:

        final_out += "            <tr>\n"

        words = textline.split('|')
        for w in words:
            if w.strip() != '':
                final_out += "              <td>"
                final_out += w.strip()
                final_out += "</td>\n"

        final_out += "            </tr>\n"





f = open('workpad.txt')

line = f.readline()

while line:


    if state == 'none':
        none_state( line )
    elif state == 'header':
        header_state( line )
    elif state == 'body':
        body_state( line )

    line = f.readline()


f.close()



print final_out

