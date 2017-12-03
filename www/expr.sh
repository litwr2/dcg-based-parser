#!/bin/bash
echo Content-type: text/html
echo

echo '[hoc5x].

hoc5g.' >data
gawk --non-decimal-data '{split($0, a, "=")
s = a[2]
gsub("\\+", " ", s)
p = index(s, "%")
while (p) {
  d = sprintf("%c", int("0x"substr(s, p + 1, 2)))
  s = substr(s, 1, p - 1) d substr(s, p + 3)
  p = index(s, "%")
}
print s
}' >data2
echo '<div align=center><p>'
cat data2
echo '<hr><p>'
cat data data2 | gprolog | sed 8!d
echo '<hr></div>'
dot -Tsvg -O data.dot
mv data.dot.svg /var/www/html
echo '<p><img width=100% src=/data.dot.svg>'
