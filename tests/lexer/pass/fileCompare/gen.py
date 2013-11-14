import os
import sys

nameFile = 'keyword';

path = os.path.abspath(__file__)
pos = path.rfind('/')
path = path[0:pos]
os.system('rm -f %s/*.in' % path)
os.system('rm -f %s/*.out' % path)
os.system('rm -f %s/*.expect' % path)
os.system("ls %s | grep '\.c'| sed 's/.c//' | xargs -I% sh -c 'cp %s/%.c %s/%.in' ".replace('%s',path));
os.system("ls %s | grep '\.c'| sed 's/.c//' | xargs -I% sh -c 'cp %s/%.e %s/%.expect' ".replace('%s',path));
