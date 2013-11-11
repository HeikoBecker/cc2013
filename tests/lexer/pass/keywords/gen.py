import os
import sys

nameFile = 'keyword';

path = os.path.abspath(__file__)
pos = path.rfind('/')
path = path[0:pos]

os.system('rm %s/*.in' % path)
os.system('rm %s/*.out' % path)
os.system('rm %s/*.expect' % path)

expectFile = open('%s/%s.expect' %(path, nameFile), 'w')
inputFile = open('%s/%s.in' %(path, nameFile), 'w')

keywords = ['auto', 'break', 'case', 'char', 'const', 'continue', 'default',
'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if',
'inline', 'int', 'long', 'register', 'restrict', 'return', 'short', 'signed',
'sizeof', 'static', 'struct', 'switch', 'typedef', 'union', 'unsigned', 'void',
'volatile', 'while', '_Alignas', '_Alignof', '_Atomic', '_Bool', '_Complex',
'_Generic', '_Imaginary', '_Noreturn', '_Static_assert', '_Thread_local'];

print ('Generating %s.in and %s.expect' % (nameFile, nameFile))
for word in keywords:
  inputFile.write('%s\n' % word)
  expectFile.write(': keyword %s\n' % word)

inputFile.close()
