import subprocess
import os
import sys

def diff(p):
  inF = '%s.out' % p
  outF = '%s.expect' % p

  print ('comparing %s %s' % (inF, outF))

  inF = open(inF, 'r').readlines()
  outF = open(outF, 'r').readlines()

  if len(inF) <> len(outF):
    print "FAIL -- different size"
    return False
  else:
    for w in range(len(inF)):
      left = inF[w][inF[w].rfind(':'):-1]
      right = outF[w][outF[w].rfind(':'):-1]

      if left <> right:
        print ('FAIL -- different lines in %s' % inF)
        print ('LEFT  : %s' % left)
        print ('Right : %s' % right)
        return False;
        
    return True

path = os.path.abspath(__file__)
pos = path.rfind('/')
path = path[0:pos]

program = '../../../build/default/c4 --tokenize' 

files = subprocess.check_output('ls -F'.split()).split('\n')

print ('getting the files ')

for word in files:
  if '/' in word:
    print('testing %s' % word)
    os.system('python %s/%s/gen.py' % (path, word))
    for path, dirs, files in os.walk('%s/%s/' % (path, word)):
      for f in files:
        if f.endswith('.in'):
          f = f[:-3]
          inFile = '%s%s.in' % (path[:-1], f)
          outFile = '%s%s.out' % (path[:-1], f)

          os.system('%s %s >>%s' % (program, inFile, outFile))
          if (not (diff('%s%s' % (path[:-1],f)))) :
            sys.exit('== FAIL TEST ERRORS ==')

print ('== SUCCESS ALL TESTS PASSED ==')
