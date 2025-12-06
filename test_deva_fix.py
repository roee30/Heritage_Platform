import sys
sys.path.insert(0, r'c:\Users\roee\code\clean\Heritage_Platform\python')

from heritage.modules.canon import unidevcode
from heritage.modules.word import Word
from heritage.modules.stubs import _LEXICON_CODES

# Test the conversion
idam_codes = _LEXICON_CODES.get('idam', [])
api_codes = _LEXICON_CODES.get('api', [])
print('idam codes:', idam_codes)
print('api codes:', api_codes)

w1 = Word(idam_codes)
w2 = Word(api_codes)

dev1 = unidevcode(w1.elements)
dev2 = unidevcode(w2.elements)
combined = dev1 + dev2

print('idam deva:', repr(dev1))
print('api deva:', repr(dev2))
print('combined:', repr(combined))
print('display:', combined)
