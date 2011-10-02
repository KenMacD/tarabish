# -*- mode: python -*-
a = Analysis([os.path.join(HOMEPATH,'support\\_mountzlib.py'), os.path.join(HOMEPATH,'support\\useUnicode.py'), 'tarabish.py'],
             pathex=['c:\\qt.win'])
pyz = PYZ(a.pure)
exe = EXE( pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas + [('cards.svg', 'cards.svg', 'DATA'), ('pips.svg', 'pips.svg', 'DATA')],
          name=os.path.join('dist', 'tarabish.exe'),
          debug=False,
          strip=False,
          upx=True,
          console=True )
