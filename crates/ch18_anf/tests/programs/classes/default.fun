data Flag = On | Off
instance Eq Flag where
  On == On = True
  Off == Off = True
  _ == _ = False
_old = On /= Off

main = print _old
