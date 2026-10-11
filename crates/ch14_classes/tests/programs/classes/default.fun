data Flag = On | Off
instance Eq Flag where
  On == On = True
  Off == Off = True
  _ == _ = False
main = On /= Off
