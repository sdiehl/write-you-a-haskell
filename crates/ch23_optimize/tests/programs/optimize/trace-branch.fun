-- stderr: exact
main = print (if True then trace "taken" 42 else trace "untaken" 0)
