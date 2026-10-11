infixr 5 %%
a %% b = a * 10 + b
_old = 1 %% 2 %% 3

main = print _old
