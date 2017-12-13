
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1

fibo n = third (n-1) (n-2)

third a b = fibo a + fibo b
