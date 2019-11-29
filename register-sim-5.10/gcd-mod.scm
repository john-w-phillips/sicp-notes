 (test-b
  (test = b 0)
  (branch gcd-done)
  (rem t a b)
  (mov a b)
  (mov b t)
  (goto test-b)
  gcd-done)
  
