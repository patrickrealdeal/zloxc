def fib(n)
  return n if n < 2
  fib(n - 2) + fib(n - 1)
end

start = Time.now
puts fib(30)
puts Time.now - start

