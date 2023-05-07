# TODO add more stuff when bored :)

def factorial(n: int) -> int:
  if (n == 1):
     return 1
  return n * factorial(n - 1)

def tail_recursed_factorial(n: int, result = 1) -> int:
  if (n == 1):
      return result
  return tail_recursed_factorial(n - 1, n * result)

def iterative_factorial(n: int) -> int:
  result = 1
  for i in range(2, n + 1):
     result *= i
  return result

def main() -> None:
  print(tail_recursed_factorial(100000))

main()
