# zero, one or more function definitions at the beginning of the file
def fibaux(a, b, k):
    if k == 0:
        return a
    else:
        return fibaux(b, a+b, k-1)

def fib(n):
    return fibaux(0, 1, n)

# one or more statements at the end of the file
print("some values of the Fibonacci sequence:")
for n in [0, 1, 11, 42]:
    print(fib(n))

a = [1,[2,3]+[4]][1] + [5]
a[-1] = a[-1] * 2
print(a)
