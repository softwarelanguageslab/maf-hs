class Counter:
    def __init__(self, initial_value):
        self.value = initial_value
    def increment(self):
        self.value += 1
    def decrement(self):
        self.value -= 1
    def status(self):
        return self.value

class DoubleCounter(Counter):
    def increment(self):
        self.value += 2

def f():
    ctr = DoubleCounter(100)
    ctr.increment()
    ctr.increment()
    ctr.decrement()
    return ctr.status()

f() 
