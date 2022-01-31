import numpy as np
import matplotlib.pyplot as plt
import torch
from pyro.infer import EmpiricalMarginal, Importance

class ProbabilisticTuringMachine:
    def __init__(self):
        self.state = 'a'
        self.tape = [0]
        self.head = 0

    def move(self, direction):
        if direction == 'l':
            self.head -=1
        elif direction == 'r':
            self.head += 1
        if self.head < 0:
            self.tape.insert(0, 0) # insert 0 in beginning of list
            self.head = 0
        if self.head >= len(self.tape):
            self.tape.append(0) # insert 0 at end of list
        
    def act(self):
        self.state = np.random.choice(['a', 'b', 'c', 'd', 'e', 'h'])
        self.tape[self.head] = 1 # always writes 1
        self.move('r') # move only in one direction

    def run(self):
        while True:
            print(f'STATE: {self.state}, TAPE: {self.tape}, HEAD: {self.head}')
            if self.state == 'h':
                break
            self.act()
        return torch.tensor(np.sum(self.tape)) # return number of 1's
            
def trial():
    tm = ProbabilisticTuringMachine()
    return tm.run()

posterior = Importance(trial, guide=None, num_samples=1000)
marginal = EmpiricalMarginal(posterior.run())
plt.hist([marginal().item() for _ in range(1000)], bins=100)
