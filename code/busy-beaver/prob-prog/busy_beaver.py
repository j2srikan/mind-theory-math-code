RULES_2 = {
    'a': {0: ('b', 1, 'r'),
          1: ('b', 1, 'l')},
    'b': {0: ('a', 1, 'l'),
          1: ('h', 1, 'r')}
}

RULES_5 = {
    'a': {0: ('b', 1, 'r'),
          1: ('a', 1, 'l')},
    'b': {0: ('c', 1, 'r'),
          1: ('c', 1, 'l')},
    'c': {0: ('d', 1, 'r'),
          1: ('d', 1, 'l')},
    'd': {0: ('e', 1, 'r'),
          1: ('e', 1, 'l')},
    'e': {0: ('a', 1, 'l'),
          1: ('h', 1, 'l')}
}

class TuringMachine:
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
        current_output = self.tape[self.head]
        next_rule = RULES_5[self.state][current_output]
        self.state = next_rule[0]
        self.tape[self.head] = next_rule[1]
        self.move(next_rule[2])

    def run(self):
        while True:
            print(f'STATE: {self.state}, TAPE: {self.tape}, HEAD: {self.head}')
            if self.state == 'h':
                break
            self.act()

tm = TuringMachine()
tm.run()
