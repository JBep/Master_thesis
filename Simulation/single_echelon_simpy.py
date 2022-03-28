

import simpy

class Single_echelon_simpy_simulation:

    def __init__(self,env):
        self.env = env
        self.action = env

    def run(self):
        while True:
            yield self.env.process(self.customer_arrival_interval())
            print(f'Customer arriving at {self.env.now}')


    def customer_arrival_interval(self):
        yield self.env.timeout(5)

mySim = Single_echelon_simpy_simulation(simpy.Environment)
mySim.run()