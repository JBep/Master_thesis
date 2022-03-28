import numpy as np
import pandas as pd



class Single_echelon_simulation:
    """ Simulates a single echelon inventory system.
    
    Simulates the behaviour of a single echelon inventory system during p time 
    periods. A period could be interpreted as any time interval: hours, days etc.
    
    The system is an R,Q system that places orders when its inventory position 
    reaches R and receives order with the constant lead time L. The system faces 
    demand according to a discrete stochastic process with stationary increments.
    Full backordering is used, an inventory level less than 0 represents backorders. 
    
    The demand is drawn as a discrete variable each period.
    
    """

    def __init__(self,R: int, Q: int, demand_type: str, period_demand_mu: float, period_demand_sigma: float, lead_time:int = 10, no_periods:int = 10000):
        """Initiates the object.
        
        params:
            R: reorder point.
            Q: order quantity.
            demand_type: Distribution for generating lead time demand. 
                Implemented types: 'Poisson'.
            
            period_demand_mu: expected number of demand during a period.
            period_demand_sigma: variance of demand distribution of a period.
            
            lead_time: constant order lead time, default = 10 periods.
            no_periods: simulation time, a run in of 1000 periods will be added. 
                Default: 10 000
            """
        
        # Control parameters
        self.R = R
        self.Q = Q
        self.L = lead_time
        
        # Demand related
        possible_demand_types = {'Poisson','Compound Poisson'}
        if demand_type in possible_demand_types:
            self.demand_type = demand_type
        else:
            raise ValueError(f"Possible demand types are: {possible_demand_types}")
        
        self.period_demand_mu = period_demand_mu
        self.period_demand_sigma = period_demand_sigma

        # Simulation related settings
        self.no_periods = no_periods + 1000 # 1000 is run-in time.
        self.current_IL = R+Q//2   # Will be updated with every time-step.
        self.current_in_transit = 0
        self.arriving_stock = np.zeros(self.no_periods+self.L) # Keeps track of stock arriving in period index+1.

        # Vectors too keep track of simulation results
        self.IL = np.zeros(self.no_periods)
        self.IP = np.zeros(self.no_periods)
        self.in_transit = np.zeros(self.no_periods)
        self.demand_array = self.generate_demand()

        # Related to item fill-rate
        self.total_items_demanded = 0
        self.total_items_delivered_from_stock = 0

        # Related to order fill-rate. Not yet implemented.
        self.total_orders_demanded = 0
        self.total_orders_delivered_from_stock = 0

    def generate_demand(self):
        """Generates a vector of period-demands. Drawn psuedo-randomly rom a 
            given distribution.
        """

        if self.demand_type == 'Poisson':
            demand_array = np.random.poisson(lam = self.period_demand_mu, size = self.no_periods)

        return demand_array

    def time_step(self,period):
        """Updates on the simulation run every time-step.
        
        Update schedule:
        * Incoming orders arrive.
        * Demand occurs.
        * Orders are placed.
        
        """
        print(f'Period no: {period}.')
        # Orders arrive
        arriving_stock = self.arriving_stock[period]
        self.current_IL = self.current_IL + arriving_stock
        self.current_in_transit = self.current_in_transit - arriving_stock

        # Demand occurs and service params updated
        period_demand = self.demand_array[period]
        self.total_items_demanded += period_demand
        
        if period_demand <= self.current_IL:
            self.total_items_delivered_from_stock += period_demand    
        elif period_demand > 0:
            self.total_items_delivered_from_stock += self.current_IL
            
        self.current_IL -= period_demand
        
        # Updating IP
        IP = self.current_IL + self.current_in_transit
        
        # Update historics
        self.IL[period] = self.current_IL
        self.IP[period] = IP
        self.in_transit[period] = self.current_in_transit

        # Sending orders
        if IP <= self.R:
            self.arriving_stock[period+self.L+1] = self.Q
            self.current_in_transit = self.current_in_transit + self.Q

    def run(self):
        # Run-in period
        for period in range(1000):
            self.time_step(period)

        # Reset service counters
        self.total_items_demanded = 0
        self.total_items_delivered_from_stock = 0
        self.total_orders_demanded = 0
        self.total_orders_delivered_from_stock = 0

        for period in range(1000,self.no_periods):
            self.time_step(period)

    def get_item_fill_rate(self):
        return self.total_items_delivered_from_stock/self.total_items_demanded

    def generate_dataframe(self) -> pd.DataFrame:
        """This function generates a pandas dataframe.

        The dataframe holds: Demand, IL, IP, In Transit stock and Arriving stock
        
        """

        df = pd.DataFrame()
        df['Demand'] = self.demand_array
        df['IL'] = self.IL
        df['IP'] = self.IP
        df['In transit'] = self.in_transit
        df['Arriving stock'] = self.arriving_stock[0:-self.L]
        return df


def main():
    mySim = Single_echelon_simulation(R=50,Q=100,demand_type='Poisson',
        period_demand_mu=10,period_demand_sigma = 0,lead_time = 5, no_periods = 10000)
    mySim.run()

    #mySim.generate_dataframe().to_excel("myDataFrame.xlsx")
    print(f'The item fill rate is: {mySim.get_item_fill_rate()}')


if __name__ == "__main__":
    main()
   





    