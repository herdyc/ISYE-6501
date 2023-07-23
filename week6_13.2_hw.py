import simpy
import random
import statistics

RANDOM_SEED = 65
NUM_ID_CHECK = 20
NUM_SCAN = 20
MEAN_ID_TIME = .75  # Mean check time (Exponential distribution)
SCAN_PARAM = [.5, 1]  # Time for uniform distribution (Minute)
PASS_ARR = .02  # Mean interval rate (Poisson)

wait_times = []

class Airport(object):
    def __init__(self, env, num_id_check, num_scan, mean_id_time, scan_param):
        self.env = env
        self.IDcheck = simpy.Resource(env, num_id_check)
        self.Scan = simpy.Resource(env, num_scan)
        self.mean_id_time = mean_id_time
        self.scan_param = scan_param

    def check_ID(self, passenger):
        rand_ID_time = random.expovariate(MEAN_ID_TIME)
        yield self.env.timeout(rand_ID_time)
        # print('Checked Id in {}'.format(round(rand_ID_time,2)))

    def scan_pass(self, passenger):
        rand_scan_time = random.uniform(self.scan_param[0], self.scan_param[1])
        yield self.env.timeout(rand_scan_time)
        # print('Scanned passenger in {}'.format(round(rand_scan_time,2)))

def go_through_security(env, passenger, airport):
    # passenger arrives
    arrival = env.now

    # goes through ID check and scan
    with airport.IDcheck.request() as request:
        yield request
        yield env.process(airport.check_ID(passenger))

    with airport.Scan.request() as request:
        yield request
        yield env.process(airport.scan_pass(passenger))

    # calc wait time
    wait_times.append(env.now - arrival)

def run_airport(env, num_id_check, num_scan, mean_id_time, scan_param):
    airport = Airport(env, num_id_check, num_scan, mean_id_time, scan_param)

    for passenger in range(1):
        env.process(go_through_security(env, passenger, airport))

    while True:
        yield env.timeout(1 / (random.expovariate(PASS_ARR)))

        passenger += 1
        env.process(go_through_security(env, passenger, airport))


def get_average_wait_time(wait_times):
    average_wait = statistics.mean(wait_times)
    return average_wait

def main(num_id_check=NUM_ID_CHECK, num_scan=NUM_SCAN):
    random.seed(65)

    # Simulation
    env = simpy.Environment()
    env.process(run_airport(env, num_id_check, num_scan, MEAN_ID_TIME, SCAN_PARAM))
    env.run(until=600)

    return get_average_wait_time(wait_times)

# setting up multiple test cases
numid = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
numscan = []
for i in range(1, 14):
    numscan = numscan + ([i] * 13)
numid = numid * 13

resultStore = [[] for i in range(len(numid))]

for i in range(len(numscan)):
    wait_times = []

    if main(numid[i], numscan[i]) <= 15.0:
        print('\n', 'AVG wait with %d ID queues and %d scanners is %.5f minutes' % (numid[i], numscan[i], main(numid[i], numscan[i])))
        main(numid[i], numscan[i])
        resultStore[i].append(numid[i])
        resultStore[i].append(numscan[i])
        resultStore[i].append(main(numid[i], numscan[i]))

finalResultStore = [x for x in resultStore if x]
finalResultStore.sort(key = lambda i: i[2])

for i in range(len(finalResultStore)):
    print(finalResultStore[i])
