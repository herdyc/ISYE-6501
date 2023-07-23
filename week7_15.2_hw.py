from pulp import *
import pandas as pd

# load the diet data
df = pd.read_excel('data 15.2/diet.xls')
print(df)
df.head()

# Only take first 64 rows since 64-66 contains NaN
data = df[0:64].values.tolist()
print(data)

# Create Foods master dictionary + Cost
foods = [x[0] for x in data]
calories = dict([(x[0], float(x[3])) for x in data])
cholesterol = dict([(x[0], float(x[4])) for x in data])
totalFat = dict([(x[0], float(x[5])) for x in data])
sodium = dict([(x[0], float(x[6])) for x in data])
carbs = dict([(x[0], float(x[7])) for x in data])
fiber = dict([(x[0], float(x[8])) for x in data])
protien = dict([(x[0], float(x[9])) for x in data])
vitaminA = dict([(x[0], float(x[10])) for x in data])
vitaminC = dict([(x[0], float(x[11])) for x in data])
calcium = dict([(x[0], float(x[12])) for x in data])
iron = dict([(x[0], float(x[13])) for x in data])
cost = dict([(x[0], float(x[1])) for x in data])

# Create the problem variable
prob1 = LpProblem('Diet_Optimization', LpMinimize)

# Define Decision Variable - Continuous
foodVars = LpVariable.dicts("foods", foods, 0)

# Define Objective Function "Total Cost of Foods per person"
prob1 += lpSum([cost[i]*foodVars[i] for i in foods])

# Define Constraints
# Hardcode Lower and Upper Bound
LBound = [1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10]
UBound = [2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40]

# Append Constraints
B = []
for j in range(0, 11):
    B.append(dict([(x[0], float(x[j + 3])) for x in data]))

for i in range(0, 11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition1 = LBound[i] <= + dot_B_x
    prob1 += condition1

for i in range(0, 11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition2 = UBound[i] >= + dot_B_x
    prob1 += condition2

prob1.solve()

# Print output status
print("Status:", LpStatus[prob1.status])

# Print foods list for Optimal Diet (Without 0 Value)
for v in prob1.variables():
    if v.varValue > 0:
        print(v.name, "=", v.varValue)

# Total Cost
print("Problem 2 - Total Cost of Foods per person = ", value(prob1.objective))

########################################################################################################################

# Create the problem variable
prob2 = LpProblem('Diet_Optimization', LpMinimize)

# Define Decision Variable - Binary (Additional)
chosenVars = LpVariable.dicts("Chosen", foods, 0, 1, "Binary")

# Define Objective Function "Total Cost of Foods per person"
prob2 += lpSum([cost[i]*foodVars[i] for i in foods])

# Define Original Constraints
for i in range(0, 11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition1 = LBound[i] <= + dot_B_x
    prob2 += condition1

for i in range(0, 11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition2 = UBound[i] >= + dot_B_x
    prob2 += condition2

# Define Constraint A (Min 0.1 serving)
for f in foods:
    prob2 += foodVars[f] <= 10000 * chosenVars[f]
    prob2 += foodVars[f] >= .1 * chosenVars[f]

# Define Constraint B (At most one, but not both Frozen Broccoli and Raw Celery)
prob2 += chosenVars['Frozen Broccoli'] + \
            chosenVars['Celery, Raw'] <= 1

# Define Constraint C (At least 3 kinds of meat/poultry/fish/eggs must be selected)
prob2 += chosenVars['Roasted Chicken'] + chosenVars['Poached Eggs'] + \
            chosenVars['Scrambled Eggs'] + chosenVars['Frankfurter, Beef'] + \
            chosenVars['Kielbasa,Prk'] + chosenVars['Hamburger W/Toppings'] + \
            chosenVars['Hotdog, Plain'] + chosenVars['Pork'] + \
            chosenVars['Bologna,Turkey'] + chosenVars['Ham,Sliced,Extralean'] + \
            chosenVars['White Tuna in Water'] \
            >= 3

prob2.solve()

# Print output status
print("Status:", LpStatus[prob2.status])

# Print foods list for Optimal Diet (Without 0 Value)
for v in prob2.variables():
    if v.varValue > 0:
        print(v.name, "=", v.varValue)

# Total Cost
print("Problem 2 - Total Cost of Foods per person = ", value(prob2.objective))