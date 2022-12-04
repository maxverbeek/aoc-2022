import sys

def getnums(pair):
    [num1, num2] = pair.split('-')
    return (int(num1), int(num2))

for line in sys.stdin:
    [pair1, pair2] = line.split(',')
    num1, num2 = getnums(pair1)
    num3, num4 = getnums(pair2)

    print(f"{num1:03}-{num2:03},{num3:03},{num4:03}")

