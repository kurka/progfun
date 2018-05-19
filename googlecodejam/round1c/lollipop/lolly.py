import sys

INF = 201

def play(n):

    #prepare n flavours
    avail = set(range(n))
    histogram = {i:0 for i in avail}
    sold = set()
    
    for customer in range(n):
        customerline = list(map(int, input().split()))
        d = customerline[0]
        if d == -1:
            return  # Error!
        requests = customerline[1:]
        # d = int(input())
        # requests = list(map(int, input().split()))
        for r in requests:  # read flavours
            # add to histogram and to requests
            histogram[r] += 1
        # choose a flavour from avail and print it
        # chose the rarest one 
        # print(histogram)
        if not requests:
            print(-1)
        else:
            choice = min(requests, key=lambda r: histogram[r])
            if choice in sold:
                print(-1)
            else:
                print(choice)
            # remove from avail
            histogram[choice] = INF  # dirty but ok I guess
            sold.add(choice)
        sys.stdout.flush()
            
    return True



if __name__ == '__main__':
    for _ in range(int(input())):  # T test cases
        n = int(input())  # number of lolipops/customers
        play(n)
