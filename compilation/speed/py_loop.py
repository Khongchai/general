import time

start = time.time()
for i in range(100000000):
    for j in range(20):
        continue;
end = time.time();
print(end - start)