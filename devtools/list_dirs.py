import os

for d in os.listdir('.'):
    if os.path.isdir(d) and not d.startswith('.'):
        print(d)
