import csv
with open('5letter.txt', 'r') as fd:
    reader = csv.reader(fd)
    for row in reader:
        print("word([",end="")
        for i,c in enumerate(row[0]):
            if i < 4:
                print(f"{c},",end="")
            else:
                print(c,end="")
        print("])")

