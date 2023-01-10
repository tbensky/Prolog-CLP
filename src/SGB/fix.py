import csv
with open('5letter.txt', 'r') as fd:
    reader = csv.reader(fd)
    for row in reader:

        # first, list the word as characters
        print("word([",end="")
        for i,c in enumerate(row[0]):
            if i < 4:
                print(f"{c},",end="")
            else:
                print(c,end="")
        print("],[",end="")

        # now list as enumerated elements, a=1, b=2, etc. (ascii codes would be OK too)
        for i,c in enumerate(row[0]):
            if i < 4:
                print(f"{ord(c)-ord('a')+1},",end="")
            else:
                print(f"{ord(c)-ord('a')+1}",end="")
        print(f"],'{row[0]}').",end="")
        print()



