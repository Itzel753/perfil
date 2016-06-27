inFileName = "confout.txt"
outFileName = "confout.xyz"

inFile = open(inFileName, "r")
lines = inFile.readlines()
inFile.close()

outFile = open(outFileName, "w")
natom = lines[3].split()[0]
outFile.write("{}\n".format(natom))
outFile.write("\n")
for line in lines:
    words = line.split()
    if len(words) == 4 and words[3] == "T":
        outFile.write("Ar {} {} 0.0 \n".format(words[0], words[1]))
outFile.close()
