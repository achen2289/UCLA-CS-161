def verifier():
    files = ["./cnfs/f1/sat_f1.cnf", "./cnfs/f2/sat_f2.cnf", "./cnfs/f3/sat_f3.cnf"]
    my_sols = [
                [25, -24, 23, 22, 21, -20, 19, 18, -17, 16, 15, -14, 13, -12, 11, -10, -9, 8, -7, -6, -5, 4, -3, 2, 1],
                [25, 24, -23, 22, 21, 20, -19, -18, 17, -16, -15, 14, -13, -12, 11, 10, 9, -8, -7, -6, -5, -4, 3, 2, 1],
                [22, 21, -20, 19, -18, 17, 16, -15, -14, -13, -12, -11, -10, -9, -8, 7, -6, 5, 4, -3, -2, -1],
    ]

    for file, sol in zip(files, my_sols):
        # var_assignments = {num: (num > 0) for num in sol}
        var_assignments = set(sol)
        cnf = read_cnf_file(file)
        cnf_satisfied = True
        for clause in cnf:
            clause_true = False
            for var in clause:
                if var in var_assignments:
                    clause_true = True
                    break
            if not clause_true:
                cnf_satisfied = False
                break
        print (cnf_satisfied)



def read_cnf_file(file):
    with open(file, "r") as f:
        lines = f.readlines()
    
    cnf = [] # list of clauses, each clause is a list of nums
    for i in range(1, len(lines)):
        line = lines[i]
        line_split = line.split(" ")
        clause = []
        for num in line_split[:-1]:
            clause.append(int(num))
        cnf.append(clause)
    return cnf
        
if __name__ == "__main__":
    verifier()