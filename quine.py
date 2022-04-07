from pyswip import Prolog
import re

RESULT_VAR = "Result"

def quine_wrapper(n, minterms):
    prolog = Prolog()
    prolog.consult("quine")
    prolog.query("silent")
    query = f"quine({n}, '{minterms}', {RESULT_VAR})"
    print(query)
    results = prolog.query(query)
    for result in results:
        print(result[RESULT_VAR])
        break

if __name__ == "__main__":
    n = int(input("Enter number of variables: "))
    minterms = input("Enter minterms (separated by comma ','): ")
    minterms = re.sub("\s+", "", minterms)
    quine_wrapper(n, minterms)