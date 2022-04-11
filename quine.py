from pyswip import Prolog
import re

RESULT_VAR = "Result"

quine_type = {'1': ('quine', 'returns the prime implicant only'),
              '2': ('quine_petrick', 'returns the global minimum minimized'),
              '3': ('quine_pruned_petrick', 'returns the first best local minimum')}


def quine_wrapper(algo, n, minterms):
    minterms = re.sub("\s+", "", minterms)
    prolog = Prolog()
    prolog.consult(algo)
    prolog.query("silent")
    query = f"quine({n}, '{minterms}', {RESULT_VAR})"
    print(query)
    results = prolog.query(query)

    for result in results:
        prime_implicants = result[RESULT_VAR]
        break

    BASE_ORD = ord('A')

    for implicant_idx, prime_implicant in enumerate(prime_implicants):
        term = ""
        for index, val in enumerate(prime_implicant):
            if(val == 2):
                continue
            term += chr(index + BASE_ORD)
            if not val:
                term += "'"
        print(term, end=('+' if implicant_idx !=
              len(prime_implicants) - 1 else ''))


if __name__ == "__main__":
    for jenis in quine_type.keys() :
        print(f"{jenis}: {quine_type[jenis]}")
    jenis = input(f"Insert algorithm type: ")
    n = int(input("Enter number of variables: "))
    minterms = input(
        f"Enter minterms ({0} - {2**n -1})(separated by comma ','): ")
    quine_wrapper(quine_type[jenis][0], n, minterms)
