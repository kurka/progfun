#!/usr/bin/python

from unidecode import unidecode
from itertools import combinations


def any_words(remaining, w_dict, root):
    results = []
    for n in range(len(remaining)+1, 3, -1):
        if len(remaining) >= n:
            for w in combinations(remaining, n):
                if "".join(w) in w_dict:
                    rem_remaining = remaining[:]
                    for char in w:
                        rem_remaining = rem_remaining.replace(char, "", 1)
                    for r in any_words(rem_remaining, w_dict, False):
                        ans = ["".join(w)]
                        if r["ans"] != ['']:
                            ans.extend(r["ans"])

                        complete = r["complete"]

                        result = {
                            "ans": ans,
                            "complete": complete
                            }

                        results.append(result)

    if results == []:
        complete2 = False if (len(remaining) > 0) else True
        results = [remaining]

        return [{"ans": results,
                "complete": complete2
                 }]
    else:
        return results


def normalize(w):
    return unidecode(w.lower().strip())


def main():

    init_letters = normalize("felizaniversário")

    with open("/usr/share/dict/pt-br") as f:
        word_dict = set([normalize(w)
                        for w in f.readlines()
                        if len(normalize(w)) > 2])

    results = any_words(init_letters, word_dict, True)
    print(">>>Resultados Incompletos:")
    for r in results:
        if not r["complete"]:
            print(r["ans"][:-1], "Resto:", r["ans"][-1])
    print(">>>Resultados Completos:")
    for r in results:
        if r["complete"]:
            print(r["ans"])

if __name__ == '__main__':
    main()
